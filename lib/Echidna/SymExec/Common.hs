{-# LANGUAGE GADTs #-}

module Echidna.SymExec.Common where

import Control.Monad.IO.Unlift (MonadUnliftIO, liftIO)
import Control.Monad.State.Strict (execState, runStateT)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.DoubleWord (Word256)
import Data.List (foldl')
import Data.Function ((&))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Optics.Core ((.~), (%), (%~))

import EVM (initialContract, loadContract, resetState, symbolify)
import EVM.ABI (abiKind, AbiKind(Dynamic), Sig(..), decodeBuf, AbiVals(..), selector, encodeAbiValue, AbiValue(..))
import EVM.Effects (TTY, ReadConfig)
import EVM.Expr qualified
import EVM.Fetch qualified as Fetch
import EVM.Format (formatPartialDetailed)
import EVM.Solidity (SolcContract(..), SourceCache(..), Method(..))
import EVM.Solvers (SolverGroup)
import EVM.SymExec (mkCalldata, verifyInputsWithHandler, VeriOpts(..), subModel, defaultSymbolicValues, Postcondition)
import EVM.Types (Addr, Contract(..), VMType(..), EType(..), EvmError(..), Expr(..), Block(..), W256, SMTCex(..), ProofResult(..), Prop(..), forceLit, isQed)
import qualified EVM.Types (VM(..))

import Echidna.Test (isFoundryMode)
import Echidna.Types (fromEVM)
import Echidna.Types.Config (EConfig(..))
import Echidna.Types.Solidity (SolConf(..))
import Echidna.Types.Tx (Tx(..), TxCall(..), TxConf(..), maxGasPerBlock)

panicMsg :: Word256 -> ByteString
panicMsg err = selector "Panic(uint256)" <> encodeAbiValue (AbiUInt 256 err)

-- | Postcondition for symbolic execution verification.
-- In foundry mode, all reverts are failures except vm.assume failures.
-- In assertion mode, only assertion failures (0xfe opcode, Error(string)
-- "assertion failed", or Panic codes) are detected.
checkAssertions :: [Word256] -> Bool -> Postcondition
checkAssertions errs isFoundry _ vmres
  | isFoundry = case vmres of
      -- vm.assume failures should not be treated as test failures
      Failure _ _ AssumeCheatFailed -> PBool True
      -- All other failures are test failures in foundry mode
      Failure {} -> PBool False
      _ -> PBool True
  | otherwise = case vmres of
      -- Solidity assert() opcode (0xfe)
      Failure _ _ (UnrecognizedOpcode 0xfe) -> PBool False
      -- Concrete revert: check for "assertion failed" message or panic code
      Failure _ _ (Revert (ConcreteBuf msg)) ->
        -- NOTE: assertTrue/assertFalse does not have the double colon after "assertion failed"
        let assertFail = selector "Error(string)" `BS.isPrefixOf` msg
              && "assertion failed" `BS.isPrefixOf` BS.drop txtOffset msg
        in PBool $ not (assertFail || msg `elem` fmap panicMsg errs)
      -- Symbolic revert: check symbolically against panic messages
      -- TODO: also check for Error(string) "assertion failed" in partially-symbolic
      -- buffers, similar to hevm's symbolicFail in EVM.UnitTest
      Failure _ _ (Revert b) ->
        foldl' PAnd (PBool True) (fmap (PNeg . PEq b . ConcreteBuf . panicMsg) errs)
      _ -> PBool True
  where
    txtOffset = 4 + 32 + 32 -- selector + offset + length

type PartialsLogs = [T.Text]

data TxOrError = TxFromResult Tx | SMTErrorFromResult String
  deriving (Show)

extractTxs :: [TxOrError] -> [Tx]
extractTxs = mapMaybe (\case
  TxFromResult tx -> Just tx
  _ -> Nothing)

extractErrors :: [TxOrError] -> [String]
extractErrors = mapMaybe (\case
  SMTErrorFromResult err -> Just err
  _ -> Nothing)

suitableForSymExec :: Method -> Bool
suitableForSymExec m = not $ null m.inputs
  && null (filter (\(_, t) -> abiKind t == Dynamic) m.inputs)
  && not (T.isInfixOf "_no_symexec" m.name)


blockMakeSymbolic :: Block -> Block
blockMakeSymbolic b
  = b {
      timestamp = Var "symbolic_block_timestamp"
    , number = Var "symbolic_block_number"
  }

addBlockConstraints :: W256 -> W256 -> Block -> [Prop] -> [Prop]
addBlockConstraints maxTimestampDiff maxNumberDiff block cs =
  cs ++ [
    PGEq (Var "symbolic_block_timestamp") (block.timestamp), PLEq (Sub (Var "symbolic_block_timestamp") (block.timestamp)) $ Lit maxTimestampDiff,
    PGEq (Var "symbolic_block_number") (block.number), PLEq (Sub (Var "symbolic_block_number") (block.number)) $ Lit maxNumberDiff
  ]

senderConstraints :: Set Addr -> [Prop]
senderConstraints as = [foldr (\a b -> POr b (PEq (SymAddr "caller") (LitAddr a))) (PBool False) $ Set.toList as]

modelToTx :: Addr -> Expr EWord -> Expr EWord -> Method -> Set Addr -> Addr -> Expr Buf -> ProofResult SMTCex String -> TxOrError
modelToTx dst oldTimestamp oldNumber method senders fallbackSender calldata result =
  case result of
    Cex cex ->
      let
        cd = defaultSymbolicValues $ subModel cex calldata
        types = snd <$> method.inputs
        argdata = case cd of
          Right cd' -> Right $ EVM.Expr.drop 4 (EVM.Expr.simplify cd')
          Left e -> Left e
        args = case argdata of
          Right argdata' -> case decodeBuf types argdata' of
            (CAbi v, _) -> v
            _ -> []
          Left _ -> []

        src_ = fromMaybe 0 $ Map.lookup (SymAddr "caller") cex.addrs
        src = if Set.member src_ senders then src_ else fallbackSender
        value = fromMaybe 0 $ Map.lookup TxValue cex.txContext

        -- If symbolic values for delay (timestamp, number) are unconstrained, no delay is applied
        newTimestamp = fromMaybe 0 $ Map.lookup (Var "symbolic_block_timestamp") cex.vars
        diffTimestamp = if newTimestamp == 0 then 0 else newTimestamp - forceLit oldTimestamp

        newNumber = fromMaybe 0 $ Map.lookup (Var "symbolic_block_number") cex.vars
        diffNumber = if newNumber == 0 then 0 else newNumber - forceLit oldNumber

      in TxFromResult $ Tx
        { call = SolCall (method.name, args)
        , src = src
        , dst = dst
        , gasprice = 0
        , gas = maxGasPerBlock
        , value = value
        , delay = (diffTimestamp, diffNumber)
        }

    Error err -> SMTErrorFromResult err
    Unknown err -> SMTErrorFromResult err
    r -> error ("Unexpected value in `modelToTx`: " ++ show r)

rpcFetcher :: Functor f =>
  f a -> Maybe W256 -> f (Fetch.BlockNumber, a)

rpcFetcher rpc block = (,) block' <$> rpc
  where block' = maybe Fetch.Latest Fetch.BlockNumber block

removeUnknownResults :: [ProofResult SMTCex String] -> ([ProofResult SMTCex String], [ProofResult SMTCex String])
removeUnknownResults results =
  let
    known = filter (\case Unknown _ -> False ; _ -> True) results
    unknown = filter (\case Unknown _ -> True; _ -> False) results
  in (known, unknown)

getUnknownLogs :: [ProofResult SMTCex String] -> [T.Text]
getUnknownLogs = mapMaybe (\case
  Error err -> Just $ T.pack err
  _ -> Nothing)

exploreMethod :: (MonadUnliftIO m, ReadConfig m, TTY m) =>
  Method -> SolcContract -> SourceCache -> EVM.Types.VM Concrete -> Addr -> EConfig -> VeriOpts -> SolverGroup -> Fetch.RpcInfo -> Fetch.Session -> m ([TxOrError], PartialsLogs)
exploreMethod = exploreMethodWith Nothing

-- | Like 'exploreMethod' but accepts a custom postcondition. When 'Nothing',
-- uses the default assertion-checking postcondition.
exploreMethodWith :: (MonadUnliftIO m, ReadConfig m, TTY m) =>
  Maybe Postcondition -> Method -> SolcContract -> SourceCache -> EVM.Types.VM Concrete -> Addr -> EConfig -> VeriOpts -> SolverGroup -> Fetch.RpcInfo -> Fetch.Session -> m ([TxOrError], PartialsLogs)

exploreMethodWith customPost method _contract _sources vm defaultSender conf veriOpts solvers rpcInfo session = do
  calldataSym@(_, constraints) <- mkCalldata (Just (Sig method.methodSignature (snd <$> method.inputs))) []
  let
    cd = fst calldataSym
    fetcher = Fetch.oracle solvers (Just session) rpcInfo
    dst = conf.solConf.contractAddr
  vmReset <- liftIO $ snd <$> runStateT (fromEVM resetState) vm
  let
    vm' = vmReset & execState (loadContract (LitAddr dst))
                  & #tx % #isCreate .~ False
                  -- Foundry tests cannot accept ether, force callvalue to zero
                  & #state % #callvalue .~ (if isFoundryMode conf.solConf.testMode then Lit 0 else TxValue)
                  & #state % #caller .~ SymAddr "caller"
                  & #state % #calldata .~ cd

    vm'' = symbolify vm'
        & #block %~ blockMakeSymbolic
        & #constraints %~ (addBlockConstraints conf.txConf.maxTimeDelay conf.txConf.maxBlockDelay vm'.block)
        & #constraints %~ (++ constraints ++ senderConstraints conf.solConf.sender)

  -- TODO we might want to switch vm's state.baseState value to to AbstractBase eventually.
  -- Doing so might mess up concolic execution.
  let foundry = isFoundryMode conf.solConf.testMode
  let postcondition = case customPost of
        Just p  -> p
        Nothing -> checkAssertions [0x1] foundry
  (models, partials) <- verifyInputsWithHandler solvers veriOpts fetcher vm'' postcondition Nothing
  let results = filter (\(r, _) -> not (isQed r)) models & map fst
  --liftIO $ mapM_ print partials
  return (map (modelToTx dst vm.block.timestamp vm.block.number method conf.solConf.sender defaultSender cd) results, map (\(p, _) -> formatPartialDetailed Nothing Map.empty p) partials)

-- | Convert a symbolic contract expression (from a Success leaf) to a
-- concrete Contract value suitable for use in a VM's env.contracts map.
fromEContract :: Expr EContract -> Contract
fromEContract (C code storage tStorage balance nonce) =
  initialContract code
    & #storage .~ storage
    & #tStorage .~ tStorage
    & #balance .~ balance
    & #nonce .~ nonce
fromEContract _ = error "fromEContract: unexpected non-C expression"

-- | Two-phase symbolic execution.
--
-- Phase 1: Symbolically execute a state-changing method with a trivially-true
-- postcondition (no solver calls). This collects all reachable Success leaves,
-- each containing symbolic contract states and path constraints.
--
-- Phase 2: For each Success leaf and each target method, reconstruct a VM
-- with the symbolic contract states, execute the target, and check the given
-- postcondition. The solver finds concrete values for phase 1's symbolic
-- variables (calldata, caller, etc.) that cause a violation.
exploreMethodTwoPhase :: (MonadUnliftIO m, ReadConfig m, TTY m) =>
  Postcondition -> (Method -> m ()) -> Method -> [Method] -> SolcContract -> SourceCache -> EVM.Types.VM Concrete -> Addr -> EConfig -> VeriOpts -> SolverGroup -> Fetch.RpcInfo -> Fetch.Session -> m ([TxOrError], PartialsLogs)

exploreMethodTwoPhase phase2Post logTarget method targetMethods _contract _sources vm defaultSender conf veriOpts solvers rpcInfo session = do
  -- Phase 1: Execute state-changing method symbolically
  calldataSym@(_, constraints) <- mkCalldata (Just (Sig method.methodSignature (snd <$> method.inputs))) []
  let
    cd = fst calldataSym
    fetcher = Fetch.oracle solvers (Just session) rpcInfo
    dst = conf.solConf.contractAddr
  vmReset <- liftIO $ snd <$> runStateT (fromEVM resetState) vm
  let
    vm' = vmReset & execState (loadContract (LitAddr dst))
                  & #tx % #isCreate .~ False
                  & #state % #callvalue .~ (if isFoundryMode conf.solConf.testMode then Lit 0 else TxValue)
                  & #state % #caller .~ SymAddr "caller"
                  & #state % #calldata .~ cd

    vm'' = symbolify vm'
        & #block %~ blockMakeSymbolic
        & #constraints %~ (addBlockConstraints conf.txConf.maxTimeDelay conf.txConf.maxBlockDelay vm'.block)
        & #constraints %~ (++ constraints ++ senderConstraints conf.solConf.sender)

  -- Phase 1: PBool True postcondition → no solver calls, all Success paths return Qed
  (phase1Models, phase1Partials) <- verifyInputsWithHandler solvers veriOpts fetcher vm'' (\_ _ -> PBool True) Nothing

  -- Extract Success leaves with their path constraints and symbolic contract states
  let successLeaves = [(props, contracts) | (_, Success props _ _ contracts) <- phase1Models]

  -- Phase 2: For each Success leaf, check each target method
  allResults <- concat <$> mapM (checkLeaf fetcher dst vm cd) successLeaves

  let allPartialLogs = map (\(p, _) -> formatPartialDetailed Nothing Map.empty p) phase1Partials
  return (allResults, allPartialLogs)

  where

  -- | Phase 2 for a single Success leaf: check all target methods
  checkLeaf fetcher dst vm0 cd (leafConstraints, leafContracts) = do
    let convertedContracts = Map.map fromEContract leafContracts
    concat <$> mapM (checkTarget fetcher dst vm0 cd convertedContracts leafConstraints) targetMethods

  -- | Phase 2 for a single (leaf, target) pair
  checkTarget fetcher dst vm0 cd convertedContracts leafConstraints targetMethod = do
    logTarget targetMethod
    vmReset2 <- liftIO $ snd <$> runStateT (fromEVM resetState) vm0
    let
      targetCd = ConcreteBuf (selector targetMethod.methodSignature)
      -- Replace contracts with phase 1's symbolic state, keeping originals as fallback
      vm2 = vmReset2 & #env % #contracts %~ Map.union convertedContracts
                     & execState (loadContract (LitAddr dst))
                     & #tx % #isCreate .~ False
                     & #state % #callvalue .~ Lit 0
                     & #state % #caller .~ SymAddr "caller"
                     & #state % #calldata .~ targetCd
      -- Carry over phase 1's path constraints so the solver must satisfy both phases
      vm2' = symbolify vm2
          & #constraints .~ leafConstraints

    (phase2Models, _) <- verifyInputsWithHandler solvers veriOpts fetcher vm2' phase2Post Nothing
    let results2 = filter (\(r, _) -> not (isQed r)) phase2Models & map fst
    return $ map (modelToTx dst vm0.block.timestamp vm0.block.number method conf.solConf.sender defaultSender cd) results2
