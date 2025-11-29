module Echidna.SymExec.Common where

import Control.Monad.IO.Unlift (MonadUnliftIO, liftIO)
import Data.Function ((&))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import GHC.IORef (IORef, readIORef)
import Optics.Core ((.~), (%), (%~))
import EVM.ABI (abiKind, AbiKind(Dynamic), Sig(..), decodeBuf, AbiVals(..))
import EVM.Fetch qualified as Fetch
import EVM (loadContract, resetState, symbolify)
import EVM.Effects (TTY, ReadConfig)
import EVM.Solidity (SolcContract(..), SourceCache(..), Method(..), WarningData(..))
import EVM.Solvers (SolverGroup)
import EVM.SymExec (mkCalldata, verifyInputs, VeriOpts(..), checkAssertions, subModel, defaultSymbolicValues)
import EVM.Expr qualified
import EVM.Types (Addr, VMType(..), EType(..), Expr(..), Block(..), W256, SMTCex(..), ProofResult(..), Prop(..), Query(..), forceLit)
import qualified EVM.Types (VM(..))
import EVM.Format (formatPartialDetailed)
import Control.Monad.ST (RealWorld)
import Control.Monad.State.Strict (execState, runStateT)

import Echidna.Types (fromEVM)
import Echidna.Types.Config (EConfig(..))
import Echidna.Types.Solidity (SolConf(..))
import Echidna.Types.Tx (Tx(..), TxCall(..), TxConf(..), maxGasPerBlock)
import Echidna.Types.Cache (ContractCache, SlotCache)

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
            CAbi v -> v
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


cachedOracle :: IORef ContractCache -> IORef SlotCache -> SolverGroup -> Fetch.RpcInfo -> Fetch.Fetcher t m s
cachedOracle contractCacheRef slotCacheRef solvers info q = do
  case q of
    PleaseFetchContract addr _ continue -> do
      cache <- liftIO $ readIORef contractCacheRef
      case Map.lookup addr cache of
          Just (Just contract) -> pure $ continue contract
          _                    -> oracle q
    PleaseFetchSlot addr slot continue -> do
      cache <- liftIO $ readIORef slotCacheRef
      case Map.lookup addr cache >>= Map.lookup slot of
        Just (Just value) -> pure $ continue value
        _                 -> oracle q
    _ -> oracle q

  where oracle = Fetch.oracle solvers info

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
  Method -> SolcContract -> SourceCache -> EVM.Types.VM Concrete RealWorld -> Addr -> EConfig -> VeriOpts -> SolverGroup -> Fetch.RpcInfo -> IORef ContractCache -> IORef SlotCache -> m ([TxOrError], PartialsLogs)
  
exploreMethod method contract sources vm defaultSender conf veriOpts solvers rpcInfo contractCacheRef slotCacheRef = do
  calldataSym@(_, constraints) <- mkCalldata (Just (Sig method.methodSignature (snd <$> method.inputs))) []
  let
    cd = fst calldataSym
  let
    fetcher = cachedOracle contractCacheRef slotCacheRef solvers rpcInfo
    dst = conf.solConf.contractAddr
  vmReset <- liftIO $ snd <$> runStateT (fromEVM resetState) vm
  let
    vm' = vmReset & execState (loadContract (LitAddr dst))
                  & #tx % #isCreate .~ False
                  & #state % #callvalue .~ TxValue
                  & #state % #caller .~ SymAddr "caller"
                  & #state % #calldata .~ cd

    vm'' = symbolify vm'
        & #block %~ blockMakeSymbolic
        & #constraints %~ (addBlockConstraints conf.txConf.maxTimeDelay conf.txConf.maxBlockDelay vm'.block)
        & #constraints %~ (++ constraints ++ senderConstraints conf.solConf.sender)

  -- TODO we might want to switch vm's state.baseState value to to AbstractBase eventually.
  -- Doing so might mess up concolic execution.
  (_, models, partials) <- verifyInputs solvers veriOpts fetcher vm'' (Just $ checkAssertions [0x1])
  let results = map fst models
  let warnData = Just $ WarningData contract sources vm' 
  --liftIO $ mapM_ TIO.putStrLn partials
  return (map (modelToTx dst vm.block.timestamp vm.block.number method conf.solConf.sender defaultSender cd) results, map (formatPartialDetailed warnData . fst) partials)
