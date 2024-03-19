{-# LANGUAGE DataKinds #-}

module Echidna.SymExec (createSymTx) where

import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Data.ByteString.Lazy qualified as BS
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Optics.Core ((.~), (%))
import Echidna.Solidity (chooseContract)
import Echidna.Types.Campaign (CampaignConf(..))
import Echidna.Types.Config (EConfig(..))
import Echidna.Types.Solidity (SolConf(..))
import EVM.ABI (Sig(..), decodeAbiValue)
import EVM.Expr (simplify)
import EVM.Fetch qualified as Fetch
import EVM.SMT (SMTCex(..))
import EVM (loadContract)
import EVM.Effects (defaultEnv)
import EVM.Solidity (SolcContract(..), Method(..))
import EVM.Solvers (withSolvers, Solver(Z3), CheckSatResult(Sat))
import EVM.SymExec (interpret, runExpr, abstractVM, mkCalldata, produceModels, LoopHeuristic (Naive))
import EVM.Types (Addr, VM(..), Frame(..), FrameState(..), VMType(..), Env(..), Expr(..), EType(..), BaseState(..), word256Bytes)
import Control.Monad.ST (stToIO, RealWorld)
import Control.Monad.State.Strict (execState)
import Echidna.Types.Tx (Tx(..), TxCall(..), maxGasPerBlock)


-- | Uses symbolic execution to find transactions which would increase coverage.
-- Spawns a new thread; returns its thread ID as the first return value.
-- The second return value is an MVar which is populated with transactions
--   once the symbolic execution is finished.
createSymTx :: EConfig -> Maybe Text -> [SolcContract] -> VM Concrete RealWorld -> IO (ThreadId, MVar [Tx])
createSymTx cfg name cs vm = do
    mainContract <- chooseContract cs name
    exploreContract cfg mainContract vm

exploreContract :: EConfig -> SolcContract -> VM Concrete RealWorld -> IO (ThreadId, MVar [Tx])
exploreContract conf contract vm = do
  let methods = Map.elems contract.abiMap
      timeout = Just (fromIntegral conf.campaignConf.symExecTimeout)

  threadIdChan <- newEmptyMVar
  doneChan <- newEmptyMVar
  resultChan <- newEmptyMVar

  flip runReaderT defaultEnv $ withSolvers Z3 (fromIntegral conf.campaignConf.symExecNSolvers) timeout $ \solvers -> do
    threadId <- liftIO $ forkIO $ flip runReaderT defaultEnv $ do
      res <- forM methods $ \method -> do
        let
          dst = conf.solConf.contractAddr
          calldata@(cd, constraints) = mkCalldata (Just (Sig method.methodSignature (snd <$> method.inputs))) []
          vmSym = abstractVM calldata contract.runtimeCode Nothing False
          maxIter = Just 10
          askSmtIters = 5
          rpcInfo = Nothing
        vmSym' <- liftIO $ stToIO vmSym
        let vm' = vm & execState (loadContract (LitAddr dst))
                     & vmMakeSymbolic
                     & #frames .~ []
                     & #constraints .~ constraints
                     & #state % #callvalue .~ TxValue
                     & #state % #caller .~ SymAddr "caller"
                     & #state % #calldata .~ cd
                     & #state % #pc .~ 0
                     & #state % #stack .~ []
                     & #config % #baseState .~ AbstractBase
                     & #env % #contracts .~ (Map.union vmSym'.env.contracts vm.env.contracts)
        exprInter <- interpret (Fetch.oracle solvers rpcInfo) maxIter askSmtIters Naive vm' runExpr
        models <- produceModels solvers (simplify exprInter)
        pure $ mapMaybe (modelToTx dst method) models
      liftIO $ putMVar resultChan (mconcat res)
      liftIO $ putMVar doneChan ()
    liftIO $ putMVar threadIdChan threadId
    liftIO $ takeMVar doneChan

  threadId <- takeMVar threadIdChan
  pure (threadId, resultChan)

-- | Sets result to Nothing, and sets gas to ()
vmMakeSymbolic :: VM Concrete s -> VM Symbolic s
vmMakeSymbolic vm
  = VM
  { result         = Nothing
  , state          = frameStateMakeSymbolic vm.state
  , frames         = map frameMakeSymbolic vm.frames
  , env            = vm.env
  , block          = vm.block
  , tx             = vm.tx
  , logs           = vm.logs
  , traces         = vm.traces
  , cache          = vm.cache
  , burned         = ()
  , iterations     = vm.iterations
  , constraints    = vm.constraints
  , config         = vm.config
  , forks          = vm.forks
  , currentFork    = vm.currentFork
  }

frameStateMakeSymbolic :: FrameState Concrete s -> FrameState Symbolic s
frameStateMakeSymbolic fs
  = FrameState
  { contract     = fs.contract
  , codeContract = fs.codeContract
  , code         = fs.code
  , pc           = fs.pc
  , stack        = fs.stack
  , memory       = fs.memory
  , memorySize   = fs.memorySize
  , calldata     = fs.calldata
  , callvalue    = fs.callvalue
  , caller       = fs.caller
  , gas          = ()
  , returndata   = fs.returndata
  , static       = fs.static
  }

frameMakeSymbolic :: Frame Concrete s -> Frame Symbolic s
frameMakeSymbolic fr = Frame { context = fr.context, state = frameStateMakeSymbolic fr.state }

modelToTx :: Addr -> Method -> (Expr 'End, CheckSatResult) -> Maybe Tx
modelToTx dst method (_end, result) =
  case result of
    Sat cex ->
      let
        args = (zip [1..] method.inputs) <&> \(i::Int, (_argName, argType)) ->
          case Map.lookup (Var ("arg" <> T.pack (show i))) cex.vars of
            Just w ->
              decodeAbiValue argType (BS.fromStrict (word256Bytes w))
            Nothing -> -- put a placeholder
              decodeAbiValue argType (BS.repeat 0)

        value = fromMaybe 0 $ Map.lookup TxValue cex.txContext

      in Just Tx
        { call = SolCall (method.name, args)
        , src = 0
        , dst = dst
        , gasprice = 0
        , gas = maxGasPerBlock
        , value = value
        , delay = (0, 0)
        }

    _ -> Nothing
