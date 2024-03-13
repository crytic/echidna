{-# LANGUAGE DataKinds #-}

module Echidna.SymExec where

import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM)
import Data.ByteString.Lazy qualified as BS
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text qualified as T
import Optics.Core ((.~), (%))
import Echidna.Types.Solidity (SolConf(..))
import EVM.ABI (Sig(..), decodeAbiValue)
import EVM.Expr (simplify)
import EVM.Fetch qualified as Fetch
import EVM.SMT (SMTCex(..))
import EVM (loadContract)
import EVM.Solidity (SolcContract(..), Method(..))
import EVM.Solvers (withSolvers, Solver(Z3), CheckSatResult(Sat))
import EVM.SymExec (interpret, runExpr, abstractVM, mkCalldata, produceModels, LoopHeuristic (Naive))
import EVM.Types (Addr, VM(..), Env(..), Expr(..), EType(..), BaseState(..), word256Bytes)
import Control.Monad.ST (stToIO, RealWorld)
import Control.Monad.State.Strict (execState)
import Echidna.Types.Tx (Tx(..), TxCall(..), maxGasPerBlock)


-- test with applying multiple state modifying transactions before
-- running exploreContract
-- use execTxWithCov
exploreContract :: SolConf -> SolcContract -> VM RealWorld -> IO (ThreadId, MVar [Tx])
exploreContract conf contract vm = do
  let methods = Map.elems contract.abiMap -- TODO randomize order? in case we hit timeout
      timeout = Just (fromIntegral conf.symExecTimeout) -- seconds

  threadIdChan <- newEmptyMVar
  doneChan <- newEmptyMVar
  resultChan <- newEmptyMVar

  withSolvers Z3 (fromIntegral conf.symExecNWorkers) timeout $ \solvers -> do
    threadId <- forkIO $ do
      res <- forM methods $ \method -> do
        let
          dst = conf.contractAddr
          calldata@(cd, constraints) = mkCalldata (Just (Sig method.methodSignature (snd <$> method.inputs))) []
          vmSym' = abstractVM calldata contract.runtimeCode Nothing False
          maxIter = Just 10
          askSmtIters = 5
          rpcInfo = Nothing
        vmSym <- stToIO vmSym'
        let vm' = vm & execState (loadContract (LitAddr dst))
                     & #frames .~ []
                     & #constraints .~ constraints
                     & #state % #callvalue .~ TxValue
                     & #state % #caller .~ SymAddr "caller"
                     & #state % #calldata .~ cd
                     & #state % #pc .~ 0
                     & #state % #stack .~ []
                     & #result .~ Nothing
                     & #config % #baseState .~ AbstractBase
                     & #env % #contracts .~ (Map.union vmSym.env.contracts vm.env.contracts)
        exprInter <- interpret (Fetch.oracle solvers rpcInfo) maxIter askSmtIters Naive vm' runExpr
        models <- produceModels solvers (simplify exprInter)
        pure $ mapMaybe (modelToTx dst method) models
      putMVar resultChan (mconcat res)
      putMVar doneChan ()
    putMVar threadIdChan threadId
    takeMVar doneChan

  threadId <- takeMVar threadIdChan
  pure (threadId, resultChan)

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
