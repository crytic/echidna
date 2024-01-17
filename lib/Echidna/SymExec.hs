{-# LANGUAGE DataKinds #-}

module Echidna.SymExec where

import Control.Monad (forM)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BS
import Data.Functor ((<&>))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text qualified as T
import Optics.State.Operators
import Optics.Core
import EVM.ABI
import EVM.Expr (simplify)
import EVM.Fetch qualified as Fetch
import EVM.SMT
import EVM.Solidity (SolcContract(..), Method(..))
import EVM.Solvers (withSolvers, Solver(Z3), CheckSatResult(Sat))
import EVM.SymExec (interpret, runExpr, abstractVM, mkCalldata, produceModels, LoopHeuristic (Naive))
import EVM.Types
import Control.Monad.ST (stToIO, RealWorld)
import Echidna.Types.Tx
import Data.Vector.Unboxed qualified as VUnboxed
import Data.Vector.Unboxed.Mutable qualified as VUnboxed.Mutable

exploreContract :: Addr -> SolcContract -> VM RealWorld -> IO [Tx]
exploreContract dst contract vm = do
  let methods = Map.elems contract.abiMap
      timeout = Just 30 -- seconds

  res <- withSolvers Z3 2 timeout $ \solvers -> do
    forM methods $ \method -> do
      let
        calldata@(cd, constraints) = mkCalldata (Just (Sig method.methodSignature (snd <$> method.inputs))) []
        vmSym' = abstractVM calldata contract.runtimeCode Nothing False
        maxIter = Just 10
        askSmtIters = 5
        rpcInfo = Nothing
      memory <- stToIO (ConcreteMemory <$> VUnboxed.Mutable.new 0)
      vmSym <- stToIO vmSym'
      let vm' = vm & #state .~ vmSym.state
                   & #frames .~ []
                   & #state % #calldata .~ cd
                   & #constraints .~ constraints
                   & #state % #contract .~ SymAddr "entrypoint"
                   & #state % #codeContract .~ SymAddr "entrypoint"
                   & #state % #callvalue .~ TxValue
                   & #result .~ Nothing
                   & #config % #baseState .~ AbstractBase
                   & #state % #caller .~ SymAddr "caller"
                   & #env .~ vmSym.env
      exprInter <- interpret (Fetch.oracle solvers rpcInfo) maxIter askSmtIters Naive vm' runExpr
      models <- produceModels solvers (simplify exprInter)
      pure $ mapMaybe (modelToTx dst method) models

  pure $ mconcat res

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
