{-# LANGUAGE DataKinds #-}

module Echidna.SymExec where

import Control.Monad (forM)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BS
import Data.Functor ((<&>))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text qualified as T

import EVM.ABI
import EVM.Expr (simplify)
import EVM.Fetch qualified as Fetch
import EVM.SMT
import EVM.Solidity (SolcContract(..), Method(..))
import EVM.Solvers (withSolvers, Solver(Z3), CheckSatResult(Sat))
import EVM.SymExec (interpret, runExpr, abstractVM, mkCalldata, produceModels, Sig(Sig), LoopHeuristic (Naive))
import EVM.Types

import Echidna.Types.Tx

exploreContract :: Addr -> SolcContract -> IO [Tx]
exploreContract dst contract = do
  let methods = Map.elems contract.abiMap
      timeout = Just 30 -- seconds

  res <- withSolvers Z3 2 timeout $ \solvers -> do
    forM methods $ \method -> do
      let
        calldata = mkCalldata (Just (Sig method.methodSignature (snd <$> method.inputs))) []
        vmSym = abstractVM calldata contract.runtimeCode Nothing AbstractStore
        maxIter = Just 10
        askSmtIters = 5
        rpcInfo = Nothing

      exprInter <- interpret (Fetch.oracle solvers rpcInfo) maxIter askSmtIters Naive vmSym runExpr
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

        value = fromMaybe 0 $ Map.lookup (CallValue 0) cex.txContext

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
