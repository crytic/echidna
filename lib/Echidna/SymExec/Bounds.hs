{-# LANGUAGE GADTs #-}

module Echidna.SymExec.Bounds (findApproximateBounds) where

import Control.Monad.IO.Unlift (MonadUnliftIO, liftIO)
import Data.Function ((&))
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.List (nub)
import GHC.IORef (IORef)
import Optics.Core ((.~), (%), (%~))
import EVM.Fetch qualified as Fetch
import EVM (loadContract, resetState)
import EVM.Effects (TTY, ReadConfig)
import EVM.Solidity (SolcContract(..), Method(..))
import EVM.Solvers (SolverGroup)
import EVM.SymExec (abstractVM, mkCalldata, verifyInputs, VeriOpts(..), Postcondition)
import EVM.Types (Addr, VMType(..), EType(..), Expr(..), W256, ProofResult(..), Prop(..))
import qualified EVM.Types (VM(..), Env(..))
import Control.Monad.ST (stToIO, RealWorld)
import Control.Monad.State.Strict (execState, runStateT)

import Echidna.Types.Config (EConfig(..))
import Echidna.Types.Solidity (SolConf(..))
import Echidna.Types.Tx (TxConf(..))
import Echidna.Types.Cache (ContractCache, SlotCache)
import Echidna.Types (fromEVM)
import Echidna.SymExec.Helpers

import Data.Map qualified as Map
import EVM.Types (Frame(..), FrameState(..), Block(..))
import qualified EVM.Types as EVM
import EVM.ABI (AbiValue(..), Sig(..))

data ValueRange = VRLEq (Set T.Text) W256
                | VRGEq (Set T.Text) W256
                | VREq (Set T.Text) W256
                deriving (Eq, Ord)

instance Show ValueRange where
  show (VRLEq vars lit) = T.unpack $ T.concat [T.intercalate "," (Set.toList vars), " <= ", T.pack (show lit)]
  show (VRGEq vars lit) = T.unpack $ T.concat [T.intercalate "," (Set.toList vars), " >= ", T.pack (show lit)]
  show (VREq vars lit) = T.unpack $ T.concat [T.intercalate "," (Set.toList vars), " == ", T.pack (show lit)]

showRange :: Expr a -> Maybe T.Text
showRange (Failure asserts _ _) = let result = T.unlines $ map (T.pack . show) $ nub $ mapMaybe computeRange asserts in if T.null result then Nothing else Just result
showRange (Success asserts _ _ _) = let result = T.unlines $ map (T.pack . show) $ nub $ mapMaybe computeRange asserts in if T.null result then Nothing else Just result
showRange (Partial asserts _ _) = let result = T.unlines $ map (T.pack . show) $ nub $ mapMaybe computeRange asserts in if T.null result then Nothing else Just result
showRange e = error $ "Unexpected expression in `showRange`: " ++ show e

computeRange :: Prop -> Maybe ValueRange
computeRange (PLEq e1 e2) = case (exprDependsOnArg e1, exprLit e2, exprLit e1, exprDependsOnArg e2) of
                              (vars, Just lit, _, _) | not $ Set.null vars -> Just $ VRLEq vars lit
                              (_, _, Just lit, vars) | not $ Set.null vars -> Just $ VRGEq vars lit
                              _ -> Nothing
computeRange (PGEq e1 e2) = case (exprDependsOnArg e1, exprLit e2, exprLit e1, exprDependsOnArg e2) of
                              (vars, Just lit, _, _) | not $ Set.null vars -> Just $ VRGEq vars lit
                              (_, _, Just lit, vars) | not $ Set.null vars -> Just $ VRLEq vars lit
                              _ -> Nothing
computeRange (PLT e1 e2)  = case (exprDependsOnArg e1, exprLit e2, exprLit e1, exprDependsOnArg e2) of
                              (vars, Just lit, _, _) | not $ Set.null vars -> Just $ VRLEq vars lit
                              (_, _, Just lit, vars) | not $ Set.null vars -> Just $ VRGEq vars lit
                              _ -> Nothing
computeRange (PGT e1 e2)  = case (exprDependsOnArg e1, exprLit e2, exprLit e1, exprDependsOnArg e2) of
                              (vars, Just lit, _, _) | not $ Set.null vars -> Just $ VRGEq vars lit
                              (_, _, Just lit, vars) | not $ Set.null vars -> Just $ VRLEq vars lit
                              _ -> Nothing
computeRange (PEq e1 e2) = case (exprDependsOnArg e1, exprLit e2, exprLit e1, exprDependsOnArg e2) of
                              (vars, Just lit, _, _) | not $ Set.null vars -> Just $ VREq vars lit
                              (_, _, Just lit, vars) | not $ Set.null vars -> Just $ VREq vars lit
                              _ -> Nothing
computeRange _ = Nothing

exprLit :: Expr a -> Maybe W256
exprLit (Lit l) = Just l
exprLit _ = Nothing

exprDependsOnArg :: Expr a -> Set T.Text
exprDependsOnArg (Failure asserts _ _) = Set.unions $ map propDependsOnArg asserts
exprDependsOnArg (Partial asserts _ _) = Set.unions $ map propDependsOnArg asserts
exprDependsOnArg (Var v) = if T.isPrefixOf "arg" v then Set.singleton v else Set.empty
exprDependsOnArg (Lit _) = Set.empty
exprDependsOnArg (SEx _ e) = exprDependsOnArg e
exprDependsOnArg (SLT e1 e2) = Set.union (exprDependsOnArg e1) (exprDependsOnArg e2)
exprDependsOnArg (Add e1 e2) = Set.union (exprDependsOnArg e1) (exprDependsOnArg e2)
exprDependsOnArg _ = Set.empty

propDependsOnArg :: Prop -> Set T.Text
propDependsOnArg (PLEq e1 e2) = Set.union (exprDependsOnArg e1) (exprDependsOnArg e2)
propDependsOnArg (PGEq e1 e2) = Set.union (exprDependsOnArg e1) (exprDependsOnArg e2)
propDependsOnArg (PLT e1 e2) = Set.union (exprDependsOnArg e1) (exprDependsOnArg e2)
propDependsOnArg (PGT e1 e2) = Set.union (exprDependsOnArg e1) (exprDependsOnArg e2)
propDependsOnArg _ = Set.empty

findApproximateBounds :: (MonadUnliftIO m, ReadConfig m, TTY m) =>
  Method -> SolcContract -> EVM.Types.VM Concrete RealWorld -> Addr -> EConfig -> VeriOpts -> SolverGroup -> Fetch.RpcInfo -> IORef ContractCache -> IORef SlotCache -> m [T.Text]
findApproximateBounds method contract vm defaultSender conf veriOpts solvers rpcInfo contractCacheRef slotCacheRef = do
  calldataSym@(cd, constraints) <- mkCalldata (Just (Sig method.methodSignature (snd <$> method.inputs))) []
  let
    fetcher = cachedOracle contractCacheRef slotCacheRef solvers rpcInfo
    dst = conf.solConf.contractAddr
    vmSym = abstractVM calldataSym contract.runtimeCode Nothing False
  vmSym' <- liftIO $ stToIO vmSym
  vmReset <- liftIO $ snd <$> runStateT (fromEVM resetState) vm
  let vm' = vmReset & execState (loadContract (LitAddr dst))
                    & vmMakeSymbolic conf.txConf.maxTimeDelay conf.txConf.maxBlockDelay
                    & #constraints %~ (++ constraints)
                    & #state % #callvalue .~ Lit 0
                    & #state % #caller .~ LitAddr defaultSender
                    & #state % #calldata .~ cd
                    & #env % #contracts .~ (Map.union vmSym'.env.contracts vm.env.contracts)
  (_, models, partials) <- verifyInputs solvers veriOpts fetcher vm' (Just nonReverts)
  let
    boundsTotal = ["Constraints inferred:"] ++ (nub $ mapMaybe (showRange . snd) models)
    boundsPartial = ["Constraints inferred:"] ++ (nub $ mapMaybe (showRange . snd) partials)
  return (boundsTotal ++ boundsPartial)
