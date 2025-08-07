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

import Data.Map.Strict qualified as Map
import EVM.Types (Frame(..), FrameState(..), Block(..))
import qualified EVM.Types as EVM
import EVM.ABI (AbiValue(..), Sig(..))
import Numeric.Natural (Natural)

data ValueRange = VRLEq (Set T.Text) W256
                | VRGEq (Set T.Text) W256
                | VREq (Set T.Text) W256
                deriving (Eq, Ord)

instance Show ValueRange where
  show (VRLEq vars lit) = T.unpack $ T.concat [T.intercalate "," (Set.toList vars), " <= ", T.pack (show lit)]
  show (VRGEq vars lit) = T.unpack $ T.concat [T.intercalate "," (Set.toList vars), " >= ", T.pack (show lit)]
  show (VREq vars lit) = T.unpack $ T.concat [T.intercalate "," (Set.toList vars), " == ", T.pack (show lit)]

data ResolvedInterval = RInterval { lower :: Maybe W256, upper :: Maybe W256 }
  deriving (Eq, Ord)

instance Show ResolvedInterval where
  show (RInterval (Just l) (Just u)) | l == u = " == " ++ show l
  show (RInterval (Just l) (Just u)) = " in [" ++ show l ++ ", " ++ show u ++ "]"
  show (RInterval (Just l) Nothing)  = " >= " ++ show l
  show (RInterval Nothing (Just u))  = " <= " ++ show u
  show (RInterval Nothing Nothing)   = ""

resolveRanges :: [ValueRange] -> Map.Map (Set T.Text) ResolvedInterval
resolveRanges ranges =
  let
    grouped = Map.fromListWith (++) $ map (\case
      VRLEq vars lit -> (vars, [VRLEq vars lit])
      VRGEq vars lit -> (vars, [VRGEq vars lit])
      VREq vars lit  -> (vars, [VREq vars lit])) ranges

    processGroup :: [ValueRange] -> ResolvedInterval
    processGroup rs =
      let
        acc (RInterval l u) (VRLEq _ lit) = RInterval l (Just $ maybe lit (min lit) u)
        acc (RInterval l u) (VRGEq _ lit) = RInterval (Just $ maybe lit (max lit) l) u
        acc (RInterval _ _) (VREq _ lit)  = RInterval (Just lit) (Just lit)
      in foldl acc (RInterval Nothing Nothing) rs

  in Map.map processGroup grouped

getProps :: Expr a -> [Prop]
getProps (Failure asserts _ _) = asserts
getProps (Success asserts _ _ _) = asserts
getProps (Partial asserts _ _) = asserts
getProps _ = []

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
    modelProps = concatMap (getProps . snd) models
    partialExprProps = concatMap (getProps . snd) partials
    --partialProps = map (fst . fst) partials
    allProps = modelProps ++ partialExprProps -- ++ partialProps
    allRanges = nub $ mapMaybe computeRange allProps
    resolved = resolveRanges allRanges
    allRangesText = map (T.pack . show) allRanges
    showResolved (vars, interval) = T.concat [T.intercalate "," (Set.toList vars), T.pack (show interval)]
    bounds = if Map.null resolved
             then allRangesText
             else map showResolved (Map.toList resolved)
  return $ if null bounds then [] else (T.pack "Constraints inferred:" : allRangesText) ++ (T.pack "Constraints resolved:" : bounds)
