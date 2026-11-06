{-# LANGUAGE GADTs #-}

module Echidna.SymExec.Bounds
  ( discoverMethodBounds
  , inferArgumentBounds
  , suitableForConstraintDiscovery
  ) where

import Control.Concurrent.STM (newTVarIO)
import Control.Monad.IO.Unlift (liftIO)
import Control.Monad.State.Strict (execStateT)
import Data.Function ((&))
import Data.List (foldl')
import Data.Map qualified as Map
import Data.Text qualified as T
import Optics.Core ((.~), (%), (%~))
import Text.Read (readMaybe)

import EVM (symbolify)
import EVM.ABI (AbiKind(..), AbiType(..), Sig(..), abiKind)
import EVM.Effects (App)
import EVM.Expr qualified as Expr
import EVM.Fetch qualified as Fetch
import EVM.Solidity (Method(..), Mutability(..))
import EVM.Solvers (SolverGroup, withRunAbort)
import EVM.SymExec (IterConfig, interpretVMAbortable, mkCalldata, runExpr)
import EVM.Types (EType(..), Expr(..), Prop(..), VMType(..))
import EVM.Types qualified as EVM

import Echidna.Types.Constraint (ArgumentBounds, NumericBounds(..), unsignedRange)
import Echidna.Types.Tx (Tx)
import Echidna.Transaction (setupTx)

-- | Only run the lightweight pass on state-changing methods whose calldata is
-- cheap to symbolize and which have at least one unsigned integer to improve.
suitableForConstraintDiscovery :: Method -> Bool
suitableForConstraintDiscovery method =
  method.mutability `notElem` [Pure, View]
  && any (isUnsigned . snd) method.inputs
  && all (supported . snd) method.inputs
  && not ("_no_symexec" `T.isInfixOf` method.name)
  where
    isUnsigned (AbiUIntType _) = True
    isUnsigned _ = False

    supported t = abiKind t == Static && case t of
      AbiUIntType _ -> True
      AbiIntType _ -> True
      AbiAddressType -> True
      AbiBoolType -> True
      AbiBytesType _ -> True
      _ -> False

-- | Symbolically execute one method and approximate intervals that reach a
-- successful return. No model is requested: branch exploration is enough to
-- recover the path constraints and keeps this pass substantially cheaper than
-- normal symbolic exploration.
discoverMethodBounds :: App m =>
  Method
  -> EVM.VM Concrete
  -> Tx -- ^ Target transaction, including its concrete envelope
  -> IterConfig
  -> SolverGroup
  -> Fetch.RpcInfo
  -> Fetch.Session
  -> m ArgumentBounds
discoverMethodBounds method vm targetTx iterConfig solvers rpcInfo session = do
  ((calldata, calldataConstraints), _caveats) <-
    mkCalldata (Just (Sig method.methodSignature (snd <$> method.inputs))) []
  symbolicVm <- prepareBoundsCall targetTx vm calldata calldataConstraints
  abort <- liftIO $ newTVarIO False
  let fetcher = Fetch.oracle solvers (Just session) rpcInfo
  leaves <- withRunAbort solvers abort $
    interpretVMAbortable abort fetcher iterConfig symbolicVm runExpr $ \leaf _ _ -> pure leaf
  let nonRevertingPaths =
        [ Expr.simplifyProps props
        | leaf <- leaves
        , props <- case leaf of
            Success pathProps _ _ _ -> [pathProps]
            -- A bounded run commonly stops after it has passed the guards we
            -- care about. Retain that prefix, as the original prototype did.
            Partial pathProps _ _ -> [pathProps]
            _ -> []
        ]
  pure $ usefulBounds method $ inferArgumentBounds nonRevertingPaths

prepareBoundsCall :: App m =>
  Tx
  -> EVM.VM Concrete
  -> Expr Buf
  -> [Prop]
  -> m (EVM.VM Symbolic)
prepareBoundsCall targetTx vm calldata constraints = do
  -- Use the same setup as concrete fuzzing. Besides preserving storage in
  -- every contract, this applies the target transaction's delay, gas fields,
  -- sender/value context, and balance transfer before calldata is symbolized.
  concreteCall <- liftIO $ execStateT (setupTx targetTx) vm
  pure $ symbolify concreteCall
    & #state % #calldata .~ calldata
    & #constraints %~ (++ constraints)

-- | Intersect constraints found on one path, then widen the intervals across
-- all successful paths. A bound missing from any successful path is discarded:
-- it came from an ordinary branch rather than a condition required to avoid a
-- revert.
inferArgumentBounds :: [[Prop]] -> ArgumentBounds
inferArgumentBounds [] = Map.empty
inferArgumentBounds paths =
  foldl1 (Map.intersectionWith widenBounds) (pathBounds <$> paths)

pathBounds :: [Prop] -> ArgumentBounds
pathBounds = foldl' add Map.empty . concatMap flatten
  where
    add bounds (argument, range) = Map.insertWith intersectBounds argument range bounds

    flatten (PAnd a b) = flatten a <> flatten b
    flatten prop = maybe [] pure $ boundFromProp prop

boundFromProp :: Prop -> Maybe (Int, NumericBounds)
boundFromProp = \case
  PLT left right -> lessThan left right
  PGT left right -> lessThan right left
  PLEq left right -> lessThanOrEqual left right
  PGEq left right -> lessThanOrEqual right left
  PEq left right -> do
    leftWord <- Expr.asWord left
    rightWord <- Expr.asWord right
    equalTo leftWord rightWord
  PNeg (PLT left right) -> lessThanOrEqual right left
  PNeg (PGT left right) -> lessThanOrEqual left right
  PNeg (PLEq left right) -> lessThan right left
  PNeg (PGEq left right) -> lessThan left right
  _ -> Nothing

lessThan :: Expr EWord -> Expr EWord -> Maybe (Int, NumericBounds)
lessThan left right = comparison (-1) 1 left right

lessThanOrEqual :: Expr EWord -> Expr EWord -> Maybe (Int, NumericBounds)
lessThanOrEqual = comparison 0 0

comparison :: Integer -> Integer -> Expr EWord -> Expr EWord -> Maybe (Int, NumericBounds)
comparison upperAdjustment lowerAdjustment left right =
  case (decompose left, decompose right) of
    (Just (Argument argument offset), Just (Literal literal)) ->
      Just (argument, NumericBounds Nothing $ Just (literal - offset + upperAdjustment))
    (Just (Literal literal), Just (Argument argument offset)) ->
      Just (argument, NumericBounds (Just $ literal - offset + lowerAdjustment) Nothing)
    _ -> Nothing

equalTo :: Expr EWord -> Expr EWord -> Maybe (Int, NumericBounds)
equalTo left right =
  case (decompose left, decompose right) of
    (Just (Argument argument offset), Just (Literal literal)) -> exact argument (literal - offset)
    (Just (Literal literal), Just (Argument argument offset)) -> exact argument (literal - offset)
    _ -> Nothing
  where
    exact argument value = Just (argument, NumericBounds (Just value) (Just value))

data Affine
  = Argument Int Integer
  | Literal Integer

-- | Recognize the small affine fragment emitted for common require checks.
-- Argument names come from hevm's calldata builder and are one-based.
decompose :: Expr EWord -> Maybe Affine
decompose = \case
  Var name -> Argument <$> argumentIndex name <*> pure 0
  Lit value -> Just $ Literal (toInteger value)
  Add expression (Lit value) -> addOffset (toInteger value) =<< decompose expression
  Add (Lit value) expression -> addOffset (toInteger value) =<< decompose expression
  Sub expression (Lit value) -> addOffset (negate $ toInteger value) =<< decompose expression
  _ -> Nothing
  where
    addOffset offset = \case
      Argument argument current -> Just $ Argument argument (current + offset)
      Literal value -> Just $ Literal (value + offset)

argumentIndex :: T.Text -> Maybe Int
argumentIndex name = do
  suffix <- T.stripPrefix "arg" name
  oneBased <- readMaybe $ T.unpack suffix
  if oneBased > 0 then Just (oneBased - 1) else Nothing

intersectBounds :: NumericBounds -> NumericBounds -> NumericBounds
intersectBounds a b = NumericBounds
  { lowerBound = combineMaybe max a.lowerBound b.lowerBound
  , upperBound = combineMaybe min a.upperBound b.upperBound
  }

widenBounds :: NumericBounds -> NumericBounds -> NumericBounds
widenBounds a b = NumericBounds
  { lowerBound = min <$> a.lowerBound <*> b.lowerBound
  , upperBound = max <$> a.upperBound <*> b.upperBound
  }

combineMaybe :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
combineMaybe _ Nothing value = value
combineMaybe _ value Nothing = value
combineMaybe f (Just a) (Just b) = Just $ f a b

usefulBounds :: Method -> ArgumentBounds -> ArgumentBounds
usefulBounds method = Map.mapMaybeWithKey $ \argument bounds -> do
  (_, inputType) <- atMay method.inputs argument
  case inputType of
    AbiUIntType bits -> do
      range@(lower, upper) <- unsignedRange bits bounds
      if range == (0, 2 ^ bits - 1)
        then Nothing
        else Just $ NumericBounds (Just lower) (Just upper)
    _ -> Nothing

atMay :: [a] -> Int -> Maybe a
atMay values index
  | index < 0 = Nothing
  | otherwise = case drop index values of
      value : _ -> Just value
      [] -> Nothing
