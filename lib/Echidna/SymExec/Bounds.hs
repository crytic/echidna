{-# LANGUAGE GADTs #-}

module Echidna.SymExec.Bounds
  ( discoverMethodBounds
  , inferArgumentBounds
  , suitableForConstraintDiscovery
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.STM (atomically, newTVarIO, writeTVar)
import Control.Monad (filterM)
import Control.Monad.IO.Unlift (liftIO)
import Control.Monad.State.Strict (execStateT)
import Data.Bits ((.|.))
import Data.Function ((&))
import Data.List (foldl')
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Text qualified as T
import Optics.Core ((.~), (%), (%~))
import Text.Read (readMaybe)

import EVM (symbolify)
import EVM.ABI (AbiKind(..), AbiType(..), Sig(..), abiKind)
import EVM.Effects (App)
import EVM.Expr qualified as Expr
import EVM.Fetch qualified as Fetch
import EVM.Solidity (Method(..), Mutability(..))
import EVM.Solvers (SolverGroup, checkSatWithPropsAbortable, withRunAbort)
import EVM.SymExec (IterConfig, interpretVMAbortable, mkCalldata, runExpr)
import EVM.Types (EType(..), Expr(..), ProofResult(Qed), Prop(..), VMType(..))
import EVM.Types qualified as EVM

import Echidna.Types.Constraint
  (ArgumentBounds, NumericBounds(..), signedRange, unsignedRange)
import Echidna.Types.Tx (Tx)
import Echidna.Transaction (setupTx)

-- | Only run the lightweight pass on state-changing methods whose calldata is
-- cheap to symbolize and which have at least one integer to improve.
suitableForConstraintDiscovery :: Method -> Bool
suitableForConstraintDiscovery method =
  method.mutability `notElem` [Pure, View]
  && any (isInteger . snd) method.inputs
  && all (supported . snd) method.inputs
  && not ("_no_symexec" `T.isInfixOf` method.name)
  where
    isInteger (AbiUIntType _) = True
    isInteger (AbiIntType _) = True
    isInteger _ = False

    supported t = abiKind t == Static && case t of
      AbiUIntType _ -> True
      AbiIntType _ -> True
      AbiAddressType -> True
      AbiBoolType -> True
      AbiBytesType _ -> True
      _ -> False

-- | Symbolically execute one method and approximate intervals that reach a
-- successful return. Lightweight satisfiability checks discard contradictory
-- leaves, but no models are requested, keeping this pass substantially cheaper
-- than normal symbolic exploration.
discoverMethodBounds :: App m =>
  Method
  -> EVM.VM Concrete
  -> Tx -- ^ Target transaction, including its concrete envelope
  -> IterConfig
  -> Int -- ^ Maximum wall-clock time for this discovery request, in seconds
  -> SolverGroup
  -> Fetch.RpcInfo
  -> Fetch.Session
  -> m ArgumentBounds
discoverMethodBounds method vm targetTx iterConfig timeoutSeconds solvers rpcInfo session = do
  ((calldata, calldataConstraints), _caveats) <-
    mkCalldata (Just (Sig method.methodSignature (snd <$> method.inputs))) []
  symbolicVm <- prepareBoundsCall targetTx vm calldata calldataConstraints
  -- A single complex request must not monopolize the only discovery worker.
  -- Solver queries have their own timeout, but interpretation between queries
  -- also needs a wall-clock budget so later corpus states can be sampled.
  (abort, timerThread) <- liftIO $ do
    timer <- newTVarIO False
    thread <- forkIO $ do
      threadDelay (max 1 timeoutSeconds * 1_000_000)
      atomically $ writeTVar timer True
    pure (timer, thread)
  let fetcher = Fetch.oracle solvers (Just session) rpcInfo
  leaves <- withRunAbort solvers abort $
    interpretVMAbortable abort fetcher iterConfig symbolicVm runExpr $ \leaf _ _ -> pure leaf
  let candidatePaths =
        [ Expr.simplifyProps props
        | leaf <- leaves
        , props <- case leaf of
            Success pathProps _ _ _ -> [pathProps]
            -- A bounded run commonly stops after it has passed the guards we
            -- care about. Retain that prefix, as the original prototype did.
            Partial pathProps _ _ -> [pathProps]
            _ -> []
        ]
      feasible props = checkSatWithPropsAbortable solvers (Just abort) props >>= \case
        Qed -> pure False
        _ -> pure True
  nonRevertingPaths <- filterM feasible candidatePaths
  liftIO $ killThread timerThread
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
pathBounds props = foldl' add Map.empty $
  mapMaybe (boundFromProp narrowedArguments) flattened
  where
    flattened = concatMap flatten props
    narrowedArguments = Set.fromList $ mapMaybe narrowedArgument flattened

    add bounds (argument, range) = Map.insertWith intersectBounds argument range bounds

    flatten (PAnd a b) = flatten a <> flatten b
    flatten prop = [prop]

-- Solidity's ABI decoder constrains narrow integer arguments with equalities
-- such as @arg2 == arg2 & 0xff@. Within that path, the masked expression is
-- therefore equivalent to the argument and can safely participate in bounds.
narrowedArgument :: Prop -> Maybe Int
narrowedArgument = \case
  PEq left right -> do
    leftWord <- Expr.asWord left
    rightWord <- Expr.asWord right
    maskedEquality leftWord rightWord <|> maskedEquality rightWord leftWord
  _ -> Nothing
  where
    maskedEquality :: Expr EWord -> Expr EWord -> Maybe Int
    maskedEquality (Var name) (And (Lit _) (Var maskedName))
      | name == maskedName = argumentIndex name
    maskedEquality (Var name) (And (Var maskedName) (Lit _))
      | name == maskedName = argumentIndex name
    maskedEquality _ _ = Nothing

boundFromProp :: Set.Set Int -> Prop -> Maybe (Int, NumericBounds)
boundFromProp narrowed = \case
  PLT left right -> lessThan narrowed left right
  PGT left right -> lessThan narrowed right left
  PLEq left right -> lessThanOrEqual narrowed left right
  PGEq left right -> lessThanOrEqual narrowed right left
  PEq result (SLT left right) -> signedComparisonResult narrowed result left right
  PEq (SLT left right) result -> signedComparisonResult narrowed result left right
  PEq left right -> do
    leftWord <- Expr.asWord left
    rightWord <- Expr.asWord right
    equalTo narrowed leftWord rightWord
  PNeg (PLT left right) -> lessThanOrEqual narrowed right left
  PNeg (PGT left right) -> lessThanOrEqual narrowed left right
  PNeg (PLEq left right) -> lessThan narrowed right left
  PNeg (PGEq left right) -> lessThan narrowed left right
  _ -> Nothing

signedComparisonResult
  :: Set.Set Int
  -> Expr EWord
  -> Expr EWord
  -> Expr EWord
  -> Maybe (Int, NumericBounds)
signedComparisonResult narrowed (Lit 0) left right = lessThanOrEqual narrowed right left
signedComparisonResult narrowed (Lit 1) left right = lessThan narrowed left right
signedComparisonResult _ _ _ _ = Nothing

lessThan :: Set.Set Int -> Expr EWord -> Expr EWord -> Maybe (Int, NumericBounds)
lessThan narrowed = comparison narrowed (-1) 1

lessThanOrEqual :: Set.Set Int -> Expr EWord -> Expr EWord -> Maybe (Int, NumericBounds)
lessThanOrEqual narrowed = comparison narrowed 0 0

comparison :: Set.Set Int -> Integer -> Integer -> Expr EWord -> Expr EWord -> Maybe (Int, NumericBounds)
comparison narrowed upperAdjustment lowerAdjustment left right =
  case (decompose narrowed left, decompose narrowed right) of
    (Just (Argument argument coefficient offset), Just (Literal literal)) ->
      argumentComparedToLiteral argument coefficient offset literal
    (Just (Literal literal), Just (Argument argument coefficient offset)) ->
      literalComparedToArgument literal argument coefficient offset
    _ -> Nothing
  where
    argumentComparedToLiteral argument 1 offset literal =
      Just (argument, NumericBounds Nothing $ Just (literal - offset + upperAdjustment))
    argumentComparedToLiteral argument (-1) offset literal =
      Just (argument, NumericBounds (Just $ offset - literal + lowerAdjustment) Nothing)
    argumentComparedToLiteral _ _ _ _ = Nothing

    literalComparedToArgument literal argument 1 offset =
      Just (argument, NumericBounds (Just $ literal - offset + lowerAdjustment) Nothing)
    literalComparedToArgument literal argument (-1) offset =
      Just (argument, NumericBounds Nothing $ Just (offset - literal + upperAdjustment))
    literalComparedToArgument _ _ _ _ = Nothing

equalTo :: Set.Set Int -> Expr EWord -> Expr EWord -> Maybe (Int, NumericBounds)
equalTo narrowed left right =
  case (decompose narrowed left, decompose narrowed right) of
    (Just (Argument argument coefficient offset), Just (Literal literal)) ->
      exact argument =<< solve coefficient offset literal
    (Just (Literal literal), Just (Argument argument coefficient offset)) ->
      exact argument =<< solve coefficient offset literal
    _ -> Nothing
  where
    exact argument value = Just (argument, NumericBounds (Just value) (Just value))
    solve 1 offset literal = Just $ literal - offset
    solve (-1) offset literal = Just $ offset - literal
    solve _ _ _ = Nothing

data Affine
  = Argument Int Integer Integer
  | Literal Integer

-- | Recognize the small affine fragment emitted for common require checks.
-- Argument names come from hevm's calldata builder and are one-based.
decompose :: Set.Set Int -> Expr EWord -> Maybe Affine
decompose narrowed = \case
  Var name -> Argument <$> argumentIndex name <*> pure 1 <*> pure 0
  Lit value -> Just $ Literal (toInteger value)
  SEx _ expression -> decompose narrowed expression
  expression@Or {} -> reassembledArgument expression
  And (Lit _) (Var name) -> narrowedVariable name
  And (Var name) (Lit _) -> narrowedVariable name
  Add expression (Lit value) -> addOffset (toInteger value) =<< decompose narrowed expression
  Add (Lit value) expression -> addOffset (toInteger value) =<< decompose narrowed expression
  Sub expression (Lit value) -> addOffset (negate $ toInteger value) =<< decompose narrowed expression
  Sub (Lit value) expression -> subtractFromLiteral (toInteger value) =<< decompose narrowed expression
  _ -> Nothing
  where
    narrowedVariable name = do
      argument <- argumentIndex name
      if argument `Set.member` narrowed
        then Just $ Argument argument 1 0
        else Nothing

    -- ABI round-trips through an external call can split a word into masked
    -- pieces and OR them back together. When every piece comes from the same
    -- argument and the masks cover the full word, the result is exactly that
    -- argument and can safely participate in bounds.
    reassembledArgument expression = do
      (name, mask) <- maskedPieces expression
      if mask == maxBound
        then Argument <$> argumentIndex name <*> pure 1 <*> pure 0
        else Nothing

    maskedPieces :: Expr EWord -> Maybe (T.Text, EVM.W256)
    maskedPieces = \case
      And (Lit mask) (Var name) -> Just (name, mask)
      And (Var name) (Lit mask) -> Just (name, mask)
      Or left right -> do
        (leftName, leftMask) <- maskedPieces left
        (rightName, rightMask) <- maskedPieces right
        if leftName == rightName
          then Just (leftName, leftMask .|. rightMask)
          else Nothing
      _ -> Nothing

    addOffset offset = \case
      Argument argument coefficient current ->
        Just $ Argument argument coefficient (current + offset)
      Literal value -> Just $ Literal (value + offset)

    subtractFromLiteral literal = \case
      Argument argument coefficient offset ->
        Just $ Argument argument (negate coefficient) (literal - offset)
      Literal value -> Just $ Literal (literal - value)

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
    AbiIntType bits -> do
      range@(lower, upper) <- signedRange bits bounds
      if range == (negate $ 2 ^ (bits - 1), 2 ^ (bits - 1) - 1)
        then Nothing
        else Just $ NumericBounds (Just lower) (Just upper)
    _ -> Nothing

atMay :: [a] -> Int -> Maybe a
atMay values index
  | index < 0 = Nothing
  | otherwise = case drop index values of
      value : _ -> Just value
      [] -> Nothing
