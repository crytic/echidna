{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}

module Echidna.Exec where

import Control.Lens
import Control.Monad.Catch (Exception, MonadThrow(..))
import Control.Monad.State.Strict (MonadState, execState)
import Data.Foldable      (toList)
import Data.Has (Has(..))
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, fromJust, mapMaybe)
import Data.Set (Set)
import Data.Text (Text, append, pack, splitOn, unlines)
import Data.Text.Encoding (decodeUtf8)
import Data.List (sort, nub)
import EVM
import EVM.Op (Op(..))
import EVM.Exec (exec)
import EVM.Solidity --(SourceCache, SrcMap, SolcContract, contractName, sourceLines, sourceFiles, runtimeCode, runtimeSrcmap, creationSrcmap)
import EVM.Symbolic (Buffer(..))
import EVM.Debug (srcMapCodePos, srcMapCode)
import Prelude hiding (unlines)

import qualified Data.Vector.Storable as VS
import qualified Data.Vector as V

import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Set as S

--import Echidna.ABI (stripBytecodeMetadata)
import Echidna.Transaction
import Echidna.Types.Tx (TxCall(..), Tx, TxResult(..), call, dst)

-- | Broad categories of execution failures: reversions, illegal operations, and ???.
data ErrorClass = RevertE | IllegalE | UnknownE

-- | Given an execution error, classify it. Mostly useful for nice @pattern@s ('Reversion', 'Illegal').
classifyError :: Error -> ErrorClass
classifyError (OutOfGas _ _)         = RevertE
classifyError (Revert _)             = RevertE
classifyError (UnrecognizedOpcode _) = RevertE
classifyError (Query _)              = RevertE
classifyError StackUnderrun          = IllegalE
classifyError BadJumpDestination     = IllegalE
classifyError StackLimitExceeded     = IllegalE
classifyError IllegalOverflow        = IllegalE
classifyError _                      = UnknownE

-- | Matches execution errors that just cause a reversion.
pattern Reversion :: VMResult
pattern Reversion <- VMFailure (classifyError -> RevertE)

-- | Matches execution errors caused by illegal behavior.
pattern Illegal :: VMResult
pattern Illegal <- VMFailure (classifyError -> IllegalE)

-- | We throw this when our execution fails due to something other than reversion.
data ExecException = IllegalExec Error | UnknownFailure Error

instance Show ExecException where
  show (IllegalExec e) = "VM attempted an illegal operation: " ++ show e
  show (UnknownFailure e) = "VM failed for unhandled reason, " ++ show e
    ++ ". This shouldn't happen. Please file a ticket with this error message and steps to reproduce!"

instance Exception ExecException

-- | Given an execution error, throw the appropriate exception.
vmExcept :: MonadThrow m => Error -> m ()
vmExcept e = throwM $ case VMFailure e of {Illegal -> IllegalExec e; _ -> UnknownFailure e}

-- | Given an error handler, an execution function, and a transaction, execute that transaction
-- using the given execution strategy, handling errors with the given handler.
execTxWith :: (MonadState x m, Has VM x) => (Error -> m ()) -> m VMResult -> Tx -> m (VMResult, Int)
execTxWith h m t = do
  (og :: VM) <- use hasLens
  setupTx t
  gasIn <- use $ hasLens . state . gas
  res <- m
  gasOut <- use $ hasLens . state . gas
  cd  <- use $ hasLens . state . calldata
  case (res, t ^. call) of
    (f@Reversion, _) -> do
      hasLens .= og
      hasLens . state . calldata .= cd
      hasLens . result ?= f
    (VMFailure x, _) -> h x
    (VMSuccess (ConcreteBuffer bc), SolCreate _) ->
      (hasLens %=) . execState $ do
        env . contracts . at (t ^. dst) . _Just . contractcode .= InitCode ""
        replaceCodeOfSelf (RuntimeCode bc)
        loadContract (t ^. dst)
    _ -> pure ()
  pure (res, fromIntegral (gasIn - gasOut))

-- | Execute a transaction "as normal".
execTx :: (MonadState x m, Has VM x, MonadThrow m) => Tx -> m (VMResult, Int)
execTx = execTxWith vmExcept $ liftSH exec

type CoverageMap = Map BS.ByteString (Set (Int, TxResult))

-- | Given a way of capturing coverage info, execute while doing so once per instruction.
usingCoverage :: (MonadState x m, Has VM x) => m () -> m VMResult
usingCoverage cov = maybe (cov >> liftSH exec1 >> usingCoverage cov) pure =<< use (hasLens . result)

-- | Given good point coverage, count unique points.
coveragePoints :: CoverageMap -> Int
coveragePoints = sum . fmap S.size

-- | Given good point coverage, count the number of unique points but
-- only considering the different instruction PCs (discarding the TxResult).
-- This is useful to report a coverage measure to the user
scoveragePoints :: CoverageMap -> Int
scoveragePoints = sum . fmap (S.size . S.map fst)

-- | Pretty-print the covered code
ppCoveredCode :: SourceCache -> [SolcContract] -> CoverageMap -> Text
ppCoveredCode sc cs s | s == mempty = ""
                      | otherwise   = append "covered code:\n\n" $
                                      unlines $ map snd $ concat $ map (\(f,vls) -> 
                                                                           (mempty, (findFile f)) : 
                                                                           (filterLines covLines $ map ((findFile f,) . decodeUtf8) $ V.toList vls)
                                                                       ) allLines 
                                      where 
                                       allLines = M.toList $ view sourceLines sc
                                       findFile k = fst $ M.findWithDefault ("<no source code>", mempty) k (view sourceFiles sc)
                                       covLines = concat $ map (srcMapCov sc s) cs

type Filename = Text

markLine :: Int -> Filename -> [(Filename, Text)] -> [(Filename, Text)] 
markLine n f ls = case splitAt (n-1) ls of
                     (xs, (f',y):ys) -> xs ++ [(f, if (f == f') then  append "|" y else y)] ++ ys
                     _               -> error $ "invalid line " ++ (show n) ++ " to mark"


filterLines :: [Maybe (Filename, Int)] -> [(Filename, Text)] -> [(Filename, Text)]
filterLines []                   ls  = ls
filterLines (Nothing       : ns) ls  = filterLines ns ls
filterLines ((Just (f',n)) : ns) ls  = filterLines ns (markLine n f' ls)


{-
allPositions :: Map k SolcContract -> SourceCache -> Set (Text, Int)
allPositions solcByName sources =
      ( S.fromList
      . mapMaybe (srcMapCodePos sources)
      . toList
      $ mconcat
        ( solcByName
        & M.elems
        & map (\x -> view runtimeSrcmap x <> view creationSrcmap x)
        )
      )
-}

srcMapCov :: SourceCache -> Map BS.ByteString (Set (Int, b)) -> SolcContract -> [Maybe (Text, Int)]
srcMapCov sc s c = nub $ sort $ map (srcMapCodePos sc) $ mapMaybe (srcMapForOpLocation c) $ S.toList $ maybe S.empty (S.map fst) $ M.lookup (stripBytecodeMetadata $ view runtimeCode c) s

srcMapForOpLocation :: SolcContract -> Int -> Maybe SrcMap
srcMapForOpLocation c n = preview (ix n) (view runtimeSrcmap c)

linesByName :: SourceCache -> Map Text (V.Vector BS.ByteString)
linesByName sources =
      ( M.fromList
      . map
          (\(k, v) ->
             (fst (fromJust (M.lookup k (view sourceFiles sources))), v))
      . M.toList
      $ view sourceLines sources
      ) 

-- | Capture the current PC and bytecode (without metadata). This should identify instructions uniquely.
pointCoverage :: (MonadState x m, Has VM x) => Lens' x CoverageMap -> m ()
pointCoverage l = do
  v <- use hasLens
  l %= M.insertWith (const . S.insert $ ( fromJust $ vmOpIx v, Success))
                    (fromMaybe (error "no contract information on coverage") $ h v)
                    mempty
  where
    h v = stripBytecodeMetadata <$>
            v ^? env . contracts . at (v ^. state . contract) . _Just . bytecode

traceCoverage :: (MonadState x m, Has VM x, Has [Op] x) => m ()
traceCoverage = do
  v <- use hasLens
  let c = v ^. state . code
  hasLens <>= [readOp (BS.index c $ v ^. state . pc) c]
