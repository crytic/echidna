{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Echidna.Campaign where

import Control.Lens
import Control.Monad (liftM2, replicateM, when)
import Control.Monad.Catch (MonadCatch(..), MonadThrow(..))
import Control.Monad.Random.Strict (MonadRandom, RandT, evalRandT, getRandomR)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Strict (MonadState(..), StateT(..), evalStateT, execStateT, MonadIO, liftIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Random.Strict (liftCatch)
import Data.Aeson (ToJSON(..), object)
import Data.Binary.Get (runGetOrFail)
import Data.Bool (bool)
--import Data.Either (lefts)
import Data.Foldable (toList)
import Data.Map (Map, mapKeys, unionWith)
import Data.Maybe (fromMaybe, isNothing, mapMaybe, maybeToList)
import Data.Ord (comparing)
import Data.Has (Has(..))
import Data.Set (Set, union)
import Data.List (intercalate)

import EVM
import EVM.Types (W256)
import Numeric (showHex)

import System.Random (mkStdGen)

import qualified Data.HashMap.Strict  as H

import Echidna.ABI
import Echidna.ABIv2 (getAbi)
import Echidna.Exec
import Echidna.Solidity
import Echidna.Test
import Echidna.Transaction

instance MonadThrow m => MonadThrow (RandT g m) where
  throwM = lift . throwM
instance MonadCatch m => MonadCatch (RandT g m) where
  catch = liftCatch catch

-- | Configuration for running an Echidna 'Campaign'.
data CampaignConf = CampaignConf { testLimit     :: Int
                                   -- ^ Maximum number of function calls to execute while fuzzing
                                 , seqLen        :: Int
                                   -- ^ Number of calls between state resets (e.g. \"every 10 calls,
                                   -- reset the state to avoid unrecoverable states/save memory\"
                                 , shrinkLimit   :: Int
                                   -- ^ Maximum number of candidate sequences to evaluate while shrinking
                                 , knownCoverage :: Maybe (Map W256 (Set Int))
                                   -- ^ If applicable, initially known coverage. If this is 'Nothing',
                                   -- Echidna won't collect coverage information (and will go faster)
                                 , seed          :: Maybe Int
                                 , corpusDir     :: Maybe FilePath
                                 }

-- | State of a particular Echidna test. N.B.: \"Solved\" means a falsifying call sequence was found.
data TestState = Open Int             -- ^ Maybe solvable, tracking attempts already made
               | Large Int [Tx]       -- ^ Solved, maybe shrinable, tracking shrinks tried + best solve
               | Passed               -- ^ Presumed unsolvable
               | Solved [Tx]          -- ^ Solved with no need for shrinking
               | Failed ExecException -- ^ Broke the execution environment
                 deriving Show

instance Eq TestState where
  (Open i)    == (Open j)    = i == j
  (Large i l) == (Large j m) = i == j && l == m
  Passed      == Passed      = True
  (Solved l)  == (Solved m)  = l == m
  _           == _           = False

instance ToJSON TestState where
  toJSON s = object $ ("passed", toJSON passed) : maybeToList desc where
    (passed, desc) = case s of Open _    -> (True, Nothing)
                               Passed    -> (True, Nothing)
                               Large _ l -> (False, Just ("callseq", toJSON l))
                               Solved  l -> (False, Just ("callseq", toJSON l))
                               Failed  e -> (False, Just ("exception", toJSON $ show e))

-- | The state of a fuzzing campaign.
data Campaign = Campaign { _tests       :: [(SolTest, TestState)]
                           -- ^ Tests being evaluated
                         , _coverage    :: Map W256 (Set Int)
                           -- ^ Coverage captured (NOTE: we don't always record this)
                         , _genDict     :: GenDict
                           -- ^ Generation dictionary
                         , _initialized :: Bool
                           -- ^ Has the campaign started
                         , _newCoverage :: Bool
                         , _genTrans :: [[Tx]]
                         }

instance ToJSON Campaign where
  toJSON (Campaign ts co _ _ _ _) = object $ ("tests", toJSON $ mapMaybe format ts)
    : if co == mempty then [] else [("coverage",) . toJSON . mapKeys (`showHex` "") $ toList <$> co] where
      format (Right _,      Open _) = Nothing
      format (Right (n, _), s)      = Just ("assertion in " <> n, toJSON s)
      format (Left (n, _),  s)      = Just (n,                    toJSON s)

makeLenses ''Campaign

instance Has GenDict Campaign where
  hasLens = genDict

defaultCampaign :: Campaign
defaultCampaign = Campaign mempty mempty defaultDict False False mempty

-- | Given a 'Campaign', checks if we can attempt any solves or shrinks without exceeding
-- the limits defined in our 'CampaignConf'.
isDone :: (MonadReader x m, Has CampaignConf x) => Campaign -> m Bool
isDone (Campaign _ _ _ False _ _) = pure False
isDone (Campaign ts _ _ _ _ _) = view (hasLens . to (liftM2 (,) testLimit shrinkLimit)) <&> \(tl, sl) ->
  all (\case Open i -> i >= tl; Large i _ -> i >= sl; _ -> True) $ snd <$> ts

-- | Given a 'Campaign', check if the test results should be reported as a
-- success or a failure.
isSuccess :: Campaign -> Bool
isSuccess (Campaign ts _ _ _ _ _) =
  all (\case { Passed -> True; Open _ -> True; _ -> False; }) $ snd <$> ts


-- | Given a 'Campaign', return the progress of it
getProgress :: Campaign -> Int
getProgress (Campaign ts _ _ _ _ _) = foldr1 max $ map ((\case Open i -> i; _ -> 0) . snd) ts 

-- | Given an initial 'VM' state and a @('SolTest', 'TestState')@ pair, as well as possibly a sequence
-- of transactions and the state after evaluation, see if:
-- (0): The test is past its 'testLimit' or 'shrinkLimit' and should be presumed un[solve|shrink]able
-- (1): The test is 'Open', and this sequence of transactions solves it
-- (2): The test is 'Open', and evaluating it breaks our runtime
-- (3): The test is unshrunk, and we can shrink it
-- Then update accordingly, keeping track of how many times we've tried to solve or shrink.
updateTest :: ( MonadCatch m, MonadRandom m, MonadReader x m
              , Has SolConf x, Has TestConf x, Has TxConf x, Has CampaignConf x)
           => VM -> Maybe (VM, [Tx]) -> (SolTest, TestState) -> m (SolTest, TestState)
updateTest v (Just (v', xs)) (n, t) = view (hasLens . to testLimit) >>= \tl -> (n,) <$> case t of
  Open i    | i >= tl -> pure Passed
  Open i              -> catch (evalStateT (checkETest n) v' <&> bool (Large (-1) xs) (Open (i + 1)))
                               (pure . Failed)
  _                   -> snd <$> updateTest v Nothing (n,t)
updateTest v Nothing (n, t) = view (hasLens . to shrinkLimit) >>= \sl -> (n,) <$> case t of
  Large i x | i >= sl -> pure $ Solved x
  Large i x           -> if length x > 1 || any canShrinkTx x
                           then Large (i + 1) <$> evalStateT (shrinkSeq n x) v
                           else pure $ Solved x
  _                   -> pure t

-- | Given a rule for updating a particular test's state, apply it to each test in a 'Campaign'.
runUpdate :: (MonadReader x m, Has TxConf x, MonadState y m, Has Campaign y)
          => ((SolTest, TestState) -> m (SolTest, TestState)) -> m ()
runUpdate f = use (hasLens . tests) >>= mapM f >>= (hasLens . tests .=)

-- | Given an initial 'VM' state and a way to run transactions, evaluate a list of transactions, constantly
-- checking if we've solved any tests or can shrink known solves.
evalSeq :: ( MonadCatch m, MonadRandom m, MonadReader x m, MonadState y m
           , Has SolConf x, Has TestConf x, Has TxConf x, Has CampaignConf x, Has Campaign y, Has VM y)
        => VM -> (Tx -> m a) -> [Tx] -> m [(Tx, a)]
evalSeq v e = go [] where
  go r xs = use hasLens >>= \v' -> runUpdate (updateTest v $ Just (v',reverse r)) >>
    case xs of []     -> pure []
               (y:ys) ->  e y >>= \a -> ((y, a) :) <$> go (y:r) ys

-- | Execute a transaction, capturing the PC and codehash of each instruction executed, saving the
-- transaction if it finds new coverage.
execTxOptC :: (MonadState x m, Has Campaign x, Has VM x, MonadThrow m) => Tx -> m VMResult
execTxOptC t = do
  og  <- hasLens . coverage <<.= mempty
  res <- execTxWith vmExcept (usingCoverage $ pointCoverage (hasLens . coverage)) t
  hasLens . coverage %= unionWith union og
  grew <- (== LT) . comparing coveragePoints og <$> use (hasLens . coverage)
  --when grew $ hasLens . genDict %= gaddCalls (lefts [t ^. call])
  when grew $ hasLens . newCoverage .= True
  return res

deleteAt :: Int -> [a] -> [a]
deleteAt _ [] = []
deleteAt i (a:as)
   | i == 0    = as
   | otherwise = a : deleteAt (i-1) as

insertAt :: a -> [a] -> Int -> [a]
insertAt v [] _ = [v]
insertAt v arr 1 = (v:arr)
insertAt v (x:xs) n = (x:(insertAt v xs $ n - 1))

insertAtRandom :: MonadRandom m => [a] -> [a] -> m [a]
insertAtRandom xs [] = return xs
insertAtRandom xs (y:ys) = do idx <- getRandomR (0, (length xs) - 1) 
                              insertAtRandom (insertAt y xs idx) ys


-- taken from https://stackoverflow.com/questions/30551033/swap-two-elements-in-a-list-by-its-indices/30551130#30551130
swapAt :: [a] -> Int -> Int -> [a]
swapAt xs i j =  let elemI = xs !! i
                     elemJ = xs !! j
                     left = take i xs
                     middle = take (j - i - 1) (drop (i + 1) xs)
                     right = drop (j + 1) xs
                 in  left ++ [elemJ] ++ middle ++ [elemI] ++ right 


randseq :: ( MonadCatch m, MonadRandom m, MonadIO m,  MonadReader x m, MonadState y m
           , Has GenDict y, Has TxConf x, Has TestConf x, Has CampaignConf x, Has Campaign y)
        => Int -> Int ->  World -> m [Tx]

randseq 1 p w = do  ca <- use hasLens
                    n <- getRandomR (0, 3 :: Integer)
                    let gts = ca ^. genTrans
                    if (length gts > p) 
                    then return $ gts !! p
                    else 
                     do 
                      gtxs <- replicateM 1 (evalStateT genTxM (w, ca ^. genDict))
                      case (n, gts) of
                              -- random generation of transactions
                       (_, []) -> return gtxs
                              -- mutate the transaction from a rare sequence
                       (0, _ ) -> do idx <- getRandomR (0, (length gts) - 1)
                                     let rtxs = gts !! idx
                                     sequence $ map mutTx rtxs
                              -- shrink all elements from a rare sequence
                       (1, _ ) -> do idx <- getRandomR (0, (length gts) - 1)
                                     let rtxs = gts !! idx
                                     sequence $ map shrinkTx rtxs
                       (2, _ ) -> do idx <- getRandomR (0, (length gts) - 1)
                                     let rtxs = gts !! idx
                                     tx' <- spliceTx (head rtxs) (concat gts)
                                     return [tx'] 
                       _       -> return gtxs


randseq ql p w = do ca <- use hasLens
                    n <- getRandomR (0, 13 :: Integer)
                    let gts = ca ^. genTrans
                    if (length gts > p) 
                    then return $ gts !! p
                    else 
                      do 
                      gtxs <- replicateM ql (evalStateT genTxM (w, ca ^. genDict))
                      case (n, gts) of
                              -- random generation of transactions
                       (_, []) -> return gtxs
                              -- concatenate rare sequences
                       (0, _ ) -> do idxs <- sequence $ replicate 10 (getRandomR (0, (length gts) - 1))
                                     return $ take (10*ql) $ concatMap (gts !!) idxs
                              -- mutate one transaction from a rare sequence
                       (1, _ ) -> do idx <- getRandomR (0, (length gts) - 1)
                                     let rtxs = gts !! idx
                                     k <- getRandomR (0, (length rtxs - 1))
                                     mtx <- mutTx $ rtxs !! k
                                     return $ replaceAt mtx rtxs k
                              -- mutate all transactions in a rare sequence
                       (2, _ ) -> do idx <- getRandomR (0, (length gts) - 1)
                                     let rtxs = gts !! idx
                                     sequence $ map mutTx rtxs 
                              -- shrink all elements from a rare sequence
                       (3, _ ) -> do idx <- getRandomR (0, (length gts) - 1)
                                     sequence $ map shrinkTx $ gts !! idx
                              -- randomly insert transactions into a rare sequence
                       (4, _ ) -> do idx <- getRandomR (0, (length gts) - 1)
                                     k <- getRandomR (1, 10)
                                     rtxs <- insertAtRandom (gts !! idx) (take k gtxs)
                                     return $ take (10*ql) rtxs
                              -- randomly remove transactions from a rare sequence
                       (5, _ ) -> do idx <- getRandomR (0, (length gts) - 1)
                                     k <- getRandomR (0, (length  (gts !! idx)) - 1 )
                                     return $ deleteAt k (gts !! idx)
                              -- randomly swap transactions from a rare sequence
                       (6, _ ) -> do idx <- getRandomR (0, (length gts) - 1)
                                     let rtxs = gts !! idx
                                     k1 <- getRandomR (0, (length  rtxs) - 1 )
                                     k2 <- getRandomR (0, (length  rtxs) - 1 )
                                     return $ swapAt rtxs k1 k2

                       _       -> return gtxs
              

-- | Given an initial 'VM' and 'World' state and a number of calls to generate, generate that many calls,
-- constantly checking if we've solved any tests or can shrink known solves. Update coverage as a result
callseq :: ( MonadCatch m, MonadRandom m, MonadReader x m, MonadState y m, MonadIO m
           , Has SolConf x, Has TestConf x, Has TxConf x, Has CampaignConf x, Has Campaign y, Has GenDict y)
        => VM -> World -> Int -> m ()
callseq v w ql = do
  -- Reset the new coverage flag
  hasLens . newCoverage .= False
  -- First, we figure out whether we need to execute with or without coverage optimization, and pick
  -- our execution function appropriately
  ef <- bool execTx execTxOptC . isNothing . knownCoverage <$> view hasLens
  -- Then, we get the current campaign state
  ca <- use hasLens
  -- Then, we generate the actual transaction in the sequence
  --liftIO $ print ((getProgress ca + 1) `div` (ql+1))
  is <- randseq ql ((getProgress ca + 1) `div` (ql+1)) w
  -- We then run each call sequentially. This gives us the result of each call, plus a new state
  (res, (_, ca')) <- runStateT (evalSeq v ef is) (v, ca)
  -- Save the global campaign state (also vm state, but that gets reset before it's used)
  hasLens .= ca'
  let rtxs = map fst (filter (\(_, vm) -> classifyRes vm /= ResRevert) res)
  when (ca' ^. newCoverage) $ liftIO $ putStrLn $ "new coverage:" ++ (ppTxs rtxs)
  when (ca' ^. newCoverage) $ hasLens . genTrans %= (rtxs:)
  -- Now we try to parse the return values as solidity constants, and add then to the 'GenDict'
  modifying (hasLens . constants) . H.unionWith (++) . parse res =<< use (hasLens . rTypes) where
    -- Given a list of transactions and a return typing rule, this checks whether we know the return
    -- type for each function called, and if we do, tries to parse the return value as a value of that
    -- type. It returns a 'GenDict' style HashMap.
    parse l rt = H.fromList . flip mapMaybe l $ \(x, r) -> case (rt =<< x ^? call . _Left . _1, r) of
      (Just ty, VMSuccess b) -> (ty, ) . pure <$> runGetOrFail (getAbi ty) (b ^. lazy) ^? _Right . _3
      _                      -> Nothing

-- | Run a fuzzing campaign given an initial universe state, some tests, and an optional dictionary
-- to generate calls with. Return the 'Campaign' state once we can't solve or shrink anything.
campaign :: ( MonadCatch m, MonadRandom m, MonadReader x m, MonadIO m
            , Has SolConf x, Has TestConf x, Has TxConf x, Has CampaignConf x)
         => StateT Campaign m a -- ^ Callback to run after each state update (for instrumentation)
         -> VM                  -- ^ Initial VM state
         -> World               -- ^ Initial world state
         -> [SolTest]           -- ^ Tests to evaluate
         -> Maybe GenDict       -- ^ Optional generation dictionary
         -> [[Tx]]
         -> m Campaign
campaign u v w ts d txs = let d' = fromMaybe defaultDict d in fmap (fromMaybe mempty) (view (hasLens . to knownCoverage)) >>= \c -> do
  g <- view (hasLens . to seed)
  let g' = mkStdGen $ fromMaybe (d' ^. defSeed) g
  execStateT (evalRandT runCampaign g') (Campaign ((,Open (-1)) <$> ts) c d' True False txs) where
    step        = runUpdate (updateTest v Nothing) >> lift u >> runCampaign
    runCampaign = use (hasLens . tests . to (fmap snd)) >>= update
    update c    = view hasLens >>= \(CampaignConf tl q sl _ _ _) ->
      if | any (\case Open  n   -> n < tl; _ -> False) c -> callseq v w q >> step
         | any (\case Large n _ -> n < sl; _ -> False) c -> step
         | otherwise                                     -> lift u


ppTxs :: [Tx] -> String
ppTxs txs = intercalate " " $ map (\(Tx c _ _ _ _ _ ) -> either ppSolCall (const "<CREATE>") c) txs
