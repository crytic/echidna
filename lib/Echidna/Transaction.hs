{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Echidna.Transaction where

import Prelude hiding (Word)

import Control.Lens
import Control.Monad (join, liftM2, liftM3, liftM5, unless)
import Control.Monad.Catch (MonadThrow, bracket)
import Control.Monad.Random.Strict (MonadRandom, getRandomR, uniform, weighted)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Strict (MonadState, State, evalStateT, runState, get, put)
import Data.Aeson (ToJSON(..), decodeStrict, encodeFile)
import Data.Has (Has(..))
import Data.Hashable (hash)
import Data.Map (Map, toList)
import Data.Maybe (catMaybes)
import EVM hiding (value)
import EVM.ABI (abiCalldata, abiValueType)
import EVM.Concrete (Word(..), w256)
import EVM.Types (Addr)

import qualified System.Directory as SD
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as M
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector as V

import Echidna.ABI
import Echidna.Types.Random
import Echidna.Orphans.JSON ()
import Echidna.Types.Signature (SignatureMap, SolCall, ContractA, FunctionHash)
import Echidna.Types.Tx
import Echidna.Types.World (World(..))

-- | If half a tuple is zero, make both halves zero. Useful for generating delays, since block number
-- only goes up with timestamp
level :: (Num a, Eq a) => (a, a) -> (a, a)
level (elemOf each 0 -> True) = (0,0)
level x                       = x

-- | Given generators for an origin, destination, value, and function call, generate a call
-- transaction. Note: This doesn't generate @CREATE@s because I don't know how to do that at random.
genTxWith :: (MonadRandom m, MonadState x m, Has World x, MonadThrow m)
          => Map Addr Contract                        -- ^ List of contracts
          -> (NE.NonEmpty Addr -> m Addr)             -- ^ Sender generator
          -> (NE.NonEmpty ContractA -> m ContractA)   -- ^ Receiver generator
          -> (Addr -> ContractA -> m SolCall)         -- ^ Call generator
          -> m Word                                   -- ^ Gas generator
          -> m Word                                   -- ^ Gas price generator
          -> Word                                     -- ^ Max value generator
          -> m (Word, Word)                           -- ^ Delay generator
          -> m Tx
genTxWith m s r c g gp mv t = use hasLens >>= \(World ss hmm lmm ps) ->
  getSignatures hmm lmm >>= \mm ->
  let s' = s ss
      r' = r rs
      c' = join $ liftM2 c s' r'
      v' = genValue ps mv
      rs = NE.fromList . catMaybes $ mkR <$> toList m
      mkR = _2 (flip M.lookup mm . view (bytecode . to stripBytecodeMetadata))
  in
    ((liftM5 Tx (SolCall <$> c') s' (fst <$> r') g gp <*>) =<< liftM3 v' s' r' c') <*> t

getSignatures :: MonadRandom m => SignatureMap -> Maybe SignatureMap -> m SignatureMap
getSignatures hmm Nothing = return hmm
getSignatures hmm (Just lmm) = usuallyRarely hmm lmm -- once in a while, this will use the low-priority signature for the input generation

-- | Synthesize a random 'Transaction', not using a dictionary.
genTx :: forall m x y. (MonadRandom m, MonadReader x m, Has TxConf x, MonadState y m, Has World y, MonadThrow m)
      => Map Addr Contract
      -> m Tx
genTx m = use (hasLens :: Lens' y World) >>= evalStateT (genTxM m) . (defaultDict,)

-- | Generate a random 'Transaction' with either synthesis or mutation of dictionary entries.
genTxM :: (MonadRandom m, MonadReader x m, Has TxConf x, MonadState y m, Has GenDict y, Has World y, MonadThrow m)
  => Map Addr Contract
  -> m Tx
genTxM m = view hasLens >>= \(TxConf _ g maxGp t b mv) -> genTxWith
  m
  rElem rElem                                                                -- src and dst
  (const $ genInteractionsM . snd)                                           -- call itself
  (pure g) (inRange maxGp) mv                                                -- gas, gasprice, value
  (level <$> liftM2 (,) (inRange t) (inRange b))                             -- delay
     where inRange hi = w256 . fromIntegral <$> getRandomR (0 :: Integer, fromIntegral hi)

genValue :: (MonadRandom m) => [FunctionHash] -> Word -> Addr -> ContractA -> SolCall -> m Word
genValue ps mv _ _ sc = 
  if sig `elem` ps
  then fromIntegral <$> randValue
  else do
    g <- usuallyRarely (pure 0) randValue -- once in a while, this will generate value in a non-payable function
    fromIntegral <$> g
  where randValue = getRandomR (1 :: Integer, fromIntegral mv)
        sig = (hashSig . encodeSig . signatureCall) sc 

-- | Check if a 'Transaction' is as \"small\" (simple) as possible (using ad-hoc heuristics).
canShrinkTx :: Tx -> Bool
canShrinkTx (Tx (SolCreate _) _ _ _ 0 0 (0, 0))   = False
canShrinkTx (Tx (SolCall (_,l)) _ _ _ 0 0 (0, 0)) = any canShrinkAbiValue l
canShrinkTx (Tx (SolCalldata _) _ _ _ 0 0 (0, 0)) = False
canShrinkTx _                                     = True

-- | Given a 'Transaction', generate a random \"smaller\" 'Transaction', preserving origin,
-- destination, value, and call signature.
shrinkTx :: MonadRandom m => Tx -> m Tx
shrinkTx tx'@(Tx c _ _ _ gp (C _ v) (C _ t, C _ b)) = let
  c' = case c of
            SolCreate{}   -> pure c
            SolCall sc    -> SolCall <$> shrinkAbiCall sc
            SolCalldata{} -> pure c
  lower 0 = pure $ w256 0
  lower x = w256 . fromIntegral <$> getRandomR (0 :: Integer, fromIntegral x)
              >>= \r -> uniform [0, r] -- try 0 quicker
  possibilities =
    [ set call      <$> c'
    , set value     <$> lower v
    , set gasprice' <$> lower gp
    , set delay     <$> fmap level (liftM2 (,) (lower t) (lower b))
    ]
  in join (uniform possibilities) <*> pure tx'

mutateTx :: (MonadRandom m) => Tx -> m Tx
mutateTx t@(Tx (SolCall c) _ _ _ _ _ _) = do f <- weighted [(mutate, 10), (skip, 90)] --FIXME: make a parameter for this?
                                             f c
                                           where mutate  z = mutateAbiCall z >>= \c' -> pure $ t { _call = SolCall c' }
                                                 skip    _ = pure t
mutateTx t                              = pure t

-- | Lift an action in the context of a component of some 'MonadState' to an action in the
-- 'MonadState' itself.
liftSH :: (MonadState a m, Has b a) => State b x -> m x
liftSH = stateST . runState . zoom hasLens
  -- This is the default state function written in terms of get and set:
  where stateST f = do
          s <- get
          let ~(a, s') = f s
          put s'
          return a

-- | Given a 'Transaction', set up some 'VM' so it can be executed. Effectively, this just brings
-- 'Transaction's \"on-chain\".
setupTx :: (MonadState x m, Has VM x) => Tx -> m ()
setupTx (Tx c s r g gp v (t, b)) = liftSH . sequence_ $
  [ result .= Nothing, state . pc .= 0, state . stack .= mempty, state . memory .= mempty, state . gas .= g
  , tx . gasprice .= gp, tx . origin .= s, state . caller .= s, state . callvalue .= v
  , block . timestamp += t, block . number += b, setup] where
    setup = case c of
      SolCreate bc   -> assign (env . contracts . at r) (Just $ initialContract (RuntimeCode bc) & set balance v) >> loadContract r
      SolCall cd     -> incrementBalance >> loadContract r >> state . calldata .= encode cd
      SolCalldata cd -> incrementBalance >> loadContract r >> state . calldata .= cd
    incrementBalance = (env . contracts . ix r . balance) += v
    encode (n, vs) = abiCalldata
      (encodeSig (n, abiValueType <$> vs)) $ V.fromList vs

saveTxs :: Maybe FilePath -> [[Tx]] -> IO ()
saveTxs (Just d) txs = mapM_ saveTx txs where 
  saveTx v = do let fn = d ++ "/" ++ (show . hash . show) v ++ ".txt"
                b <- SD.doesFileExist fn
                unless b $ encodeFile fn (toJSON v)
saveTxs Nothing  _   = pure ()

listDirectory :: FilePath -> IO [FilePath]
listDirectory path = filter f <$> SD.getDirectoryContents path
  where f filename = filename /= "." && filename /= ".."

withCurrentDirectory :: FilePath  -- ^ Directory to execute in
                     -> IO a      -- ^ Action to be executed
                     -> IO a
withCurrentDirectory dir action =
  bracket SD.getCurrentDirectory SD.setCurrentDirectory $ \_ -> do
    SD.setCurrentDirectory dir
    action

loadTxs :: Maybe FilePath -> IO [[Tx]]
loadTxs (Just d) = do 
  fs <- listDirectory d
  css <- mapM readCall <$> mapM SD.makeRelativeToCurrentDirectory fs
  txs <- catMaybes <$> withCurrentDirectory d css
  putStrLn ("Loaded total of " ++ show (length txs) ++ " transactions from " ++ d)
  return txs
  where readCall f = decodeStrict <$> BS.readFile f

loadTxs Nothing  = pure [] 
