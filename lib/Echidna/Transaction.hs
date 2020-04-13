{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Echidna.Transaction where

import Prelude hiding (Word)

import Control.Lens
import Control.Monad (join, liftM2, liftM3, liftM5, unless)
import Control.Monad.Catch (MonadThrow, bracket)
import Control.Monad.Random.Strict (MonadRandom, getRandomR, uniform)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Strict (MonadState, State, evalStateT, runState, get, put)
import Data.Aeson (ToJSON(..), FromJSON(..), withText, defaultOptions, decodeStrict, encodeFile)
import Data.Aeson.TH (deriveJSON)
import Data.DoubleWord (Word256(..), Int256(..), Word160(..))
import Data.ByteString (ByteString)
import Data.Has (Has(..))
import Data.Hashable (hash)
import Data.Map (Map, toList)
import Data.Maybe (catMaybes)
import Data.List (intercalate)
import Text.Read (readMaybe)
import EVM hiding (value)
import EVM.ABI (abiCalldata, abiValueType, AbiValue(..), AbiType(..))
import EVM.Concrete (Word(..), w256)
import EVM.Types (Addr)

import qualified System.Directory as SD
import qualified Control.Monad.Fail as M (MonadFail(..))
import qualified Data.ByteString.Base16 as BS16
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as M
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Vector as V

import Echidna.ABI

-- | A transaction call is either a @CREATE@, a fully instrumented 'SolCall', or
-- an abstract call consisting only of calldata.
data TxCall = SolCreate   ByteString
            | SolCall     SolCall
            | SolCalldata ByteString
  deriving (Show, Ord, Eq)
makePrisms ''TxCall

-- | A transaction is either a @CREATE@ or a regular call with an origin, destination, and value.
-- Note: I currently don't model nonces or signatures here.
data Tx = Tx { _call  :: TxCall       -- | Call
             , _src   :: Addr         -- | Origin
             , _dst   :: Addr         -- | Destination
             , _gas'  :: Word         -- | Gas
             , _gasprice' :: Word     -- | Gas price
             , _value :: Word         -- | Value
             , _delay :: (Word, Word) -- | (Time, # of blocks since last call)
             } deriving (Eq, Ord, Show)

makeLenses ''Tx

data TxResult = Success
              | ErrorBalanceTooLow 
              | ErrorUnrecognizedOpcode
              | ErrorSelfDestruction
              | ErrorStackUnderrun
              | ErrorBadJumpDestination
              | ErrorRevert
              | ErrorNoSuchContract
              | ErrorOutOfGas
              | ErrorBadCheatCode
              | ErrorStackLimitExceeded
              | ErrorIllegalOverflow
              | ErrorQuery
              | ErrorStateChangeWhileStatic
              | ErrorInvalidMemoryAccess
              | ErrorCallDepthLimitReached
              | ErrorMaxCodeSizeExceeded
              | ErrorPrecompileFailure      deriving (Eq, Ord, Show)

$(deriveJSON defaultOptions ''TxResult)

getResult :: VMResult -> TxResult
getResult (VMSuccess _)                      = Success
getResult (VMFailure (BalanceTooLow _ _ ))   = ErrorBalanceTooLow
getResult (VMFailure (UnrecognizedOpcode _)) = ErrorUnrecognizedOpcode
getResult (VMFailure SelfDestruction )       = ErrorSelfDestruction
getResult (VMFailure StackUnderrun )         = ErrorStackUnderrun
getResult (VMFailure BadJumpDestination )    = ErrorBadJumpDestination
getResult (VMFailure (Revert _))             = ErrorRevert
getResult (VMFailure (NoSuchContract _))     = ErrorNoSuchContract
getResult (VMFailure (OutOfGas _ _))         = ErrorOutOfGas
getResult (VMFailure (BadCheatCode _))       = ErrorBadCheatCode
getResult (VMFailure StackLimitExceeded)     = ErrorStackLimitExceeded
getResult (VMFailure IllegalOverflow)        = ErrorIllegalOverflow
getResult (VMFailure (Query _))              = ErrorQuery
getResult (VMFailure StateChangeWhileStatic) = ErrorStateChangeWhileStatic
getResult (VMFailure InvalidMemoryAccess)    = ErrorInvalidMemoryAccess
getResult (VMFailure CallDepthLimitReached)  = ErrorCallDepthLimitReached
getResult (VMFailure (MaxCodeSizeExceeded _ _)) = ErrorMaxCodeSizeExceeded
getResult (VMFailure PrecompileFailure)      = ErrorPrecompileFailure

instance ToJSON Word256 where
  toJSON = toJSON . show

instance FromJSON Word256 where
  parseJSON = withText "Word256" $ maybe (M.fail "could not parse Word256") pure . readMaybe . T.unpack

instance ToJSON Int256 where
  toJSON = toJSON . show

instance FromJSON Int256 where
  parseJSON = withText "Int256" $ maybe (M.fail "could not parse Int256") pure . readMaybe . T.unpack

instance ToJSON Word160 where
  toJSON = toJSON . show

instance FromJSON Word160 where
  parseJSON = withText "Int160" $ maybe (M.fail "could not parse Word160") pure . readMaybe . T.unpack

instance ToJSON ByteString where
  toJSON = toJSON . show

instance FromJSON ByteString where
  parseJSON = withText "ByteString" $ maybe (M.fail "could not parse ByteString") pure . readMaybe . T.unpack

instance ToJSON Addr where
  toJSON = toJSON . show

instance ToJSON Word where
  toJSON = toJSON . show

instance FromJSON Word where
  parseJSON = withText "Word" $ maybe (M.fail "could not parse Word") pure . readMaybe . T.unpack

$(deriveJSON defaultOptions ''AbiType)
$(deriveJSON defaultOptions ''AbiValue)
$(deriveJSON defaultOptions ''TxCall)
$(deriveJSON defaultOptions ''Tx)

data TxConf = TxConf { _propGas       :: Word
                     -- ^ Gas to use evaluating echidna properties
                     , _txGas         :: Word
                     -- ^ Gas to use in generated transactions
                     , _maxGasprice   :: Word
                     -- ^ Maximum gasprice to be checked for a transaction
                     , _maxTimeDelay  :: Word
                     -- ^ Maximum time delay between transactions (seconds)
                     , _maxBlockDelay :: Word
                     -- ^ Maximum block delay between transactions
                     }

makeLenses 'TxConf

-- | Pretty-print some 'AbiCall'.
ppSolCall :: SolCall -> String
ppSolCall (t, vs) = (if t == "" then T.unpack "*fallback*" else T.unpack t) ++ "(" ++ intercalate "," (ppAbiValue <$> vs) ++ ")"

-- | Pretty-print some 'TxCall'
ppTxCall :: TxCall -> String
ppTxCall (SolCreate _)    = "<CREATE>"
ppTxCall (SolCall x)      = ppSolCall x
ppTxCall (SolCalldata x)  = BSC8.unpack $ "0x" <> BS16.encode x

-- | If half a tuple is zero, make both halves zero. Useful for generating delays, since block number
-- only goes up with timestamp
level :: (Num a, Eq a) => (a, a) -> (a, a)
level (elemOf each 0 -> True) = (0,0)
level x                       = x

-- | A contract is just an address with an ABI (for our purposes).
type ContractA = (Addr, NE.NonEmpty SolSignature)

-- | The world is made our of humans with an address, and a way to map contract
-- bytecodes to an ABI
data World = World { _senders         :: NE.NonEmpty Addr
                   , _bytecodeMapping :: M.HashMap ByteString (NE.NonEmpty SolSignature)
                   }
makeLenses ''World

-- | Given generators for an origin, destination, value, and function call, generate a call
-- transaction. Note: This doesn't generate @CREATE@s because I don't know how to do that at random.
genTxWith :: (MonadRandom m, MonadState x m, Has World x, MonadThrow m)
          => Map Addr Contract                        -- ^ List of contracts
          -> (NE.NonEmpty Addr -> m Addr)             -- ^ Sender generator
          -> (NE.NonEmpty ContractA -> m ContractA)   -- ^ Receiver generator
          -> (Addr -> ContractA -> m SolCall)         -- ^ Call generator
          -> m Word                                   -- ^ Gas generator
          -> m Word                                   -- ^ Gas price generator
          -> (Addr -> ContractA -> SolCall -> m Word) -- ^ Value generator
          -> m (Word, Word)                           -- ^ Delay generator
          -> m Tx
genTxWith m s r c g gp v t = use hasLens >>= \(World ss mm) ->
  let s' = s ss
      r' = r rs
      c' = join $ liftM2 c s' r'
      rs = NE.fromList . catMaybes $ mkR <$> toList m
      mkR = _2 (flip M.lookup mm . view (bytecode . to stripBytecodeMetadata))
  in
    ((liftM5 Tx (SolCall <$> c') s' (fst <$> r') g gp <*>) =<< liftM3 v s' r' c') <*> t

-- | Synthesize a random 'Transaction', not using a dictionary.
genTx :: forall m x y. (MonadRandom m, MonadReader x m, Has TxConf x, MonadState y m, Has World y, MonadThrow m)
      => Map Addr Contract
      -> m Tx
genTx m = use (hasLens :: Lens' y World) >>= evalStateT (genTxM m) . (defaultDict,)

-- | Generate a random 'Transaction' with either synthesis or mutation of dictionary entries.
genTxM :: (MonadRandom m, MonadReader x m, Has TxConf x, MonadState y m, Has GenDict y, Has World y, MonadThrow m)
  => Map Addr Contract
  -> m Tx
genTxM m = view hasLens >>= \(TxConf _ g maxGp t b) -> genTxWith
  m
  rElem rElem                                                                -- src and dst
  (const $ genInteractionsM . snd)                                           -- call itself
  (pure g) (inRange maxGp) (\_ _ _ -> pure 0)                                -- gas, gasprice, value
  (level <$> liftM2 (,) (inRange t) (inRange b))                             -- delay
     where inRange hi = w256 . fromIntegral <$> getRandomR (0 :: Integer, fromIntegral hi)

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

-- | Lift an action in the context of a component of some 'MonadState' to an action in the
-- 'MonadState' itself.
liftSH :: (MonadState a m, Has b a) => State b x -> m x
liftSH = stateST . runState . zoom hasLens
  -- This is the default state function written in terms of get and set:
  where stateST :: MonadState s m => (s -> (a, s)) -> m a
        stateST f = do s <- get
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
      SolCreate bc -> assign (env . contracts . at r) (Just $ initialContract (RuntimeCode bc) & set balance v) >> loadContract r
      SolCall cd  -> loadContract r >> state . calldata .= encode cd
      SolCalldata cd  -> loadContract r >> state . calldata .= cd
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
