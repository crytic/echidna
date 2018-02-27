{-# LANGUAGE FlexibleContexts, KindSignatures #-}

module Main where

import Control.Monad.IO.Class

import Data.IORef

  {-
import EVM
import EVM.ABI      (AbiType, AbiValue(..), abiCalldata, abiValueType, encodeAbiValue)
import Echidna.Exec
import Echidna.Solidity
  -}

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- locked initially true
-- coin() -> locked to false
-- push() when locked -> false
-- push() when unlocked -> true, locked now true

-- model of contract states
data TurnstileState = Locked
                    | Unlocked
                    deriving (Eq, Ord, Show)

newtype Turnstile = Turnstile {
    unTurnstile :: IORef TurnstileState
  }

newTurnstile :: IO Turnstile
newTurnstile = Turnstile <$> newIORef Locked

insertCoin :: Turnstile -> IO ()
insertCoin (Turnstile ref) = atomicModifyIORef' ref $ \_ -> (Unlocked, ())

pushTurnstile :: Turnstile -> IO Bool
pushTurnstile (Turnstile ref) = atomicModifyIORef' ref $ \s ->
                                                           case s of
                                                             Locked -> (Locked, False)
                                                             Unlocked -> (Locked, True)

-- model of contract transitions
data ModelState (v :: * -> *) = TLocked
                              | TUnlocked
                              deriving (Eq, Ord, Show)

initialState :: ModelState v
initialState = TLocked

data Coin (v :: * -> *) = Coin
                        deriving (Eq, Show)

data Push (v :: * -> *) = Push
                        deriving (Eq, Show)

instance HTraversable Coin where
  htraverse _ Coin = pure Coin

instance HTraversable Push where
  htraverse _ Push = pure Push

-- commands representing state transitions that are passed to hedgehog
s_coin :: (Monad n, MonadIO m, MonadTest m) => Turnstile -> Command n m ModelState
s_coin ts =
  let gen _ = Just $ pure Coin
      execute Coin = liftIO (insertCoin ts)
  in
    Command gen execute [ Update $ \_ Coin _o -> TUnlocked
                        , Ensure $ \_ after Coin () -> do after === TUnlocked
                        ]

s_push_locked :: (Monad n, MonadIO m, MonadTest m) => Turnstile -> Command n m ModelState
s_push_locked ts =
  let gen state = case state of
                    TLocked -> Just $ pure Push
                    TUnlocked -> Nothing
      execute Push = do liftIO $ pushTurnstile ts
  in
    Command gen execute [ Require $ \s Push -> s == TLocked
                        , Update $ \_ Push _o -> TLocked
                        , Ensure $ \before after Push b -> do
                            before === TLocked
                            assert (not b)
                            after === TLocked
                        ]

s_push_unlocked :: (Monad n, MonadIO m, MonadTest m) => Turnstile -> Command n m ModelState
s_push_unlocked ts =
  let gen state = case state of
                    TUnlocked -> Just $ pure Push
                    TLocked -> Nothing
      execute Push = do liftIO $ pushTurnstile ts
  in
    Command gen execute [ Require $ \s Push -> s == TUnlocked
                        , Update $ \_ Push _o -> TLocked
                        , Ensure $ \before after Push b -> do
                            before === TUnlocked
                            assert b
                            after === TLocked
                        ]

-- the property, the actual hedgehog proposal
prop_turnstile :: Property
prop_turnstile = property $ do
    t <- liftIO newTurnstile
    ops <- forAll $ Gen.sequential (Range.linear 1 100) initialState [
      s_coin t
      , s_push_locked t
      , s_push_unlocked t
      ]
    executeSequential initialState ops

main :: IO ()
main = do
  r <- check $ prop_turnstile
  putStrLn $ show r
