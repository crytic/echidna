{-# LANGUAGE FlexibleContexts, KindSignatures #-}

module Main where

import Control.Monad.State.Strict

import Echidna.Exec
import Echidna.Solidity

import EVM
import EVM.ABI
import EVM.Concrete

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

data ModelState (v :: * -> *) = TLocked
                              | TUnlocked
                              deriving (Eq, Ord, Show)

initialState :: ModelState v
initialState = TLocked

data Coin (v :: * -> *) = Coin deriving (Eq, Show)

data Push (v :: * -> *) = Push deriving (Eq, Show)

instance HTraversable Coin where
  htraverse _ Coin = pure Coin

instance HTraversable Push where
  htraverse _ Push = pure Push

-- commands representing state transitions that are passed to hedgehog
s_coin :: (Monad n, MonadTest m, MonadState VM m) => Command n m ModelState
s_coin = Command (\_ -> Just $ pure Coin)
  (\Coin -> cleanUp >> execCall ("coin", []))
  [ Update $ \_ Coin _ -> TUnlocked
  , Ensure $ \_ s Coin _ -> s === TUnlocked
  ]

match :: VMResult -> Bool -> Bool
match (VMSuccess (B s)) b = s == encodeAbiValue (AbiBool b)
match _ _ = False

s_push_locked :: (Monad n, MonadTest m, MonadState VM m) => Command n m ModelState
s_push_locked = Command (\s -> if s == TLocked then Just $ pure Push else Nothing)
  (\Push -> cleanUp >> execCall ("push", []))
  [ Require $ \s Push -> s == TLocked
  , Update $ \_ Push _ -> TLocked 
  , Ensure $ \before after Push b -> do before === TLocked
                                        assert (match b False)
                                        after === TLocked
  ]

s_push_unlocked :: (Monad n, MonadTest m, MonadState VM m) => Command n m ModelState
s_push_unlocked = Command (\s -> if s == TUnlocked then Just $ pure Push else Nothing)
  (\Push -> cleanUp >> execCall ("push", []))
  [ Require $ \s Push -> s == TUnlocked
  , Update $ \_ Push _ -> TLocked 
  , Ensure $ \before after Push b -> do before === TUnlocked
                                        assert (match b True)
                                        after === TLocked
  ]

prop_turnstile :: Property
prop_turnstile = property $ do
  (v,_,_) <- loadSolidity "solidity/turnstile.sol"
  actions <- forAll $ Gen.sequential (Range.linear 1 100) initialState
    [s_coin, s_push_locked, s_push_unlocked]
  evalStateT (executeSequential initialState actions) v

main :: IO ()
main = check prop_turnstile >> pure ()
