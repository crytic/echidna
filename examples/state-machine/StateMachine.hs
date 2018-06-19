{-# LANGUAGE FlexibleContexts, KindSignatures #-}

module Main where

import Control.Monad.State.Strict (MonadState, evalStateT)
import Data.List (isSuffixOf)
import System.Directory (getDirectoryContents)

import Echidna.Exec (execCall)
import Echidna.Solidity (loadSolidity)

import EVM (VM, VMResult(..))
import EVM.ABI (AbiValue(..), encodeAbiValue)
import EVM.Concrete (Blob(..))

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
  (\Coin -> execCall ("coin", []))
  [ Update $ \_ Coin _ -> TUnlocked
  , Ensure $ \_ s Coin _ -> s === TUnlocked
  ]

match :: VMResult -> Bool -> Bool
match (VMSuccess (B s)) b = s == encodeAbiValue (AbiBool b)
match _ _ = False

s_push_locked :: (Monad n, MonadTest m, MonadState VM m) => Command n m ModelState
s_push_locked = Command (\s -> if s == TLocked then Just $ pure Push else Nothing)
  (\Push -> execCall ("push", []))
  [ Require $ \s Push -> s == TLocked
  , Update $ \_ Push _ -> TLocked 
  , Ensure $ \before after Push b -> do before === TLocked
                                        assert (match b False)
                                        after === TLocked
  ]

s_push_unlocked :: (Monad n, MonadTest m, MonadState VM m) => Command n m ModelState
s_push_unlocked = Command (\s -> if s == TUnlocked then Just $ pure Push else Nothing)
  (\Push -> execCall ("push", []))
  [ Require $ \s Push -> s == TUnlocked
  , Update $ \_ Push _ -> TLocked 
  , Ensure $ \before after Push b -> do before === TUnlocked
                                        assert (match b True)
                                        after === TLocked
  ]

prop_turnstile :: VM -> Property
prop_turnstile v = property $ do
  actions <- forAll $ Gen.sequential (Range.linear 1 100) initialState
    [s_coin, s_push_locked, s_push_unlocked]
  evalStateT (executeSequential initialState actions) v

check_turnstile :: FilePath -> FilePath -> IO Bool
check_turnstile dir fp = do putStrLn ("Checking " ++ fp ++ "...")
                            (v,_,_) <- loadSolidity (dir ++ "/" ++ fp) Nothing Nothing Nothing
                            check (prop_turnstile v)

main :: IO ()
main = let dir = "solidity/turnstile" in mapM_ (check_turnstile dir)
  =<< filter (isSuffixOf ".sol") <$> getDirectoryContents dir
