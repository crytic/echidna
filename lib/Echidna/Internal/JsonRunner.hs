{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module Echidna.Internal.JsonRunner (
    checkParallelJson
  ) where

--import           Control.Monad (forM_)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson    (ToJSON, encode)
import           Data.ByteString.Lazy.Char8 (unpack)
import           Data.Maybe    (mapMaybe)   
import           GHC.Generics

import           Hedgehog.Internal.Config
import           Hedgehog.Internal.Gen (runDiscardEffect, runGenT)
import           Hedgehog.Internal.Property
  (Failure(..), Group(..), Property(..), PropertyT(..),
    PropertyConfig(..), ShrinkLimit(..), ShrinkRetries(..),
    Log(..), Diff(..), runTestT, unPropertyName)
import           Hedgehog.Internal.Queue
import           Hedgehog.Internal.Report
  (FailedAnnotation(..), FailureReport(..), Result(..), ShrinkCount(..))
import           Hedgehog.Internal.Runner (RunnerConfig(..))
import           Hedgehog.Internal.Seed (Seed)
import qualified Hedgehog.Internal.Seed as Seed
import           Hedgehog.Internal.Source
import           Hedgehog.Internal.Tree (Node(..), Tree(..), runTree)
import           Hedgehog.Range (Size)


data JsonOutput = JsonOutput {
    propName :: !String
  , propTrue :: !Bool
  , propCall :: !(Maybe [String])
  } deriving (Generic, Show)

instance ToJSON JsonOutput

checkParallelJson :: MonadIO m => Group -> m Bool
checkParallelJson =
  checkGroup
    RunnerConfig {
        runnerWorkers =
          Nothing
      , runnerColor =
          Nothing
      , runnerVerbosity =
          Nothing
      }

checkGroup :: MonadIO m => RunnerConfig -> Group -> m Bool
checkGroup config (Group _ props) =
  liftIO $ do
    n <- resolveWorkers (runnerWorkers config)

    updateNumCapabilities (n + 2)

    --verbosity <- resolveVerbosity (runnerVerbosity config)
    _ <- runTasks n props st noop noop $ \(name, prop) -> do
      result <- checkProp 0 prop
      putStrLn $ unpack $ encode (format name result)
      pure ()
    
    --output JSON here
    --forM_ rs $ (putStrLn . unpack . encode)

    pure True

  where st _ _ (name,prop) = pure (name,prop)
        noop = const $ pure ()
        failVals (FailureReport _ _ _ xs _ _ _ _) = map (\(FailedAnnotation _ v) -> v) xs
        format n r  = let name = unPropertyName n in
          case r of
            OK       -> JsonOutput { propName = name, propTrue = True, propCall = Nothing }
            GaveUp   -> JsonOutput { propName = name, propTrue = False, propCall = Nothing }
            Failed s -> JsonOutput { propName = name, propTrue = False, propCall = Just (failVals s) }

checkProp :: Size -> Property -> IO Result
checkProp size0 (Property conf test) = Seed.random >>= loop (0 :: Integer) (0 :: Integer) size0
  where loop !tests !discards !size !seed =
          if size > 99 then
            -- size has reached limit, reset to 0
            loop tests discards 0 seed

          else if tests >= fromIntegral (propertyTestLimit conf) then
            -- we've hit the test limit, test was successful
            pure $ OK
          
          else if discards >= fromIntegral (propertyDiscardLimit conf) then
            -- we've hit the discard limit, give up
            pure $ GaveUp

          else
            case Seed.split seed of
              (s0, s1) -> do
                node@(Node x _) <-
                  runTree . runDiscardEffect $ runGenT size s0 . runTestT $ unPropertyT test
                case x of
                  Nothing ->
                    loop tests (discards + 1) (size + 1) s1
  
                  Just (Left _, _) ->
                    takeSmallest
                      size
                      seed
                      0
                      (propertyShrinkLimit conf)
                      (propertyShrinkRetries conf)
                      node

                  Just (Right (), _) ->
                    loop (tests + 1) discards (size + 1) s1

takeSmallest ::
     Size
  -> Seed
  -> ShrinkCount
  -> ShrinkLimit
  -> ShrinkRetries
  -> Node IO (Maybe (Either Failure (), [Log]))
  -> IO Result
takeSmallest size seed shrinks slimit retries = \case
  Node Nothing _ ->
    pure GaveUp

  Node (Just (x, w)) xs ->
    case x of
      Left (Failure loc err mdiff) -> do
        let failure = mkFailure size seed shrinks loc err mdiff (reverse w)

        if shrinks >= fromIntegral slimit then
          -- if we've hit the shrink limit, don't shrink any further
          pure $ Failed failure

        else
          findM xs (Failed failure) $ \m -> do
            o <- runTreeN retries m
            if isFailure o then
              Just <$> takeSmallest size seed (shrinks + 1) slimit retries o
            else
              return Nothing

      Right () ->
        return OK

isFailure :: Node m (Maybe (Either x a, b)) -> Bool
isFailure = \case
  Node (Just (Left _, _)) _ ->
    True
  _ ->
    False

isSuccess :: Node m (Maybe (Either x a, b)) -> Bool
isSuccess =
  not . isFailure

runTreeN ::
     Monad m
  => ShrinkRetries
  -> Tree m (Maybe (Either x a, b))
  -> m (Node m (Maybe (Either x a, b)))
runTreeN n m = do
  o <- runTree m
  if n > 0 && isSuccess o then
    runTreeN (n - 1) m
  else
    pure o

findM :: Monad m => [a] -> b -> (a -> m (Maybe b)) -> m b
findM xs0 def p =
  case xs0 of
    [] ->
      return def
    x0 : xs ->
      p x0 >>= \m ->
        case m of
          Nothing ->
            findM xs def p
          Just x ->
            return x

takeAnnotation :: Log -> Maybe FailedAnnotation
takeAnnotation = \case
  Annotation loc val ->
    Just $ FailedAnnotation loc val
  _ ->
    Nothing

takeFootnote :: Log -> Maybe String
takeFootnote = \case
  Footnote x ->
    Just x
  _ ->
    Nothing

mkFailure ::
     Size
  -> Seed
  -> ShrinkCount
  -> Maybe Span
  -> String
  -> Maybe Diff
  -> [Log]
  -> FailureReport
mkFailure size seed shrinks location message diff logs =
  let
    inputs =
      mapMaybe takeAnnotation logs

    footnotes =
      mapMaybe takeFootnote logs
  in
    FailureReport size seed shrinks inputs location message diff footnotes
