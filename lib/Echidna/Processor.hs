{-# LANGUAGE LambdaCase #-}

module Echidna.Processor where

import Control.Arrow              (second)
import Control.Lens
import Control.Monad.IO.Class     (MonadIO(..))
import Control.Exception          (Exception)
import Control.Monad.Catch        (MonadThrow(..))
import Data.Aeson                 (decode, Value(..))
import System.Directory           (findExecutable)
import System.Process             (StdStream(..), readCreateProcessWithExitCode, proc, std_err)
import System.Exit                (ExitCode(..))

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.HashMap.Strict as M
import qualified Data.Text           as T

import Echidna.Config
import Echidna.Solidity
import Echidna.Transaction
import Echidna.ABI (hashSig)

-- | Things that can go wrong trying to run a processor. Read the 'Show'
-- instance for more detailed explanations.
data ProcException = ProcessorFailure String String
                   | ProcessorNotFound String

instance Show ProcException where
  show = \case
    ProcessorFailure p e     -> "Error running " ++ p ++ ": " ++ e
    ProcessorNotFound p      -> "Cannot find processor " ++ p

instance Exception ProcException

process :: (MonadIO m, MonadThrow m) => FilePath -> Maybe String -> EConfig -> m EConfig
process f _ e = do 
                 rs <- runSlither f (e ^. sConf ^. cryticArgs)
                 let ps = concatMap (map hashSig . snd) rs
                 return $ e & xConf . payableSigs .~ ps

type SlitherInfo = PayableInfo
type PayableInfo = [(T.Text, [T.Text])]

-- Slither processing
runSlither :: (MonadIO m, MonadThrow m) => FilePath -> [String] -> m SlitherInfo
runSlither fp aa = let args = ["--print", "echidna", "--json", "-"] ++ aa in do
  mp  <- liftIO $ findExecutable "slither"
  case mp of
   Nothing -> return [] 
   Just path -> liftIO $ do (ec, out, err) <- readCreateProcessWithExitCode (proc path $ args |> fp) {std_err = Inherit} ""
                            case ec of
                              ExitSuccess -> return $ procSlither out
                              ExitFailure _ -> throwM $ ProcessorFailure "slither" err

procSlither :: String -> SlitherInfo
procSlither r = case (decode . BSL.pack) r of
                 Nothing -> []
                 Just v  -> mresult "" v  

-- parse result json
mresult :: T.Text -> Value -> PayableInfo
mresult "description" (String x)  = case (decode . BSL.pack . T.unpack) x of
                                      Nothing -> []
                                      Just v  -> mpayable "" v  

mresult _ (Object o) = concatMap (uncurry mresult) $ M.toList o
mresult _ (Array  a) = concatMap (mresult "")    a
mresult _  _         = []

-- parse actual payable information
mpayable :: T.Text -> Value -> PayableInfo
mpayable "payable" (Object o)  = map (second f) $ M.toList o
                                 where f (Array xs)            = concatMap f xs
                                       f (String "fallback()") = ["()"]
                                       f (String s)            = [s]
                                       f _                     = []
mpayable _ (Object o) = concatMap (uncurry mpayable) $ M.toList o
mpayable _ (Array  a) = concatMap (mpayable "")    a
mpayable _  _         = []
