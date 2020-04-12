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
import GHC.Word                   (Word32)

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.HashMap.Strict as M
import qualified Data.Text           as T

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

filterResults :: Maybe String -> [(T.Text, [T.Text])] -> [Word32] 
filterResults (Just c) rs = case lookup (T.pack c) rs of
                             Nothing -> filterResults Nothing rs
                             Just s -> map hashSig s 
filterResults Nothing rs = concatMap (map hashSig . snd) rs

data SlitherInfo = PayableInfo (T.Text, [T.Text]) 
                 | ConstantFunctionInfo (T.Text, [T.Text]) deriving (Show)

filterPayable :: [SlitherInfo] -> [(T.Text, [T.Text])]
filterPayable = map g . filter f
                 where f (PayableInfo _) = True
                       f _               = False
                       g (PayableInfo i) = i
                       g _               = error "fail in filterPayable"

filterConstantFunction :: [SlitherInfo] -> [(T.Text, [T.Text])]
filterConstantFunction = map g . filter f 
                 where f (ConstantFunctionInfo _) = True
                       f _               = False
                       g (ConstantFunctionInfo i) = i
                       g _               = error "fail in filterPayable"

-- Slither processing
runSlither :: (MonadIO m, MonadThrow m) => FilePath -> [String] -> m [SlitherInfo]
runSlither fp aa = let args = ["--ignore-compile", "--print", "echidna", "--json", "-"] ++ aa in do
  mp  <- liftIO $ findExecutable "slither"
  case mp of
   Nothing -> return [] 
   Just path -> liftIO $ do (ec, out, err) <- readCreateProcessWithExitCode (proc path $ args |> fp) {std_err = Inherit} ""
                            case ec of
                              ExitSuccess -> return $ procSlither out
                              ExitFailure _ -> throwM $ ProcessorFailure "slither" err

procSlither :: String -> [SlitherInfo]
procSlither r = case (decode . BSL.pack) r of
                 Nothing -> []
                 Just v  -> mresult "" v

-- parse result json
mresult :: T.Text -> Value -> [SlitherInfo]
mresult "description" (String x)  = case (decode . BSL.pack . T.unpack) x of
                                      Nothing -> []
                                      Just v  -> mpayable "" v ++ mcfuncs "" v

mresult _ (Object o) = concatMap (uncurry mresult) $ M.toList o
mresult _ (Array  a) = concatMap (mresult "") a
mresult _  _         = []

-- parse actual payable information
mpayable :: T.Text -> Value -> [SlitherInfo]
mpayable "payable" (Object o)  = map ( PayableInfo . second f) $ M.toList o
                                 where f (Array xs)            = concatMap f xs
                                       f (String "fallback()") = ["()"]
                                       f (String s)            = [s]
                                       f _                     = []
mpayable _ (Object o) = concatMap (uncurry mpayable) $ M.toList o
mpayable _ (Array  a) = concatMap (mpayable "") a
mpayable _  _         = []

-- parse actual constant functions information
mcfuncs :: T.Text -> Value -> [SlitherInfo]
mcfuncs "constant_functions" (Object o)  = map ( ConstantFunctionInfo . second f) $ M.toList o
                                 where f (Array xs)            = concatMap f xs
                                       f (String s)            = [s]
                                       f _                     = []
mcfuncs _ (Object o) = concatMap (uncurry mcfuncs) $ M.toList o
mcfuncs _ (Array  a) = concatMap (mcfuncs "") a
mcfuncs _  _         = []
