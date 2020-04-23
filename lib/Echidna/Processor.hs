{-# LANGUAGE LambdaCase #-}

module Echidna.Processor where

import Control.Arrow              (second)
import Control.Lens
import Control.Monad.IO.Class     (MonadIO(..))
import Control.Exception          (Exception)
import Control.Monad.Catch        (MonadThrow(..))
import Data.Aeson                 (decode, Value(..))
import Data.Text                  (Text, pack, unpack)
import System.Directory           (findExecutable)
import System.Process             (StdStream(..), readCreateProcessWithExitCode, proc, std_err)
import System.Exit                (ExitCode(..))

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.HashMap.Strict as M

import Echidna.Types.Signature (ContractName, FunctionName, FunctionHash)
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

-- | This function is used to filter the lists of function names according to the supplied
-- contract name (if any) and returns a list of hashes 
filterResults :: Maybe String -> [(ContractName, [FunctionName])] -> [FunctionHash] 
filterResults (Just c) rs = 
  case lookup (pack c) rs of
    Nothing -> filterResults Nothing rs
    Just s -> map hashSig s 

filterResults Nothing rs = concatMap (map hashSig . snd) rs

data SlitherInfo = PayableInfo (ContractName, [FunctionName]) 
                 | ConstantFunctionInfo (ContractName, [FunctionName])
                 | AssertFunction (ContractName, [FunctionName])
                 | ConstantValue (ContractName, Text) -- Not used right now, it will change soon
                 | GenerationGraph (ContractName, FunctionName, [FunctionName]) deriving (Show)

filterPayable :: [SlitherInfo] -> [(ContractName, [FunctionName])]
filterPayable = map g . filter f
  where f (PayableInfo _) = True
        f _               = False
        g (PayableInfo i) = i
        g _               = error "fail in filterPayable"

filterAssert :: [SlitherInfo] -> [(ContractName, [FunctionName])]
filterAssert = map g . filter f
  where f (AssertFunction _) = True
        f _               = False
        g (AssertFunction i) = i
        g _               = error "fail in filterAssert"

filterConstantFunction :: [SlitherInfo] -> [(ContractName, [FunctionName])]
filterConstantFunction = map g . filter f 
  where f (ConstantFunctionInfo _) = True
        f _               = False
        g (ConstantFunctionInfo i) = i
        g _               = error "fail in filterConstantFunction"

filterGenerationGraph :: [SlitherInfo] -> [(ContractName, FunctionName, [FunctionName])]
filterGenerationGraph = map g . filter f
  where f (GenerationGraph _) = True
        f _                   = False
        g (GenerationGraph i) = i
        g _                   = error "fail in filterGenerationGraph"

-- Slither processing
runSlither :: (MonadIO m, MonadThrow m) => FilePath -> [String] -> m [SlitherInfo]
runSlither fp aa = let args = ["--ignore-compile", "--print", "echidna", "--json", "-"] ++ aa in do
  mp <- liftIO $ findExecutable "slither"
  case mp of
    Nothing -> return [] 
    Just path -> liftIO $ do 
                             (ec, out, _) <- readCreateProcessWithExitCode (proc path $ args |> fp) {std_err = Inherit} ""
                             case ec of
                               ExitSuccess -> return $ procSlither out
                               ExitFailure _ -> return [] --throwM $ ProcessorFailure "slither" err

procSlither :: String -> [SlitherInfo]
procSlither r = case (decode . BSL.pack) r of
                  Nothing -> []
                  Just v  -> mresult "" v

-- parse result json
mresult :: Text -> Value -> [SlitherInfo]
mresult "description" (String x)  = case (decode . BSL.pack . unpack) x of
                                      Nothing -> []
                                      Just v  -> mpayable "" v ++ mcfuncs "" v ++ mggraph "" v ++ mconsts "" v ++ massert "" v

mresult _ (Object o) = concatMap (uncurry mresult) $ M.toList o
mresult _ (Array  a) = concatMap (mresult "") a
mresult _  _         = []

-- parse actual payable information
mpayable :: Text -> Value -> [SlitherInfo]
mpayable "payable" (Object o) = map ( PayableInfo . second f) $ M.toList o
  where f (Array xs)            = concatMap f xs
        f (String "fallback()") = ["()"]
        f (String s)            = [s]
        f _                     = []

mpayable _ (Object o) = concatMap (uncurry mpayable) $ M.toList o
mpayable _ (Array  a) = concatMap (mpayable "") a
mpayable _  _         = []

-- parse actual assert information
massert :: Text -> Value -> [SlitherInfo]
massert "assert" (Object o) = map ( AssertFunction . second f) $ M.toList o
  where f (Array xs)            = concatMap f xs
        f (String "fallback()") = ["()"]
        f (String s)            = [s]
        f _                     = []

massert _ (Object o) = concatMap (uncurry massert) $ M.toList o
massert _ (Array  a) = concatMap (massert "") a
massert _  _         = []

-- parse actual constant functions information
mcfuncs :: Text -> Value -> [SlitherInfo]
mcfuncs "constant_functions" (Object o)  = ConstantFunctionInfo . second f <$> M.toList o
  where f (Array xs)            = concatMap f xs
        f (String s)            = [s]
        f _                     = []

mcfuncs _ (Object o) = concatMap (uncurry mcfuncs) $ M.toList o
mcfuncs _ (Array  a) = concatMap (mcfuncs "") a
mcfuncs _  _         = []

-- parse actual constant functions information
mconsts :: Text -> Value -> [SlitherInfo]
mconsts "constants_used" (Object o) = concatMap (uncurry mconsts') $ M.toList o
mconsts _ (Object o) = concatMap (uncurry mconsts) $ M.toList o
mconsts _ (Array  a) = concatMap (mconsts "") a
mconsts _  _         = []

mconsts' :: Text -> Value -> [SlitherInfo]
mconsts' _ (Object o) = case (M.lookup "value" o, M.lookup "type" o) of
                         (Just v, Just (String t)) -> [ConstantValue (pack $ show v, t)]
                         (Nothing, Nothing)        -> concatMap (uncurry mconsts') $ M.toList o
                         _                         -> error "invalid JSON formatting parsing constants"

mconsts' _ (Array  a) = concatMap (mconsts' "") a
mconsts' _  _         = []


-- parse actual generation graph
mggraph :: Text -> Value -> [SlitherInfo]
mggraph "functions_relations" (Object o)  = concatMap f $ M.toList o
                                 where f (c, Object o1) = map (\(m,v) -> GenerationGraph (c, m, mggraph' "" v)) $ M.toList o1
                                       f _              = []

mggraph _ (Object o) = concatMap (uncurry mggraph) $ M.toList o
mggraph _ (Array  a) = concatMap (mggraph "") a
mggraph _  _         = []

mggraph' :: Text -> Value -> [Text]
mggraph' "impacts" (Array a) = concatMap f a
  where f (Array xs)            = concatMap f xs
        f (String "fallback()") = ["()"]
        f (String s)            = [s]
        f _                     = []

mggraph' _ (Object o) = concatMap (uncurry mggraph') $ M.toList o
mggraph' _ (Array  a) = concatMap (mggraph' "") a
mggraph' _  _         = []
