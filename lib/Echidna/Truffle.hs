{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Echidna.Truffle where

import Control.Lens
import Control.Exception          (Exception)
import Control.Monad              (liftM2, mapM_, when, unless)
import Control.Monad.Catch        (MonadThrow(..))
import Control.Monad.IO.Class     (MonadIO(..))
import Control.Monad.Reader       (MonadReader)
import Control.Monad.State.Strict (execStateT)
import Data.Aeson                 --(Value (..), FromJSON(..))
import Data.Foldable              (toList, fold)
import Data.Has                   (Has(..))
import Data.List                  (find, partition)

import Data.Maybe                 (isNothing, fromMaybe)
import Data.Monoid                ((<>))
import Data.Text                  (Text, isPrefixOf, pack, unpack, intercalate)
import Data.Text.Encoding   (encodeUtf8)
import System.Process             (readCreateProcess, std_err, proc, StdStream(..))
import System.IO                  (openFile, IOMode(..))
import System.IO.Temp             (writeSystemTempFile)

import Echidna.ABI (SolSignature)
import Echidna.Exec (execTx)
import Echidna.Transaction (Tx(..), World(..))
import Echidna.Solidity (SolConf(..), sender)

import EVM hiding (contracts)
import EVM.Exec     (vmForEthrunCreation)
import EVM.Solidity
import EVM.Types    (Addr)
import EVM.ABI      (AbiType, AbiValue(..), parseTypeName)
import EVM.Keccak   (abiKeccak)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS16
import qualified Data.Yaml as Y
import qualified Data.Map.Strict        as Map

data RawMethodIO = RawMethodIO { _metname :: String, _mettype :: String } deriving (Show)
data RawMethod = RawMethod { _inputs :: [RawMethodIO],  _abiname :: String, _outputs :: [RawMethodIO], _abitype :: String } deriving (Show)
data Artifact = Artifact { _contractName :: String
                         , _bytecode     :: String
                         , _abi          :: [RawMethod]
                         } deriving (Show)

force :: String -> Maybe a -> a
force s = fromMaybe (error s)

signature (RawMethod is n _ t) =
  case t of
    "fallback" -> "<fallback>"
    _ ->
      fold [
        (if (null n) then "<constructor>" else (pack n)), 
        "(",
        intercalate ","
          (map (pack . _mettype) is),
        ")"
      ]

parseMethodIO (RawMethodIO n t) = (pack n, force  "internal error: method type" (parseTypeName $ pack t))

parseMethod :: RawMethod -> Method 
parseMethod r@(RawMethod is n [] t) = Method Nothing (map parseMethodIO is) (pack n) (signature r)
parseMethod r@(RawMethod is n [o] t) = Method (Just (parseMethodIO o)) (map parseMethodIO is) (pack n) (signature r)

toCode :: String -> BS.ByteString
toCode = fst . BS16.decode . encodeUtf8 . pack . strip0x

strip0x ('0' : 'x' : s) = s
strip0x _               = error "invalid bytecode"

parseContract :: Artifact -> SolcContract 
parseContract (Artifact n b abi) = let abi' = map parseMethod abi
                                       abiMap = Map.fromList (map (\m@(Method _ _ _ s) -> ((abiKeccak . encodeUtf8) s, m)) abi') 
                                       eventMap = Map.empty 
                                    in SolcContract 0 0 (toCode b) (toCode "0x") (pack n) [] abiMap eventMap Empty Empty Null 

instance FromJSON RawMethodIO where
  parseJSON (Object v) =
     RawMethodIO <$> (v .:? "name" .!= "")
                 <*> (v .:? "type" .!= error "no method type")
  parseJSON _ = parseJSON (Object mempty)

instance FromJSON RawMethod where
  parseJSON (Object v) =
    RawMethod <$> (v .:? "inputs" .!= error "no inputs")
              <*> (v .:? "name" .!= "methodName")
              <*> (v .:? "outputs" .!= [])
              <*> (v .:? "type" .!= "methodType")
  parseJSON _ = parseJSON (Object mempty)


instance FromJSON Artifact where
  parseJSON (Object v) =
    Artifact <$> (v .:? "contractName" .!= "contractName")
             <*> (v .:? "bytecode" .!= "bytecode")
             <*> (v .:? "abi" .!= [])
  parseJSON _ = parseJSON (Object mempty)

readTruffle :: FilePath -> IO Artifact
readTruffle fp = do x <- (eitherDecode <$> LBS.readFile fp) :: IO (Either String Artifact)
                    case x of 
                      Right art -> return art
                      Left e  -> error e  

loadTruffle :: (MonadIO m, MonadThrow m, MonadReader x m, Has SolConf x)
             => FilePath -> Maybe Text -> m (VM, [SolSignature], [Text])
loadTruffle fp name = do
                       art <- liftIO $ readTruffle fp
                       liftIO $ print art
                       (SolConf ca d _ pref _ _) <- view hasLens
                       let c = parseContract art
                           bc = c ^. creationCode
                           blank = vmForEthrunCreation bc
                           abi = liftM2 (,) (view methodName) (fmap snd . view methodInputs) <$> toList (c ^. abiMap)
                           (tests, funs) = partition (isPrefixOf pref . fst) abi
                       --mapM_ (uncurry ensure) [(abi, NoFuncs), (tests, NoTests), (funs, OnlyTests)] -- ABI checks
                       --ensure bc (NoBytecode $ c ^. contractName)                                   -- Bytecode check
                       case find (not . null . snd) tests of
                            Just (t,_) -> undefined --throwM $ TestArgsFound t
                            Nothing    -> (, funs, fst <$> tests) <$> execStateT (execTx $ Tx (Right bc) d ca 0) blank

-- | Basically loadTruffle, but prepares the results to be passed directly into
-- a testing function.
loadTruffleTests :: (MonadIO m, MonadThrow m, MonadReader x m, Has SolConf x)
             => FilePath -> Maybe Text -> m (VM, World, [(Text, Addr)])
loadTruffleTests fp name = do
  (v, a, ts) <- loadTruffle fp name
  s <- view $ hasLens . sender
  let r = v ^. state . contract
      w = World s [(r, a)]
  return (v, w, zip ts $ repeat r)

