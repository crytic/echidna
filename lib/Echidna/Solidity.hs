{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Echidna.Solidity where

import Control.Lens
import Control.Exception          (Exception)
import Control.Monad              (liftM2, mapM_, when, unless)
import Control.Monad.Catch        (MonadThrow(..))
import Control.Monad.IO.Class     (MonadIO(..))
import Control.Monad.Reader       (MonadReader)
import Control.Monad.State.Strict (execStateT)
import Data.Aeson                 (Value(..))
import Data.ByteString.Lens       (packedChars)
import Data.Foldable              (toList)
import Data.Has                   (Has(..))
import Data.List                  (find, nub, partition)
import Data.List.Lens             (prefixed, suffixed)
import Data.Maybe                 (isNothing)
import Data.Monoid                ((<>))
import Data.Text                  (Text, isPrefixOf, pack, unpack)
import Data.Text.Lens             (unpacked)
import Data.Text.Read             (decimal, {-hexadecimal-})
import System.Process             (readCreateProcess, std_err, proc, StdStream(..))
import System.IO                  (openFile, IOMode(..))
import System.IO.Temp             (writeSystemTempFile)
import System.FilePath.Posix       (takeExtension)

import Echidna.ABI         (SolSignature)
import Echidna.Exec        (execTx)
import Echidna.Transaction (Tx(..), World(..))

import EVM hiding (contracts)
import EVM.ABI      (AbiValue(..))
import EVM.Exec     (vmForEthrunCreation)
import EVM.Solidity
import EVM.Types    (Addr)

import qualified Data.ByteString     as BS
import qualified Data.HashMap.Strict as M
import qualified Data.Text           as T

-- | Things that can go wrong trying to load a Solidity file for Echidna testing. Read the 'Show'
-- instance for more detailed explanations.
data SolException = BadAddr Addr
                  | CompileFailure
                  | NoContracts
                  | TestArgsFound Text
                  | ContractNotFound Text
                  | NoBytecode Text
                  | NoFuncs
                  | NoTests
                  | OnlyTests

instance Show SolException where
  show = \case
    BadAddr a            -> "No contract at " ++ show a ++ " exists"
    CompileFailure       -> "Couldn't compile given file"
    NoContracts          -> "No contracts found in given file"
    (ContractNotFound c) -> "Given contract " ++ show c ++ " not found in given file"
    (TestArgsFound t)    -> "Test " ++ show t ++ " has arguments, aborting"
    (NoBytecode t)       -> "No bytecode found for contract " ++ show t
    NoFuncs              -> "ABI is empty, are you sure your constructor is right?"
    NoTests              -> "No tests found in ABI"
    OnlyTests            -> "Only tests and no public functions found in ABI"

instance Exception SolException

-- | Configuration for loading Solidity for Echidna testing.
data SolConf = SolConf { _contractAddr :: Addr   -- ^ Contract address to use
                       , _deployer     :: Addr   -- ^ Contract deployer address to use
                       , _sender       :: [Addr] -- ^ Sender addresses to use
                       , _prefix       :: Text   -- ^ Function name prefix used to denote tests
                       , _solcArgs     :: String -- ^ Args to pass to @solc@
                       , _quiet        :: Bool   -- ^ Suppress @solc@ output, errors, and warnings
                       }
makeLenses ''SolConf

-- | Given a file, use its extenstion to check if it is a precompiled contract or try to compile it and
-- get a list of its contracts, throwing exceptions if necessary.
contracts :: (MonadIO m, MonadThrow m, MonadReader x m, Has SolConf x) => FilePath -> m [SolcContract]
contracts fp = do
  a <- view (hasLens . solcArgs)
  q <- view (hasLens . quiet)
  pure (a, q) >>= liftIO . solc >>= (\case
    Nothing -> throwM CompileFailure
    Just m  -> pure . toList $ fst m) where
      usual = ["--combined-json=bin-runtime,bin,srcmap,srcmap-runtime,abi,ast,compact-format", fp]
      solc (a, q) = do
        if (takeExtension fp == ".json")
        then readSolc fp
        else do
              stderr <- if q then UseHandle <$> openFile "/dev/null" WriteMode
                        else pure Inherit
              readSolc =<< writeSystemTempFile ""
                       =<< readCreateProcess (proc "solc" $ usual <> words a) {std_err = stderr} ""

-- | Given an optional contract name and a list of 'SolcContract's, try to load the specified
-- contract, or, if not provided, the first contract in the list, into a 'VM' usable for Echidna
-- testing and extract an ABI and list of tests. Throws exceptions if anything returned doesn't look
-- usable for Echidna. NOTE: Contract names passed to this function should be prefixed by the
-- filename their code is in, plus a colon.
loadSpecified :: (MonadIO m, MonadThrow m, MonadReader x m, Has SolConf x)
              => Maybe Text -> [SolcContract] -> m (VM, [SolSignature], [Text])
loadSpecified name cs = let ensure l e = if l == mempty then throwM e else pure () in do
  -- Pick contract to load
  c <- choose cs name
  q <- view (hasLens . quiet)
  liftIO $ do
    when (isNothing name && length cs > 1) $
      putStrLn "Multiple contracts found in file, only analyzing the first"
    unless q . putStrLn $ "Analyzing contract: " <> unpack (c ^. contractName)

  -- Local variables
  (SolConf ca d _ pref _ _) <- view hasLens
  let bc = c ^. creationCode
      blank = vmForEthrunCreation bc
      abi = liftM2 (,) (view methodName) (fmap snd . view methodInputs) <$> toList (c ^. abiMap)
      (tests, funs) = partition (isPrefixOf pref . fst) abi

  -- Make sure everything is ready to use, then ship it
  mapM_ (uncurry ensure) [(abi, NoFuncs), (tests, NoTests), (funs, OnlyTests)] -- ABI checks
  ensure bc (NoBytecode $ c ^. contractName)                                   -- Bytecode check
  case find (not . null . snd) tests of
    Just (t,_) -> throwM $ TestArgsFound t                                     -- Test args check
    Nothing    -> (, funs, fst <$> tests) <$> execStateT (execTx $ Tx (Right bc) d ca 0) blank

  where choose []    _        = throwM NoContracts
        choose (c:_) Nothing  = return c
        choose _     (Just n) = maybe (throwM $ ContractNotFound n) pure $
                                      find ((n ==) . view contractName) cs

-- | Given a file and an optional contract name, compile the file as solidity, then, if a name is
-- given, try to fine the specified contract (assuming it is in the file provided), otherwise, find
-- the first contract in the file. Take said contract and return an initial VM state with it loaded,
-- its ABI (as 'SolSignature's), and the names of its Echidna tests. NOTE: unlike 'loadSpecified',
-- contract names passed here don't need the file they occur in specified.
loadSolidity :: (MonadIO m, MonadThrow m, MonadReader x m, Has SolConf x)
             => FilePath -> Maybe Text -> m (VM, [SolSignature], [Text])
loadSolidity fp name = contracts fp >>= loadSpecified ((pack fp <>) <$> name)

-- | Given the results of 'loadSolidity', assuming a single-contract test, get everything ready
-- for running a 'Campaign' against the tests found.
prepareForTest :: (MonadReader x m, Has SolConf x)
               => (VM, [SolSignature], [Text]) -> m (VM, World, [(Text, Addr)])
prepareForTest (v, a, ts) = let r = v ^. state . contract in
  view (hasLens . sender) <&> \s -> (v, World s [(r, a)], zip ts $ repeat r)

-- | Basically loadSolidity, but prepares the results to be passed directly into
-- a testing function.
loadSolTests :: (MonadIO m, MonadThrow m, MonadReader x m, Has SolConf x)
             => FilePath -> Maybe Text -> m (VM, World, [(Text, Addr)])
loadSolTests fp name = loadSolidity fp name >>= prepareForTest

-- | Given a list of 'SolcContract's, try to parse out string and integer literals
extractConstants :: [SolcContract] -> [AbiValue]
extractConstants = nub . concatMap (constants "" . view contractAst) where
  -- Tools for parsing numbers and quoted strings from 'Text'
  as f     = preview $ to f . _Right . _1
  --asAddr x = as hexadecimal =<< T.stripPrefix "0x" x
  asQuoted = preview $ unpacked . prefixed "\"" . suffixed "\"" . packedChars
  -- We need this because sometimes @solc@ emits a json string with a type, then a string
  -- representation of some value of that type. Why is this? Unclear. Anyway, this lets us match
  -- those cases like regular strings
  literal t f = \case String (T.words -> ((^? only t) -> m) : y : _) -> m *> f y
                      _                                              -> Nothing
  -- 'constants' takes a property name and its 'Value', then tries to find solidity literals
  -- CASE ONE: we're looking at a big object with a bunch of little objects, recurse
  constants _ (Object o) = concatMap (uncurry constants) $ M.toList o
  constants _ (Array  a) = concatMap (constants "")        a
  -- CASE TWO: we're looking at a @type@ or @value@ object, try to parse it
  -- 2.1: We're looking at a @value@ starting with "0x", which is how solc represents addresses
  --      @value: "0x123"@ ==> @[AbiAddress 291]@
  constants "value" (String                   ((as decimal) -> Just i)) = 
     let l f = f <$> [8,16..256] <*> [fromIntegral (i :: Integer)] in l AbiInt ++ l AbiUInt ++ [AbiAddress (fromIntegral i)]
  -- 2.2: We're looking at something of the form @type: int_const [...]@, an integer literal
  --      @type: "int_const 123"@ ==> @[AbiUInt 8 123, AbiUInt 16 123, ... AbiInt 256 123]@
  --constants "type"  (literal "int_const"      (as decimal) -> Just i) =
  --  let l f = f <$> [8,16..256] <*> [fromIntegral (i :: Integer)] in l AbiInt ++ l AbiUInt
  -- 2.3: We're looking at something of the form @type: literal_string "[...]"@, a string literal
  --      @type: "literal_string \"123\""@ ==> @[AbiString "123", AbiBytes 3 "123"...]@
  constants "type"  (literal "literal_string" asQuoted     -> Just b) =
   let size = BS.length b in  
   ([AbiString, AbiBytesDynamic] <&> ($ b)) ++
   map (\n -> AbiBytes n (BS.append b (BS.replicate (n - size) 0))) [size .. 32]
  -- CASE THREE: we're at a leaf node with no constants
  constants _  _ = []
