{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Echidna.Solidity where

import Control.Lens
import Control.Exception          (Exception)
import Control.Monad              (liftM2, mapM_, when, unless)
import Control.Monad.Catch        (MonadThrow(..))
import Control.Monad.IO.Class     (MonadIO(..))
import Control.Monad.Reader       (MonadReader)
import Control.Monad.State.Strict (execStateT)
import Data.Aeson                 (Value(..))
import Data.Foldable              (toList)
import Data.Has                   (Has(..))
import Data.List                  (find, partition, stripPrefix)
import Data.Maybe                 (isNothing, mapMaybe)
import Data.Monoid                ((<>))
import Data.Text                  (Text, isPrefixOf, pack, unpack)
import System.Process             (readCreateProcess, std_err, proc, StdStream(..))
import System.IO                  (openFile, IOMode(..))
import System.IO.Temp             (writeSystemTempFile)
import Text.Read                  (readMaybe)

import Echidna.ABI (SolSignature)
import Echidna.Exec (execTx)
import Echidna.Transaction (Tx(..), World(..))

import EVM hiding (contracts)
import EVM.ABI      (AbiValue(..))
import EVM.Exec     (vmForEthrunCreation)
import EVM.Solidity
import EVM.Types    (Addr)

import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict   as M

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

-- | Given a file, try to compile it and get a list of its contracts, throwing exceptions if necessary.
contracts :: (MonadIO m, MonadThrow m, MonadReader x m, Has SolConf x) => FilePath -> m [SolcContract]
contracts fp = do
  a <- view (hasLens . solcArgs)
  q <- view (hasLens . quiet)
  pure (a, q) >>= liftIO . solc >>= (\case
    Nothing -> throwM CompileFailure
    Just m  -> pure . toList $ fst m) where
      usual = ["--combined-json=bin-runtime,bin,srcmap,srcmap-runtime,abi,ast", fp]
      solc (a, q) = do
        stderr <- if q then UseHandle <$> openFile "/dev/null" WriteMode
                       else pure Inherit
        readSolc =<< writeSystemTempFile ""
                 =<< readCreateProcess (proc "solc" $ usual <> words a) {std_err = stderr} ""

-- | Given a file, an optional name and a list of all the 'SolcContract's in a file, try to load the
-- specified contract into a 'VM' usable for Echidna testing and extract an ABI and list of tests.
-- Throws exceptions if anything returned doesn't look usable for Echidna
loadSpecified :: (MonadIO m, MonadThrow m, MonadReader x m, Has SolConf x)
              => FilePath -> Maybe Text -> [SolcContract] -> m (VM, [SolSignature], [Text])
loadSpecified fp name cs = let ensure l e = if l == mempty then throwM e else pure () in do
  -- Pick contract to load
  c <- choose cs $ ((pack fp <> ":") <>) <$> name
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

loadSolidity :: (MonadIO m, MonadThrow m, MonadReader x m, Has SolConf x)
             => FilePath -> Maybe Text -> m (VM, [SolSignature], [Text])
loadSolidity fp name = contracts fp >>= loadSpecified fp name

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
extractConstants = concatMap $ getConstants . view contractAst where
  getConstants :: Value -> [AbiValue]
  getConstants (Object o) = concat . mapMaybe fromPair $ M.toList o
  getConstants (Array  a) = concatMap getConstants a
  getConstants _          = []

  fromPair ("type", (String s)) = let split = words $ unpack s in case split of
    "int_const"      : i : _ -> ints      <$> readMaybe i
    "literal_string" : l : _ -> (strs "") <$> stripPrefix "\\\"" l
    _                        -> Nothing
  fromPair _ = Nothing

  ints :: Integer -> [AbiValue]
  ints n = let l f = f <$> [8,16..256] <*> [fromIntegral n] in l AbiInt ++ l AbiUInt

  strs :: String -> String -> [AbiValue]
  strs _ ""                = []
  strs x (y : '\\':'"':"") = let s = reverse $ y : x in
    [AbiString, AbiBytes (length s), AbiBytesDynamic] <&> ($ (BS.pack s))
  strs x (y : ys)          = strs (y : x) ys
