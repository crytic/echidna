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
import Data.DoubleWord            (Int256, Word256)
import Data.Foldable              (toList)
import Data.Has                   (Has(..))
import Data.List                  (find, nub, partition)
import Data.List.Lens             (prefixed, suffixed)
import Data.Maybe                 (isNothing, catMaybes)
import Data.Monoid                ((<>))
import Data.Text                  (Text, isPrefixOf, isSuffixOf)
import Data.Text.Lens             (unpacked)
import Data.Text.Read             (decimal)
import System.Process             (StdStream(..), readCreateProcess, proc, std_err)
import System.IO                  (openFile, IOMode(..))

import Echidna.ABI         (SolSignature)
import Echidna.Exec        (execTx)
import Echidna.Transaction (Tx(..), World(..))

import EVM hiding (contracts)
import qualified EVM (contracts)
import EVM.ABI      (AbiType, AbiValue(..))
import EVM.Exec     (vmForEthrunCreation)
import EVM.Solidity
import EVM.Types    (Addr)
import EVM.Concrete (w256)

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
data SolConf = SolConf { _contractAddr    :: Addr     -- ^ Contract address to use
                       , _deployer        :: Addr     -- ^ Contract deployer address to use
                       , _sender          :: [Addr]   -- ^ Sender addresses to use
                       , _balanceAddr     :: Integer  -- ^ Initial balance of deployer and senders
                       , _balanceContract :: Integer  -- ^ Initial balance of contract to test
                       , _prefix          :: Text     -- ^ Function name prefix used to denote tests
                       , _cryticArgs      :: [String] -- ^ Args to pass to crytic
                       , _solcArgs        :: String   -- ^ Args to pass to @solc@
                       , _solcLibs        :: [String] -- ^ List of libraries to load, in order.
                       , _quiet           :: Bool     -- ^ Suppress @solc@ output, errors, and warnings
                       , _checkAsserts    :: Bool     -- ^ Test if we can cause assertions to fail
                       }
makeLenses ''SolConf

-- | An Echidna test is either the name of the function to call and the address where its contract is,
-- or a function that could experience an exception
type SolTest = Either (Text, Addr) SolSignature

-- | Given a file, use its extenstion to check if it is a precompiled contract or try to compile it and
-- get a list of its contracts, throwing exceptions if necessary.
contracts :: (MonadIO m, MonadThrow m, MonadReader x m, Has SolConf x) => FilePath -> m [SolcContract]
contracts fp = let usual = ["--solc-disable-warnings", "--export-format", "solc"] in do
  a  <- view (hasLens . solcArgs)
  q  <- view (hasLens . quiet)
  ls <- view (hasLens . solcLibs)
  c  <- view (hasLens . cryticArgs)
  let solargs = a ++ linkLibraries ls & (usual ++) . 
                  (\sa -> if null sa then [] else ["--solc-args", sa])
  maybe (throwM CompileFailure) (pure . toList . fst) =<< liftIO (do
    stderr <- if q then UseHandle <$> openFile "/dev/null" WriteMode else pure Inherit
    _ <- readCreateProcess (proc "crytic-compile" $ (c ++ solargs) |> fp) {std_err = stderr} ""
    readSolc "crytic-export/combined_solc.json")


addresses :: (MonadReader x m, Has SolConf x) => m [AbiValue]
addresses = view hasLens <&> \(SolConf ca d ads _ _ _ _ _ _ _ _) ->
  AbiAddress . fromIntegral <$> nub (ads ++ [ca, d, 0x0])

populateAddresses :: [Addr] -> Integer -> VM -> VM
populateAddresses []     _ vm = vm
populateAddresses (a:as) b vm = populateAddresses as b (vm & set (env . EVM.contracts . at a) (Just account))
  where account = initialContract (RuntimeCode mempty) & set nonce 1 & set balance (w256 $ fromInteger b)

-- | Address to load the first library
addrLibrary :: Addr
addrLibrary = 0xff

 -- | Load a list of solidity contracts as libraries
loadLibraries :: (MonadIO m, MonadThrow m, MonadReader x m, Has SolConf x)
              => [SolcContract] -> Addr -> Addr -> VM -> m VM
loadLibraries []     _  _ vm = return vm
loadLibraries (l:ls) la d vm = loadLibraries ls (la + 1) d =<< loadRest
  where loadRest = execStateT (execTx $ Tx (Right $ l ^. creationCode) d la 0xffffffff 0) vm

-- | Generate a string to use as argument in solc to link libraries starting from addrLibrary
linkLibraries :: [String] -> String
linkLibraries [] = ""
linkLibraries ls = "--libraries " ++
  iconcatMap (\i x -> concat [x, ":", show $ addrLibrary + toEnum i, ","]) ls

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
    when (isNothing name && length cs > 1 && not q) $
      putStrLn "Multiple contracts found in file, only analyzing the first"
    unless q . putStrLn $ "Analyzing contract: " <> c ^. contractName . unpacked

  -- Local variables
  (SolConf ca d ads bala balc pref _ _ libs _ ch) <- view hasLens
  let bc = c ^. creationCode
      blank = populateAddresses (ads |> d) bala (vmForEthrunCreation bc)
      abi = liftM2 (,) (view methodName) (fmap snd . view methodInputs) <$> toList (c ^. abiMap)
      (tests, funs) = partition (isPrefixOf pref . fst) abi

  -- Select libraries
  ls <- mapM (choose cs . Just . T.pack) libs

  -- Make sure everything is ready to use, then ship it
  mapM_ (uncurry ensure) $ [(abi, NoFuncs), (funs, OnlyTests)]
                        ++ if ch then [] else [(tests, NoTests)] -- ABI checks
  ensure bc (NoBytecode $ c ^. contractName)                     -- Bytecode check
  case find (not . null . snd) tests of
    Just (t,_) -> throwM $ TestArgsFound t                       -- Test args check
    Nothing    -> loadLibraries ls addrLibrary d blank >>= fmap (, fallback : funs, fst <$> tests) .
      execStateT (execTx $ Tx (Right bc) d ca 0xffffffff (w256 $ fromInteger balc))


  where choose []    _        = throwM NoContracts
        choose (c:_) Nothing  = return c
        choose _     (Just n) = maybe (throwM $ ContractNotFound n) pure $
                                      find (isSuffixOf n . view contractName) cs
        fallback = ("",[])

-- | Given a file and an optional contract name, compile the file as solidity, then, if a name is
-- given, try to fine the specified contract (assuming it is in the file provided), otherwise, find
-- the first contract in the file. Take said contract and return an initial VM state with it loaded,
-- its ABI (as 'SolSignature's), and the names of its Echidna tests. NOTE: unlike 'loadSpecified',
-- contract names passed here don't need the file they occur in specified.
--loadSolidity :: (MonadIO m, MonadThrow m, MonadReader x m, Has SolConf x)
--             => FilePath -> Maybe Text -> m (VM, [SolSignature], [Text])
--loadSolidity fp name = contracts fp >>= loadSpecified name
loadWithCryticCompile :: (MonadIO m, MonadThrow m, MonadReader x m, Has SolConf x)
             => FilePath -> Maybe Text -> m (VM, [SolSignature], [Text])
loadWithCryticCompile fp name = contracts fp >>= loadSpecified name 

-- | Given the results of 'loadSolidity', assuming a single-contract test, get everything ready
-- for running a 'Campaign' against the tests found.
prepareForTest :: (MonadReader x m, Has SolConf x)
               => (VM, [SolSignature], [Text]) -> m (VM, World, [SolTest])
prepareForTest (v, a, ts) = view hasLens <&> \(SolConf _ _ s _ _ _ _ _ _ _ ch) ->
  (v, World s [(r, a)], fmap Left (zip ts $ repeat r) ++ if ch then Right <$> drop 1 a else []) where
    r = v ^. state . contract

-- | Basically loadSolidity, but prepares the results to be passed directly into
-- a testing function.
loadSolTests :: (MonadIO m, MonadThrow m, MonadReader x m, Has SolConf x)
             => FilePath -> Maybe Text -> m (VM, World, [SolTest])
loadSolTests fp name = loadWithCryticCompile fp name >>= prepareForTest

mkValidAbiInt :: Int -> Int256 -> Maybe AbiValue
mkValidAbiInt i x = if abs x <= 2 ^ (i - 1) - 1 then Just $ AbiInt i x else Nothing

mkValidAbiUInt :: Int -> Word256 -> Maybe AbiValue
mkValidAbiUInt i x = if x <= 2 ^ i - 1 then Just $ AbiUInt i x else Nothing

-- | Given a list of 'SolcContract's, try to parse out string and integer literals
extractConstants :: [SolcContract] -> [AbiValue]
extractConstants = nub . concatMap (constants "" . view contractAst) where
  -- Tools for parsing numbers and quoted strings from 'Text'
  asDecimal = preview $ to decimal . _Right . _1
  asQuoted  = preview $ unpacked . prefixed "\"" . suffixed "\"" . packedChars
  -- We need this because sometimes @solc@ emits a json string with a type, then a string
  -- representation of some value of that type. Why is this? Unclear. Anyway, this lets us match
  -- those cases like regular strings
  literal t f (String (T.words -> ((^? only t) -> m) : y : _)) = m *> f y
  literal _ _ _                                                = Nothing
  -- When we get a number, it could be an address, uint, or int. We'll try everything.
  dec i = let l f = f <$> [8,16..256] <*> fmap fromIntegral [i-1..i+1] in
    AbiAddress i : catMaybes (l mkValidAbiInt ++ l mkValidAbiUInt)
  -- 'constants' takes a property name and its 'Value', then tries to find solidity literals
  -- CASE ONE: we're looking at a big object with a bunch of little objects, recurse
  constants _ (Object o) = concatMap (uncurry constants) $ M.toList o
  constants _ (Array  a) = concatMap (constants "")        a
  -- CASE TWO: we're looking at a @type@, try to parse it
  -- 2.1: We're looking at a @int_const@ with a decimal number inside, could be an address, int, or uint
  --      @type: "int_const 0x12"@ ==> @[AbiAddress 18, AbiUInt 8 18,..., AbiUInt 256 18, AbiInt 8 18,...]@
  constants "typeString" (literal "int_const" asDecimal -> Just i) = dec i
  -- 2.2: We're looking at something of the form @type: literal_string "[...]"@, a string literal
  --      @type: "literal_string \"123\""@ ==> @[AbiString "123", AbiBytes 3 "123"...]@
  constants "typeString" (literal "literal_string" asQuoted -> Just b) =
    let size = BS.length b in [AbiString b, AbiBytesDynamic b] ++
      fmap (\n -> AbiBytes n . BS.append b $ BS.replicate (n - size) 0) [size..32]
  -- CASE THREE: we're at a leaf node with no constants
  constants _  _ = []

returnTypes :: [SolcContract] -> Text -> Maybe AbiType
returnTypes cs t = preview (_Just . methodOutput . _Just . _2) .
  find ((== t) . view methodName) $ concatMap (toList . view abiMap) cs
