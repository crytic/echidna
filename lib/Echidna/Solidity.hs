{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Echidna.Solidity where

import Control.Lens
import Control.Exception          (Exception)
import Control.Arrow              (first)
import Control.Monad              (liftM2, when, unless)
import Control.Monad.Catch        (MonadThrow(..))
import Control.Monad.IO.Class     (MonadIO(..))
import Control.Monad.Reader       (MonadReader)
import Control.Monad.State.Strict (execStateT)
import Data.Foldable              (toList)
import Data.Has                   (Has(..))
import Data.List                  (find, partition)
import Data.Map                   (elems, unions)
import Data.Maybe                 (isJust, isNothing, catMaybes)
import Data.Text                  (Text, isPrefixOf, isSuffixOf, append)
import Data.Text.Lens             (unpacked)
import System.Process             (StdStream(..), readCreateProcessWithExitCode, proc, std_err)
import System.IO                  (openFile, IOMode(..))
import System.Exit                (ExitCode(..))
import System.Directory           (findExecutable)

import Echidna.ABI                (encodeSig, encodeSigWithName, hashSig, fallback, commonTypeSizes, mkValidAbiInt, mkValidAbiUInt)
import Echidna.Exec               (execTx, initialVM)
import Echidna.Events             (EventMap)
import Echidna.Test               (createTests)
import Echidna.RPC                (loadEthenoBatch)
import Echidna.Types.Signature    (ContractName, FunctionHash, SolSignature, SignatureMap, getBytecodeMetadata)
import Echidna.Types.Tx           (TxConf, createTx, createTxWithValue, unlimitedGasPerBlock, initialTimestamp, initialBlockNumber)
import Echidna.Types.Test         (TestMode, TestConf(..), EchidnaTest(..), TestType(..), TestState(..))
import Echidna.Types.World        (World(..))
import Echidna.Processor

import EVM hiding (contracts, path)
import qualified EVM (contracts)
import EVM.ABI
import EVM.Solidity
import EVM.Types    (Addr)
import EVM.Concrete (w256)

import qualified Data.List.NonEmpty  as NE
import qualified Data.List.NonEmpty.Extra as NEE
import qualified Data.HashMap.Strict as M
import qualified Data.Text           as T

data Filter = Blacklist [Text] | Whitelist [Text] deriving Show

-- | Things that can go wrong trying to load a Solidity file for Echidna testing. Read the 'Show'
-- instance for more detailed explanations.
data SolException = BadAddr Addr
                  | CompileFailure String String
                  | SolcReadFailure
                  | NoContracts
                  | TestArgsFound Text
                  | ContractNotFound Text
                  | NoBytecode Text
                  | NoFuncs
                  | NoTests
                  | OnlyTests
                  | ConstructorArgs String
                  | DeploymentFailed
                  | NoCryticCompile
                  | InvalidMethodFilters Filter
makePrisms ''SolException

instance Show SolException where
  show = \case
    BadAddr a                -> "No contract at " ++ show a ++ " exists"
    CompileFailure x y       -> "Couldn't compile given file\n" ++ "stdout:\n" ++ x ++ "stderr:\n" ++ y
    SolcReadFailure          -> "Could not read crytic-export/combined_solc.json"
    NoContracts              -> "No contracts found in given file"
    (ContractNotFound c)     -> "Given contract " ++ show c ++ " not found in given file"
    (TestArgsFound t)        -> "Test " ++ show t ++ " has arguments, aborting"
    (NoBytecode t)           -> "No bytecode found for contract " ++ show t
    NoFuncs                  -> "ABI is empty, are you sure your constructor is right?"
    NoTests                  -> "No tests found in ABI"
    OnlyTests                -> "Only tests and no public functions found in ABI"
    (ConstructorArgs s)      -> "Constructor arguments are required: " ++ s
    NoCryticCompile          -> "crytic-compile not installed or not found in PATH. To install it, run:\n   pip install crytic-compile"
    (InvalidMethodFilters f) -> "Applying " ++ show f ++ " to the methods produces an empty list. Are you filtering the correct functions or fuzzing the correct contract?"
    DeploymentFailed         -> "Deploying the contract failed (revert, out-of-gas, sending ether to an non-payable constructor, etc.)"

instance Exception SolException

-- | Configuration for loading Solidity for Echidna testing.
data SolConf = SolConf { _contractAddr    :: Addr             -- ^ Contract address to use
                       , _deployer        :: Addr             -- ^ Contract deployer address to use
                       , _sender          :: NE.NonEmpty Addr -- ^ Sender addresses to use
                       , _balanceAddr     :: Integer          -- ^ Initial balance of deployer and senders
                       , _balanceContract :: Integer          -- ^ Initial balance of contract to test
                       , _codeSize        :: Integer          -- ^ Max code size for deployed contratcs (default 24576, per EIP-170)
                       , _prefix          :: Text             -- ^ Function name prefix used to denote tests
                       , _cryticArgs      :: [String]         -- ^ Args to pass to crytic
                       , _solcArgs        :: String           -- ^ Args to pass to @solc@
                       , _solcLibs        :: [String]         -- ^ List of libraries to load, in order.
                       , _quiet           :: Bool             -- ^ Suppress @solc@ output, errors, and warnings
                       , _initialize      :: Maybe FilePath   -- ^ Initialize world with Etheno txns
                       , _multiAbi        :: Bool             -- ^ Whether or not to use the multi-abi mode
                       , _checkAsserts    :: Bool             -- ^ Test if we can cause assertions to fail
                       , _benchmarkMode   :: Bool             -- ^ Benchmark mode allows to generate coverage
                       , _testMode        :: String           -- TODO
                       , _methodFilter    :: Filter           -- ^ List of methods to avoid or include calling during a campaign
                       }
makeLenses ''SolConf

-- | Given a list of files, use its extenstion to check if it is a precompiled
-- contract or try to compile it and get a list of its contracts, throwing
-- exceptions if necessary.
contracts :: (MonadIO m, MonadThrow m, MonadReader x m, Has SolConf x) => NE.NonEmpty FilePath -> m ([SolcContract], SourceCache)
contracts fp = let usual = ["--solc-disable-warnings", "--export-format", "solc"] in do
  mp  <- liftIO $ findExecutable "crytic-compile"
  case mp of
   Nothing -> throwM NoCryticCompile
   Just path -> do
    a  <- view (hasLens . solcArgs)
    q  <- view (hasLens . quiet)
    ls <- view (hasLens . solcLibs)
    c  <- view (hasLens . cryticArgs)
    let solargs = a ++ linkLibraries ls & (usual ++) .
                  (\sa -> if null sa then [] else ["--solc-args", sa])
        fps = toList fp
        compileOne :: (MonadIO m, MonadThrow m, MonadReader x m, Has SolConf x) => FilePath -> m ([SolcContract], SourceCache)
        compileOne x = do
          mSolc <- liftIO $ do
            stderr <- if q then UseHandle <$> openFile "/dev/null" WriteMode else pure Inherit
            (ec, out, err) <- readCreateProcessWithExitCode (proc path $ (c ++ solargs) |> x) {std_err = stderr} ""
            case ec of
              ExitSuccess -> readSolc "crytic-export/combined_solc.json"
              ExitFailure _ -> throwM $ CompileFailure out err
            
          maybe (throwM SolcReadFailure) (pure . first toList) mSolc
    cps <- mapM compileOne fps
    let (cs, ss) = unzip cps
    when (length ss > 1) $ liftIO $ putStrLn "WARNING: more than one SourceCache was found after compile. Only the first one will be used."
    pure (concat cs, head ss)

addresses :: (MonadReader x m, Has SolConf x) => m (NE.NonEmpty AbiValue)
addresses = do
  SolConf{_contractAddr = ca, _deployer = d, _sender = ads} <- view hasLens
  pure $ AbiAddress . fromIntegral <$> NE.nub (join ads [ca, d, 0x0])
  where join (f NE.:| r) l = f NE.:| (r ++ l)

populateAddresses :: [Addr] -> Integer -> VM -> VM
populateAddresses []     _ vm = vm
populateAddresses (a:as) b vm = populateAddresses as b (vm & set (env . EVM.contracts . at a) (Just account))
  where account = initialContract (RuntimeCode mempty) & set nonce 0 & set balance (w256 $ fromInteger b)

-- | Address to load the first library
addrLibrary :: Addr
addrLibrary = 0xff

 -- | Load a list of solidity contracts as libraries
loadLibraries :: (MonadIO m, MonadThrow m, MonadReader x m, Has SolConf x)
              => [SolcContract] -> Addr -> Addr -> VM -> m VM
loadLibraries []     _  _ vm = return vm
loadLibraries (l:ls) la d vm = loadLibraries ls (la + 1) d =<< loadRest
  where loadRest = execStateT (execTx $ createTx (l ^. creationCode) d la (fromInteger unlimitedGasPerBlock) (0, 0)) vm

-- | Generate a string to use as argument in solc to link libraries starting from addrLibrary
linkLibraries :: [String] -> String
linkLibraries [] = ""
linkLibraries ls = "--libraries " ++
  iconcatMap (\i x -> concat [x, ":", show $ addrLibrary + toEnum i, ","]) ls

filterMethods :: Text -> Filter -> NE.NonEmpty SolSignature -> NE.NonEmpty SolSignature
filterMethods _  f@(Whitelist [])  _ = error $ show $ InvalidMethodFilters f
filterMethods cn f@(Whitelist ic) ms = case NE.filter (\s -> encodeSigWithName cn s `elem` ic) ms of
                                         [] -> error $ show $ InvalidMethodFilters f
                                         fs -> NE.fromList fs
filterMethods cn f@(Blacklist ig) ms = case NE.filter (\s -> encodeSigWithName cn s `notElem` ig) ms of
                                         [] -> error $ show $ InvalidMethodFilters f
                                         fs -> NE.fromList fs

abiOf :: Text -> SolcContract -> NE.NonEmpty SolSignature
abiOf pref cc = fallback NE.:| filter (not . isPrefixOf pref . fst) (elems (cc ^. abiMap) <&> \m -> (m ^. methodName, m ^.. methodInputs . traverse . _2))

-- | Given an optional contract name and a list of 'SolcContract's, try to load the specified
-- contract, or, if not provided, the first contract in the list, into a 'VM' usable for Echidna
-- testing and extract an ABI and list of tests. Throws exceptions if anything returned doesn't look
-- usable for Echidna. NOTE: Contract names passed to this function should be prefixed by the
-- filename their code is in, plus a colon.
loadSpecified :: (MonadIO m, MonadThrow m, MonadReader x m, Has SolConf x, Has SolConf x, Has TestConf x, Has TxConf x, MonadFail m)
              => Maybe Text -> [SolcContract] -> m (VM, EventMap, NE.NonEmpty SolSignature, [Text], SignatureMap, TestMode)
loadSpecified name cs = do
  -- Pick contract to load
  c <- choose cs name
  q <- view (hasLens . quiet)
  liftIO $ do
    when (isNothing name && length cs > 1 && not q) $
      putStrLn "Multiple contracts found, only analyzing the first"
    unless q . putStrLn $ "Analyzing contract: " <> c ^. contractName . unpacked

  -- Local variables
  SolConf ca d ads bala balc mcs pref _ _ libs _ fp ma ch bm tm fs <- view hasLens
  TestConf _ _ <- view hasLens

  -- generate the complete abi mapping
  let bc = c ^. creationCode
      abi = liftM2 (,) (view methodName) (fmap snd . view methodInputs) <$> toList (c ^. abiMap)
      con = view constructorInputs c
      (tests, funs) = partition (isPrefixOf pref . fst) abi


  -- Filter ABI according to the config options
  let fabiOfc = filterMethods (c ^. contractName) fs $ abiOf pref c
  -- Filter again for assertions checking if enabled
  let neFuns = filterMethods (c ^. contractName) fs (fallback NE.:| funs)

  -- Construct ABI mapping for World
  let abiMapping = if ma then M.fromList $ cs <&> \cc -> (getBytecodeMetadata $ cc ^. runtimeCode,  filterMethods (cc ^. contractName) fs $ abiOf pref cc)
                         else M.singleton (getBytecodeMetadata $ c ^. runtimeCode) fabiOfc
  
  -- Set up initial VM, either with chosen contract or Etheno initialization file
  -- need to use snd to add to ABI dict
  blank' <- maybe (pure (initialVM & block . gaslimit .~ fromInteger unlimitedGasPerBlock & block . maxCodeSize .~ w256 (fromInteger mcs)))
                  loadEthenoBatch
                  fp
  let blank = populateAddresses (NE.toList ads |> d) bala blank'

  unless (null con || isJust fp) (throwM $ ConstructorArgs (show con))
  -- Select libraries
  ls <- mapM (choose cs . Just . T.pack) libs

  -- Make sure everything is ready to use, then ship it
  when (null abi) $ throwM NoFuncs                              -- < ABI checks
  when (not ch && null tests && not bm) $ throwM NoTests        -- <
  when (bc == mempty) $ throwM (NoBytecode $ c ^. contractName) -- Bytecode check
  case find (not . null . snd) tests of
    Just (t,_) -> throwM $ TestArgsFound t                      -- Test args check
    Nothing    -> do
      vm <- loadLibraries ls addrLibrary d blank
      let transaction = execTx $ createTxWithValue bc d ca (fromInteger unlimitedGasPerBlock) (w256 $ fromInteger balc) (0, 0)
      vm' <- execStateT transaction vm
      case currentContract vm' of
        Just _  -> return (vm', unions $ map (view eventMap) cs, neFuns, fst <$> tests, abiMapping, tm)
        Nothing -> throwM DeploymentFailed

  where choose []    _        = throwM NoContracts
        choose (c:_) Nothing  = return c
        choose _     (Just n) = maybe (throwM $ ContractNotFound n) pure $
                                      find (isSuffixOf (if T.any (== ':') n then n else ":" `append` n) . view contractName) cs

-- | Given a file and an optional contract name, compile the file as solidity, then, if a name is
-- given, try to fine the specified contract (assuming it is in the file provided), otherwise, find
-- the first contract in the file. Take said contract and return an initial VM state with it loaded,
-- its ABI (as 'SolSignature's), and the names of its Echidna tests. NOTE: unlike 'loadSpecified',
-- contract names passed here don't need the file they occur in specified.
--loadSolidity :: (MonadIO m, MonadThrow m, MonadReader x m, Has SolConf x)
--             => FilePath -> Maybe Text -> m (VM, [SolSignature], [Text])
--loadSolidity fp name = contracts fp >>= loadSpecified name
loadWithCryticCompile :: (MonadIO m, MonadThrow m, MonadReader x m, Has SolConf x, Has TestConf x, Has TxConf x, MonadFail m)
                      => NE.NonEmpty FilePath -> Maybe Text -> m (VM, EventMap, NE.NonEmpty SolSignature, [Text], SignatureMap, TestMode)
loadWithCryticCompile fp name = contracts fp >>= \(cs, _) -> loadSpecified name cs


-- | Given the results of 'loadSolidity', assuming a single-contract test, get everything ready
-- for running a 'Campaign' against the tests found.
prepareForTest :: (MonadReader x m, Has SolConf x)
               => (VM, EventMap, NE.NonEmpty SolSignature, [Text], SignatureMap, TestMode)
               -> Maybe ContractName
               -> SlitherInfo
               -> m (VM, World, [EchidnaTest])
prepareForTest (v, em, a, ts, m, tm) c si = do
  SolConf{ _sender = s, _checkAsserts = ch } <- view hasLens
  let r = v ^. state . contract
      a' = NE.toList a
      ps = filterResults c $ payableFunctions si
      as = if ch then filterResults c $ asserts si else []
      cs = filterResults c $ constantFunctions si
      (hm, lm) = prepareHashMaps cs as m
  pure (v, World s hm lm ps em, createTests tm ts r a') 

-- this limited variant is used only in tests
prepareForTest' :: (MonadReader x m, Has SolConf x)
               => (VM, EventMap, NE.NonEmpty SolSignature, [Text], SignatureMap, TestMode)
               -> m (VM, World, [EchidnaTest])
prepareForTest' (v, em, a, ts, _, tm) = do
  SolConf{ _sender = s, _checkAsserts = ch } <- view hasLens
  let r = v ^. state . contract
      a' = NE.toList a
  pure (v, World s M.empty Nothing [] em, createTests tm ts r a') -- fmap Left (zip ts $ repeat r) ++ if ch then Right <$> drop 1 a' else [])

prepareHashMaps :: [FunctionHash] -> [FunctionHash] -> SignatureMap -> (SignatureMap, Maybe SignatureMap)
prepareHashMaps [] _  m = (m, Nothing)                                -- No constant functions detected
prepareHashMaps cs as m =
  (\case (hm, lm) | M.size hm > 0  && M.size lm > 0  -> (hm, Just lm) -- Usual case
                  | M.size hm > 0  && M.size lm == 0 -> (hm, Nothing) -- No low-priority functions detected
                  | M.size hm == 0 && M.size lm > 0  -> (m,  Nothing) -- No high-priority functions detected
                  | otherwise                        -> error "Error processing function hashmaps"
  ) (M.unionWith NEE.union (filterHashMap not cs m) (filterHashMap id as m), filterHashMap id cs m)
  where filterHashMap f xs = M.mapMaybe (NE.nonEmpty . NE.filter (\s -> f $ (hashSig . encodeSig $ s) `elem` xs))

-- | Basically loadSolidity, but prepares the results to be passed directly into
-- a testing function.
loadSolTests :: (MonadIO m, MonadThrow m, MonadReader x m, Has SolConf x, Has TestConf x, Has TxConf x, MonadFail m)
             => NE.NonEmpty FilePath -> Maybe Text -> m (VM, World, [EchidnaTest])
loadSolTests fp name = loadWithCryticCompile fp name >>= prepareForTest'

mkLargeAbiInt :: Int -> AbiValue
mkLargeAbiInt i = AbiInt i $ 2 ^ (i - 1) - 1

mkLargeAbiUInt :: Int -> AbiValue
mkLargeAbiUInt i = AbiUInt i $ 2 ^ i - 1

timeConstants :: [AbiValue]
timeConstants = concatMap dec [initialTimestamp, initialBlockNumber]
  where dec i = let l f = f <$> commonTypeSizes <*> fmap fromIntegral [i-1..i+1] in
                catMaybes (l mkValidAbiInt ++ l mkValidAbiUInt)

largeConstants :: [AbiValue]
largeConstants = concatMap (\i -> [mkLargeAbiInt i, mkLargeAbiUInt i]) commonTypeSizes

returnTypes :: [SolcContract] -> Text -> Maybe AbiType
returnTypes cs t = preview (_Just . methodOutput . _Just . _2) .
  find ((== t) . view methodName) $ concatMap (toList . view abiMap) cs
