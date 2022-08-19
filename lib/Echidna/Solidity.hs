{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Echidna.Solidity where

import Control.Lens
import Control.Arrow              (first)
import Control.Monad              (liftM2, when, unless, forM_)
import Control.Monad.Catch        (MonadThrow(..))
import Control.Monad.Extra        (whenM)
import Control.Monad.IO.Class     (MonadIO(..))
import Control.Monad.Reader       (MonadReader)
import Control.Monad.State.Strict (execStateT)
import Data.Foldable              (toList)
import Data.Has                   (Has(..))
import Data.List                  (find, partition, isSuffixOf, (\\))
import Data.Map                   (Map, keys, elems, unions, member)
import Data.Maybe                 (isJust, isNothing, catMaybes, listToMaybe)
import Data.Text                  (Text, isPrefixOf, isSuffixOf, append)
import Data.Text.Lens             (unpacked)
import System.Process             (StdStream(..), readCreateProcessWithExitCode, proc, std_err)
import System.IO                  (openFile, IOMode(..))
import System.Exit                (ExitCode(..))
import System.Directory           (doesDirectoryExist, doesFileExist, findExecutable, listDirectory, removeFile)
import System.FilePath.Posix      ((</>))

import Echidna.ABI                (encodeSig, encodeSigWithName, hashSig, fallback, commonTypeSizes, mkValidAbiInt, mkValidAbiUInt)
import Echidna.Exec               (execTx, initialVM)
import Echidna.Events             (EventMap)
import Echidna.Test               (createTests, isAssertionMode, isPropertyMode, isDapptestMode)
import Echidna.RPC                (loadEthenoBatch)
import Echidna.Types.Solidity     hiding (deployBytecodes, deployContracts)
import Echidna.Types.Signature    (ContractName, FunctionHash, SolSignature, SignatureMap, getBytecodeMetadata)
import Echidna.Types.Tx           (TxConf, basicTx, createTxWithValue, unlimitedGasPerBlock, initialTimestamp, initialBlockNumber)
import Echidna.Types.Test         (TestConf(..), EchidnaTest(..))
import Echidna.Types.World        (World(..))
import Echidna.Fetch              (deployContracts, deployBytecodes)
import Echidna.Processor

import EVM hiding (contracts, path)
import qualified EVM (contracts)
import EVM.ABI
import EVM.Solidity
import EVM.Types    (Addr, w256)

import qualified Data.List.NonEmpty  as NE
import qualified Data.List.NonEmpty.Extra as NEE
import qualified Data.HashMap.Strict as M
import qualified Data.Text           as T

-- | Given a list of source caches (SourceCaches) and an optional contract name, 
-- select one that includes that contract (if possible). Otherwise, use the first source
-- cache available (or fail if it is empty)
selectSourceCache :: Maybe ContractName -> SourceCaches -> SourceCache
selectSourceCache (Just c) scs =
  let r = concatMap (\(cs,sc) -> [sc | isJust $ find (Data.Text.isSuffixOf (":" `append` c)) cs]) scs in
  case r of
    (sc:_) -> sc
    _      -> error "Source cache selection returned no result"

selectSourceCache _ scs =
  case scs of
    (_,sc):_ -> sc
    _        -> error "Empty source cache"

readSolcBatch :: FilePath -> IO (Maybe (Map Text SolcContract, SourceCaches))
readSolcBatch d = do
  fs <- listDirectory d
  mxs <- mapM (\f -> readSolc (d </> f)) fs
  case catMaybes mxs of
    [] -> return Nothing
    xs -> return $ Just (unions $ map fst xs, map (first keys) xs)

-- | Given a list of files, use its extenstion to check if it is a precompiled
-- contract or try to compile it and get a list of its contracts and a list of source cache, throwing
-- exceptions if necessary.
contracts :: (MonadIO m, MonadThrow m, MonadReader x m, Has SolConf x) => NE.NonEmpty FilePath -> m ([SolcContract], SourceCaches)
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
        compileOne :: (MonadIO m, MonadThrow m) => FilePath -> m ([SolcContract], SourceCaches)
        compileOne x = do
          mSolc <- liftIO $ do
            stderr <- if q then UseHandle <$> openFile "/dev/null" WriteMode else pure Inherit
            (ec, out, err) <- readCreateProcessWithExitCode (proc path $ (c ++ solargs) |> x) {std_err = stderr} ""
            case ec of
              ExitSuccess -> readSolcBatch "crytic-export"
              ExitFailure _ -> throwM $ CompileFailure out err

          maybe (throwM SolcReadFailure) (pure . first toList) mSolc
    -- clean up previous artifacts
    liftIO $ removeJsonFiles "crytic-export"
    cps <- mapM compileOne fp
    let (cs, ss) = NE.unzip cps
    when (length ss > 1) $ liftIO $ putStrLn "WARNING: more than one SourceCaches was found after compile. Only the first one will be used."
    pure (concat cs, NE.head ss)

removeJsonFiles :: FilePath -> IO ()
removeJsonFiles dir =
  whenM (doesDirectoryExist dir) $ do
    files <- listDirectory dir
    forM_ files $ \file ->
      when (".json" `Data.List.isSuffixOf` file) $ do
        let path = dir </> file
        whenM (doesFileExist path) $ removeFile path

addresses :: (MonadReader x m, Has SolConf x) => m (NE.NonEmpty AbiValue)
addresses = do
  SolConf{_contractAddr = ca, _deployer = d, _sender = ads} <- view hasLens
  pure $ AbiAddress . fromIntegral <$> NE.nub (join ads [ca, d, 0x0])
  where join (f NE.:| r) l = f NE.:| (r ++ l)

populateAddresses :: [Addr] -> Integer -> VM -> VM
populateAddresses []     _ vm = vm
populateAddresses (a:as) b vm = if deployed then populateAddresses as b vm else populateAddresses as b (vm & set (env . EVM.contracts . at a) (Just account))
  where account   = initialContract (RuntimeCode mempty) & set nonce 0 & set balance (w256 $ fromInteger b)
        deployed = a `member` (vm ^. env . EVM.contracts)

-- | Address to load the first library
addrLibrary :: Addr
addrLibrary = 0xff

-- | Generate a string to use as argument in solc to link libraries starting from addrLibrary
linkLibraries :: [String] -> String
linkLibraries [] = ""
linkLibraries ls = "--libraries " ++
  iconcatMap (\i x -> concat [x, ":", show $ addrLibrary + toEnum i, ","]) ls

-- | Filter methods using a whitelist/blacklist
filterMethods :: Text -> Filter -> NE.NonEmpty SolSignature -> NE.NonEmpty SolSignature
filterMethods _  f@(Whitelist [])  _ = error $ show $ InvalidMethodFilters f
filterMethods cn f@(Whitelist ic) ms = case NE.filter (\s -> encodeSigWithName cn s `elem` ic) ms of
                                         [] -> error $ show $ InvalidMethodFilters f
                                         fs -> NE.fromList fs
filterMethods cn f@(Blacklist ig) ms = case NE.filter (\s -> encodeSigWithName cn s `notElem` ig) ms of
                                         [] -> error $ show $ InvalidMethodFilters f
                                         fs -> NE.fromList fs

-- | Filter methods with arguments, used for dapptest mode
filterMethodsWithArgs :: NE.NonEmpty SolSignature -> NE.NonEmpty SolSignature
filterMethodsWithArgs ms = case NE.filter (\(_, xs) -> not $ null xs) ms of
                             [] -> error "No dapptest tests found" 
                             fs -> NE.fromList fs

abiOf :: Text -> SolcContract -> NE.NonEmpty SolSignature
abiOf pref cc = fallback NE.:| filter (not . isPrefixOf pref . fst) (elems (cc ^. abiMap) <&> \m -> (m ^. methodName, m ^.. methodInputs . traverse . _2))

-- | Given an optional contract name and a list of 'SolcContract's, try to load the specified
-- contract, or, if not provided, the first contract in the list, into a 'VM' usable for Echidna
-- testing and extract an ABI and list of tests. Throws exceptions if anything returned doesn't look
-- usable for Echidna. NOTE: Contract names passed to this function should be prefixed by the
-- filename their code is in, plus a colon.
loadSpecified :: (MonadIO m, MonadThrow m, MonadReader x m, Has SolConf x, Has SolConf x, Has TestConf x, Has TxConf x, MonadFail m)
              => Maybe Text -> [SolcContract] -> m (VM, EventMap, NE.NonEmpty SolSignature, [Text], SignatureMap)
loadSpecified name cs = do
  -- Pick contract to load
  c <- choose cs name
  q <- view (hasLens . quiet)
  liftIO $ do
    when (isNothing name && length cs > 1 && not q) $
      putStrLn "Multiple contracts found, only analyzing the first"
    unless q . putStrLn $ "Analyzing contract: " <> c ^. contractName . unpacked

  -- Local variables
  SolConf ca d ads bala balc mcs pref _ _ libs _ fp dpc dpb ma tm _ fs <- view hasLens
  TestConf _ _ <- view hasLens

  -- generate the complete abi mapping
  let bc = c ^. creationCode
      abi = liftM2 (,) (view methodName) (fmap snd . view methodInputs) <$> toList (c ^. abiMap)
      con = view constructorInputs c
      (tests, funs) = partition (isPrefixOf pref . fst) abi


  -- Filter ABI according to the config options
  let fabiOfc = if isDapptestMode tm then filterMethodsWithArgs (abiOf pref c) else filterMethods (c ^. contractName) fs $ abiOf pref c
  -- Filter again for dapptest tests or assertions checking if enabled
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
  when (null tests && isPropertyMode tm)                        -- < Properties checks
    $ throwM NoTests
  when (null abiMapping && isDapptestMode tm)                   -- < Dapptests checks
    $ throwM NoTests
  when (bc == mempty) $ throwM (NoBytecode $ c ^. contractName) -- Bytecode check
  case find (not . null . snd) tests of
    Just (t,_) -> throwM $ TestArgsFound t                      -- Test args check
    Nothing    -> do
      -- library deployment
      vm0 <- deployContracts (zip [addrLibrary ..] ls) d blank

      -- additional contract deployment (by name)
      cs' <- mapM ((choose cs . Just) . T.pack . snd) dpc
      vm1 <- deployContracts (zip (map fst dpc) cs') d vm0

      -- additional contract deployment (bytecode)
      vm2 <- deployBytecodes dpb d vm1

      -- main contract deployment
      let deployment = execTx $ createTxWithValue bc d ca (fromInteger unlimitedGasPerBlock) (w256 $ fromInteger balc) (0, 0)
      vm3 <- execStateT deployment vm2
      when (isNothing $ currentContract vm3) (throwM $ DeploymentFailed ca)

      -- Run
      let transaction = execTx $ uncurry basicTx setUpFunction d ca (fromInteger unlimitedGasPerBlock) (0, 0)
      vm4 <- if isDapptestMode tm && setUpFunction `elem` abi then execStateT transaction vm3 else return vm3

      case vm4 ^. result of
        Just (VMFailure _) -> throwM SetUpCallFailed
        _                  -> return (vm4, unions $ map (view eventMap) cs, neFuns, fst <$> tests, abiMapping)

  where choose []    _        = throwM NoContracts
        choose (c:_) Nothing  = return c
        choose _     (Just n) = maybe (throwM $ ContractNotFound n) pure $
                                      find (Data.Text.isSuffixOf (if T.any (== ':') n then n else ":" `append` n) . view contractName) cs
        setUpFunction = ("setUp", [])


-- | Given a file and an optional contract name, compile the file as solidity, then, if a name is
-- given, try to fine the specified contract (assuming it is in the file provided), otherwise, find
-- the first contract in the file. Take said contract and return an initial VM state with it loaded,
-- its ABI (as 'SolSignature's), and the names of its Echidna tests. NOTE: unlike 'loadSpecified',
-- contract names passed here don't need the file they occur in specified.
loadWithCryticCompile :: (MonadIO m, MonadThrow m, MonadReader x m, Has SolConf x, Has TestConf x, Has TxConf x, MonadFail m)
                      => NE.NonEmpty FilePath -> Maybe Text -> m (VM, EventMap, NE.NonEmpty SolSignature, [Text], SignatureMap)
loadWithCryticCompile fp name = contracts fp >>= \(cs, _) -> loadSpecified name cs


-- | Given the results of 'loadSolidity', assuming a single-contract test, get everything ready
-- for running a 'Campaign' against the tests found.
prepareForTest :: (MonadReader x m, Has SolConf x)
               => (VM, EventMap, NE.NonEmpty SolSignature, [Text], SignatureMap)
               -> Maybe ContractName
               -> SlitherInfo
               -> m (VM, World, [EchidnaTest])
prepareForTest (vm, em, a, ts, m) c si = do
  SolConf{ _sender = s, _testMode = tm, _testDestruction = td } <- view hasLens
  let r = vm ^. state . contract
      a' = NE.toList a
      ps = filterResults c $ payableFunctions si
      as = if isAssertionMode tm then filterResults c $ asserts si else []
      cs = if isDapptestMode tm then [] else filterResults c (constantFunctions si) \\ as
      (hm, lm) = prepareHashMaps cs as $ filterFallbacks c (fallbackDefined si) (receiveDefined si) m
  pure (vm, World s hm lm ps em, createTests tm td ts r a')


filterFallbacks :: Maybe ContractName -> [ContractName] -> [ContractName] -> SignatureMap -> SignatureMap
filterFallbacks _ [] [] sm = M.map f sm 
  where f ss = NE.fromList $ case NE.filter (/= fallback) ss of 
                []  -> [fallback] -- No other alternative
                ss' -> ss' 
filterFallbacks _ _ _ sm = sm

-- this limited variant is used only in tests
prepareForTest' :: (MonadReader x m, Has SolConf x)
               => (VM, EventMap, NE.NonEmpty SolSignature, [Text], SignatureMap)
               -> m (VM, World, [EchidnaTest])
prepareForTest' (v, em, a, ts, _) = do
  SolConf{ _sender = s, _testMode = tm } <- view hasLens
  let r = v ^. state . contract
      a' = NE.toList a
  pure (v, World s M.empty Nothing [] em, createTests tm True ts r a') 

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

mkSmallAbiInt :: Int -> AbiValue
mkSmallAbiInt i = AbiInt i $ -(2 ^ (i - 1))

timeConstants :: [AbiValue]
timeConstants = concatMap dec [initialTimestamp, initialBlockNumber]
  where dec i = let l f = f <$> commonTypeSizes <*> fmap fromIntegral [i-1..i+1] in
                catMaybes (l mkValidAbiInt ++ l mkValidAbiUInt)

extremeConstants :: [AbiValue]
extremeConstants = concatMap (\i -> [mkSmallAbiInt i, mkLargeAbiInt i, mkLargeAbiUInt i]) commonTypeSizes

returnTypes :: [SolcContract] -> Text -> Maybe AbiType
returnTypes cs t = do
  method <- find ((== t) . view methodName) $ concatMap (toList . view abiMap) cs
  (_, abiType) <- listToMaybe $ method ^. methodOutput
  pure abiType
