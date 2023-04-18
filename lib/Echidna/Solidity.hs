module Echidna.Solidity where

import Optics.Core hiding (filtered)

import Control.Monad (when, unless, forM_)
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.Extra (whenM)
import Control.Monad.Reader (ReaderT(runReaderT))
import Control.Monad.ST (stToIO, RealWorld)
import Data.Foldable (toList)
import Data.List (find, partition, isSuffixOf, (\\))
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonEmpty qualified as NE
import Data.List.NonEmpty.Extra qualified as NEE
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (isJust, isNothing, catMaybes, listToMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text, isPrefixOf, isSuffixOf, append)
import Data.Text qualified as T
import System.Directory
  (doesDirectoryExist, doesFileExist, findExecutable, listDirectory, removeFile)
import System.Process (StdStream(..), readCreateProcessWithExitCode, proc, std_err)
import System.Exit (ExitCode(..))
import System.FilePath (joinPath, splitDirectories, (</>))
import System.FilePath.Posix qualified as FPP
import System.IO (openFile, IOMode(..))
import System.Info (os)

import EVM (initialContract, currentContract)
import EVM.ABI
import EVM.Dapp (DappInfo(..))
import EVM.Solidity
import EVM.Types hiding (Env)

import Echidna.ABI
  ( encodeSig, encodeSigWithName, hashSig, fallback
  , commonTypeSizes, mkValidAbiInt, mkValidAbiUInt )
import Echidna.Deploy (deployContracts, deployBytecodes)
import Echidna.Etheno (loadEthenoBatch)
import Echidna.Events (EventMap, extractEvents)
import Echidna.Exec (execTx, initialVM)
import Echidna.SourceAnalysis.Slither
import Echidna.Symbolic (forceAddr)
import Echidna.Test (createTests, isAssertionMode, isPropertyMode, isDapptestMode)
import Echidna.Types.Config (EConfig(..), Env(..))
import Echidna.Types.Signature
  (ContractName, SolSignature, SignatureMap, FunctionName)
import Echidna.Types.Solidity
import Echidna.Types.Test (EchidnaTest(..))
import Echidna.Types.Tx
  ( basicTx, createTxWithValue, unlimitedGasPerBlock, initialTimestamp
  , initialBlockNumber )
import Echidna.Types.World (World(..))
import Echidna.Utility (measureIO)

readSolcBatch :: FilePath -> IO [BuildOutput]
readSolcBatch d = do
  fs <- filter (".json" `Data.List.isSuffixOf`) <$> listDirectory d
  mapM parseOne fs
  where
  parseOne f =
    readSolc CombinedJSON "" (d </> f) >>= \case
      Right buildOutput -> pure buildOutput
      Left e ->
        error $ "Failed to parse combined JSON file " <> (d </> f) <> "\n" <> e

-- | Given a list of files, use its extenstion to check if it is a precompiled
-- contract or try to compile it and get a list of its contracts and a list of source
-- cache, throwing exceptions if necessary.
compileContracts
  :: SolConf
  -> NonEmpty FilePath
  -> IO BuildOutput
compileContracts solConf fp = do
  path <- findExecutable "crytic-compile" >>= \case
    Nothing -> throwM NoCryticCompile
    Just path -> pure path

  let
    usual = ["--solc-disable-warnings", "--export-format", "solc"]
    solargs = solConf.solcArgs ++ linkLibraries solConf.solcLibs & (usual ++) .
              (\sa -> if null sa then [] else ["--solc-args", sa])
    compileOne :: FilePath -> IO BuildOutput
    compileOne x = do
      stderr <- if solConf.quiet
                   then UseHandle <$> openFile nullFilePath WriteMode
                   else pure Inherit
      (ec, out, err) <- measureIO solConf.quiet ("Compiling " <> x) $ do
        readCreateProcessWithExitCode
          (proc path $ (solConf.cryticArgs ++ solargs) |> x) {std_err = stderr} ""
      case ec of
        ExitSuccess -> mconcat <$> readSolcBatch "crytic-export"
        ExitFailure _ -> throwM $ CompileFailure out err

    -- | OS-specific path to the "null" file, which accepts writes without storing them
    nullFilePath :: String
    nullFilePath = if os == "mingw32" then "\\\\.\\NUL" else "/dev/null"
  -- clean up previous artifacts
  removeJsonFiles "crytic-export"
  mconcat . NE.toList <$> mapM compileOne fp

removeJsonFiles :: FilePath -> IO ()
removeJsonFiles dir =
  whenM (doesDirectoryExist dir) $ do
    files <- listDirectory dir
    forM_ files $ \file ->
      when (".json" `Data.List.isSuffixOf` file) $ do
        let path = dir </> file
        whenM (doesFileExist path) $ removeFile path

staticAddresses :: SolConf -> Set AbiValue
staticAddresses SolConf{contractAddr, deployer, sender} =
  Set.map AbiAddress $
    Set.union sender (Set.fromList [contractAddr, deployer, 0x0])

populateAddresses :: Set Addr -> Integer -> VM Concrete s -> VM Concrete s
populateAddresses addrs b vm =
  Set.foldl' (\vm' addr ->
    if deployed addr
       then vm'
       else vm' & set (#env % #contracts % at (LitAddr addr)) (Just account)
  ) vm addrs
  where
    account =
      initialContract (RuntimeCode (ConcreteRuntimeCode mempty))
        & set #nonce (Just 0)
        & set #balance (Lit $ fromInteger b)
    deployed addr = LitAddr addr `Map.member` vm.env.contracts

-- | Address to load the first library
addrLibrary :: Addr
addrLibrary = 0xff

-- | Generate a string to use as argument in solc to link libraries starting from addrLibrary
linkLibraries :: [String] -> String
linkLibraries [] = ""
linkLibraries ls = "--libraries " ++
  concatMap (\(i,x) -> concat [x, ":", show $ addrLibrary + i, ","]) (zip [0..] ls)

-- | Filter methods using a whitelist/blacklist
filterMethods :: Text -> Filter -> NonEmpty SolSignature -> [SolSignature]
filterMethods _ f@(Whitelist [])  _ = error $ show $ InvalidMethodFilters f
filterMethods contractName (Whitelist ic) ms =
  NE.filter (\s -> encodeSigWithName contractName s `elem` ic) ms
filterMethods contractName (Blacklist ig) ms =
  NE.filter (\s -> encodeSigWithName contractName s `notElem` ig) ms

-- | Filter methods with arguments, used for dapptest mode
filterMethodsWithArgs :: NonEmpty SolSignature -> NonEmpty SolSignature
filterMethodsWithArgs ms =
  case NE.filter (\(n, xs) -> T.isPrefixOf "invariant_" n || not (null xs)) ms of
    [] -> error "No dapptest tests found"
    fs -> NE.fromList fs

abiOf :: Text -> SolcContract -> NonEmpty SolSignature
abiOf pref solcContract =
  fallback :|
    filter (not . isPrefixOf pref . fst)
           (Map.elems solcContract.abiMap <&> \method -> (method.name, snd <$> method.inputs))

-- | Given an optional contract name and a list of 'SolcContract's, try to load the specified
-- contract, or, if not provided, the first contract in the list, into a 'VM' usable for Echidna
-- testing and extract an ABI and list of tests. Throws exceptions if anything returned doesn't look
-- usable for Echidna. NOTE: Contract names passed to this function should be prefixed by the
-- filename their code is in, plus a colon.
loadSpecified
  :: Env
  -> Maybe Text
  -> [SolcContract]
  -> IO (VM Concrete RealWorld, [SolSignature], [Text], SignatureMap)
loadSpecified env name cs = do
  let solConf = env.cfg.solConf

  -- Pick contract to load
  mainContract <- chooseContract cs name
  when (isNothing name && length cs > 1 && not solConf.quiet) $
    putStrLn "Multiple contracts found, only analyzing the first"
  unless solConf.quiet $
    putStrLn $ "Analyzing contract: " <> T.unpack mainContract.contractName

  let
    -- generate the complete abi mapping
    abi = Map.elems mainContract.abiMap <&> \method -> (method.name, snd <$> method.inputs)
    (tests, funs) = partition (isPrefixOf solConf.prefix . fst) abi

    -- Filter ABI according to the config options
    fabiOfc = if isDapptestMode solConf.testMode
                then NE.toList $ filterMethodsWithArgs (abiOf solConf.prefix mainContract)
                else filterMethods mainContract.contractName solConf.methodFilter $
                       abiOf solConf.prefix mainContract
    -- Filter again for dapptest tests or assertions checking if enabled
    neFuns = filterMethods mainContract.contractName solConf.methodFilter (fallback NE.:| funs)
    -- Construct ABI mapping for World
    abiMapping =
      if solConf.allContracts then
        Map.fromList $ mapMaybe (\contract ->
            let filtered = filterMethods contract.contractName
                                         solConf.methodFilter
                                         (abiOf solConf.prefix contract)
            in (contract.runtimeCodehash,) <$> NE.nonEmpty filtered)
          cs
      else
        case NE.nonEmpty fabiOfc of
          Just ne -> Map.singleton mainContract.runtimeCodehash ne
          Nothing -> mempty

  -- Set up initial VM, either with chosen contract or Etheno initialization file
  -- need to use snd to add to ABI dict
  initVM <- stToIO $ initialVM solConf.allowFFI
  let vm = initVM & #block % #gaslimit .~ unlimitedGasPerBlock
                  & #block % #maxCodeSize .~ fromIntegral solConf.codeSize

  blank' <- maybe (pure vm) (loadEthenoBatch solConf.allowFFI) solConf.initialize
  let blank = populateAddresses (Set.insert solConf.deployer solConf.sender)
                                solConf.balanceAddr blank'

  unless (null mainContract.constructorInputs || isJust solConf.initialize) $
    throwM $ ConstructorArgs (show mainContract.constructorInputs)

  -- Select libraries
  ls <- mapM (chooseContract cs . Just . T.pack) solConf.solcLibs

  -- Make sure everything is ready to use, then ship it
  when (null abi) $
    throwM NoFuncs
  when (null tests && isPropertyMode solConf.testMode) $
    throwM NoTests
  when (null abiMapping && isDapptestMode solConf.testMode) $
    throwM NoTests
  when (mainContract.creationCode == mempty) $
    throwM (NoBytecode mainContract.contractName)

  case find (not . null . snd) tests of
    Just (t, _) -> throwM $ TestArgsFound t
    Nothing -> pure ()

  flip runReaderT env $ do
    -- library deployment
    vm0 <- deployContracts (zip [addrLibrary ..] ls) solConf.deployer blank

    -- additional contract deployment (by name)
    cs' <- mapM ((chooseContract cs . Just) . T.pack . snd) solConf.deployContracts
    vm1 <- deployContracts (zip (map fst solConf.deployContracts) cs') solConf.deployer vm0

    -- additional contract deployment (bytecode)
    vm2 <- deployBytecodes solConf.deployBytecodes solConf.deployer vm1

    -- main contract deployment
    let deployment = execTx vm2 $ createTxWithValue
                                    mainContract.creationCode
                                    solConf.deployer
                                    solConf.contractAddr
                                    unlimitedGasPerBlock
                                    (fromIntegral solConf.balanceContract)
                                    (0, 0)
    (_, vm3) <- deployment
    when (isNothing $ currentContract vm3) $
      throwM $ DeploymentFailed solConf.contractAddr $ T.unlines $ extractEvents True env.dapp vm3

    -- Run
    let transaction = execTx vm3 $ uncurry basicTx
                                             setUpFunction
                                             solConf.deployer
                                             solConf.contractAddr
                                             unlimitedGasPerBlock
                                             (0, 0)
    vm4 <- if isDapptestMode solConf.testMode && setUpFunction `elem` abi
              then snd <$> transaction
              else pure vm3

    case vm4.result of
      Just (VMFailure _) -> throwM SetUpCallFailed
      _ -> pure (vm4, neFuns, fst <$> tests, abiMapping)

  where
    setUpFunction = ("setUp", [])

-- | Given a list of contracts and a requested contract name, pick a contract.
-- See 'loadSpecified' for more information.
chooseContract :: (MonadThrow m) => [SolcContract] -> Maybe Text -> m SolcContract
chooseContract [] _ = throwM NoContracts
chooseContract (c:_) Nothing = pure c
chooseContract cs (Just n) =
  maybe (throwM $ ContractNotFound n) pure $
    find isMatch cs
  where
    isMatch s =
      (Data.Text.isSuffixOf (contractId rewriteOsPathSeparators) . (.contractName)) s ||
      (Data.Text.isSuffixOf (contractId rewritePosixPathSeparators) . (.contractName)) s
    contractId rewrite
      | T.any (== ':') n =
        let (splitPath, splitName) = T.breakOn ":" n
        in rewrite splitPath `T.append` splitName
      | otherwise = ":" `append` n

    rewriteOsPathSeparators = T.pack . joinPath . splitDirectories . T.unpack
    rewritePosixPathSeparators = T.pack . FPP.joinPath . FPP.splitDirectories . T.unpack

-- | Given the results of 'loadSolidity', assuming a single-contract test, get everything ready
-- for running a 'Campaign' against the tests found.
mkWorld
  :: SolConf
  -> EventMap
  -> SignatureMap
  -> Maybe ContractName
  -> SlitherInfo
  -> World
mkWorld SolConf{sender, testMode} eventMap sigMap maybeContract slitherInfo =
  let
    payableSigs = filterResults maybeContract slitherInfo.payableFunctions
    as = if isAssertionMode testMode then filterResults maybeContract slitherInfo.asserts else []
    cs = if isDapptestMode testMode then [] else filterResults maybeContract slitherInfo.constantFunctions \\ as
    (highSignatureMap, lowSignatureMap) = prepareHashMaps cs as $
      filterFallbacks maybeContract slitherInfo.fallbackDefined slitherInfo.receiveDefined sigMap
  in World { senders = sender
           , highSignatureMap
           , lowSignatureMap
           , payableSigs
           , eventMap
           }

-- | This function is used to filter the lists of function names according to the supplied
-- contract name (if any) and returns a list of hashes
filterResults :: Maybe ContractName -> Map ContractName [FunctionName] -> [FunctionSelector]
filterResults (Just contractName) rs =
  case Map.lookup contractName rs of
    Nothing -> filterResults Nothing rs
    Just sig -> hashSig <$> sig
filterResults Nothing rs = hashSig <$> (concat . Map.elems) rs

filterFallbacks
  :: Maybe ContractName
  -> [ContractName]
  -> [ContractName]
  -> SignatureMap
  -> SignatureMap
filterFallbacks _ [] [] sm = Map.map f sm
  where f ss = NE.fromList $ case NE.filter (/= fallback) ss of
                []  -> [fallback] -- No other alternative
                ss' -> ss'
filterFallbacks _ _ _ sm = sm

prepareHashMaps
  :: [FunctionSelector]
  -> [FunctionSelector]
  -> SignatureMap
  -> (SignatureMap, Maybe SignatureMap)
prepareHashMaps [] _  m = (m, Nothing) -- No constant functions detected
prepareHashMaps cs as m =
  let
    (hm, lm) =
      ( Map.unionWith NEE.union (filterHashMap not cs m) (filterHashMap id as m)
      , filterHashMap id cs m )
  in
    if | Map.size hm > 0  && Map.size lm > 0  -> (hm, Just lm) -- Usual case
       | Map.size hm > 0  && Map.size lm == 0 -> (hm, Nothing) -- No low-priority functions detected
       | Map.size hm == 0 && Map.size lm > 0  -> (m,  Nothing) -- No high-priority functions detected
       | otherwise                        -> error "Error processing function hashmaps"
  where
    filterHashMap f xs =
      Map.mapMaybe (NE.nonEmpty . NE.filter (\s -> f $ (hashSig . encodeSig $ s) `elem` xs))

-- | Given a file and an optional contract name, compile the file as solidity, then, if a name is
-- given, try to fine the specified contract (assuming it is in the file provided), otherwise, find
-- the first contract in the file. Take said contract and return an initial VM state with it loaded,
-- its ABI (as 'SolSignature's), and the names of its Echidna tests. NOTE: unlike 'loadSpecified',
-- contract names passed here don't need the file they occur in specified.
loadSolTests
  :: Env
  -> Maybe Text
  -> IO (VM Concrete RealWorld, World, [EchidnaTest])
loadSolTests env name = do
  let solConf = env.cfg.solConf
  let contracts = Map.elems env.dapp.solcByName
  (vm, funs, testNames, _signatureMap) <- loadSpecified env name contracts
  let
    eventMap = Map.unions $ map (.eventMap) contracts
    world = World solConf.sender mempty Nothing [] eventMap
    echidnaTests = createTests solConf.testMode True testNames (forceAddr vm.state.contract) funs
  pure (vm, world, echidnaTests)

mkLargeAbiInt :: Int -> AbiValue
mkLargeAbiInt i = AbiInt i $ 2 ^ (i - 1) - 1

mkLargeAbiUInt :: Int -> AbiValue
mkLargeAbiUInt i = AbiUInt i $ 2 ^ i - 1

mkSmallAbiInt :: Int -> AbiValue
mkSmallAbiInt i = AbiInt i $ -(2 ^ (i - 1))

timeConstants :: Set AbiValue
timeConstants = Set.fromList $ concatMap dec [initialTimestamp, initialBlockNumber]
  where dec i = let l f = f <$> commonTypeSizes <*> fmap fromIntegral [i-1..i+1] in
                catMaybes (l mkValidAbiInt ++ l mkValidAbiUInt)

extremeConstants :: Set AbiValue
extremeConstants =
  Set.unions $
    (\i -> Set.fromList [mkSmallAbiInt i, mkLargeAbiInt i, mkLargeAbiUInt i]) <$> commonTypeSizes

returnTypes :: [SolcContract] -> Text -> Maybe AbiType
returnTypes cs t = do
  method <- find ((== t) . (.name)) $ concatMap (toList . (.abiMap)) cs
  (_, abiType) <- listToMaybe method.output
  pure abiType
