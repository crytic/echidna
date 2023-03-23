module Echidna.Solidity where

import Control.Lens hiding (filtered)
import Control.Arrow (first)
import Control.Monad (when, unless, forM_)
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.Extra (whenM)
import Control.Monad.Reader (ReaderT(runReaderT))
import Control.Monad.State.Strict (execStateT)
import Data.Foldable (toList)
import Data.HashMap.Strict qualified as M
import Data.List (find, partition, isSuffixOf, (\\))
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonEmpty qualified as NE
import Data.List.NonEmpty.Extra qualified as NEE
import Data.Map (Map, keys, unions, member)
import Data.Map qualified as Map
import Data.Maybe (isJust, isNothing, catMaybes, listToMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text, isPrefixOf, isSuffixOf, append)
import Data.Text qualified as T
import System.Directory (doesDirectoryExist, doesFileExist, findExecutable, listDirectory, removeFile)
import System.Process (StdStream(..), readCreateProcessWithExitCode, proc, std_err)
import System.Exit (ExitCode(..))
import System.FilePath (joinPath, splitDirectories, (</>))
import System.IO (openFile, IOMode(..))
import System.Info (os)

import EVM hiding (Env, env, contract, contracts, path)
import EVM qualified (contracts, env)
import EVM.ABI
import EVM.Solidity
import EVM.Types (Addr)

import Echidna.ABI (encodeSig, encodeSigWithName, hashSig, fallback, commonTypeSizes, mkValidAbiInt, mkValidAbiUInt)
import Echidna.Exec (execTx, initialVM)
import Echidna.Events (EventMap, extractEvents)
import Echidna.Fetch (deployContracts, deployBytecodes)
import Echidna.Processor
import Echidna.RPC (loadEthenoBatch)
import Echidna.Test (createTests, isAssertionMode, isPropertyMode, isDapptestMode)
import Echidna.Types.Config (EConfig(..), Env(..))
import Echidna.Types.Signature (ContractName, FunctionHash, SolSignature, SignatureMap, getBytecodeMetadata)
import Echidna.Types.Solidity
import Echidna.Types.Test (EchidnaTest(..))
import Echidna.Types.Tx (basicTx, createTxWithValue, unlimitedGasPerBlock, initialTimestamp, initialBlockNumber)
import Echidna.Types.World (World(..))
import Echidna.Utility (measureIO)

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
compileContracts :: SolConf -> NE.NonEmpty FilePath -> IO ([SolcContract], SourceCaches)
compileContracts solConf fp = do
  let usual = ["--solc-disable-warnings", "--export-format", "solc"]
  mp <- findExecutable "crytic-compile"
  case mp of
   Nothing -> throwM NoCryticCompile
   Just path -> do
    let solargs = solConf.solcArgs ++ linkLibraries solConf.solcLibs & (usual ++) .
                  (\sa -> if null sa then [] else ["--solc-args", sa])
        compileOne :: FilePath -> IO ([SolcContract], SourceCaches)
        compileOne x = do
          mSolc <- do
            stderr <- if solConf.quiet
                         then UseHandle <$> openFile nullFilePath WriteMode
                         else pure Inherit
            (ec, out, err) <- measureIO solConf.quiet ("Compiling " <> x) $ do
              readCreateProcessWithExitCode
                (proc path $ (solConf.cryticArgs ++ solargs) |> x) {std_err = stderr} ""
            case ec of
              ExitSuccess -> readSolcBatch "crytic-export"
              ExitFailure _ -> throwM $ CompileFailure out err

          maybe (throwM SolcReadFailure) (pure . first toList) mSolc
        -- | OS-specific path to the "null" file, which accepts writes without storing them
        nullFilePath :: String
        nullFilePath = if os == "mingw32" then "\\\\.\\NUL" else "/dev/null"
    -- clean up previous artifacts
    removeJsonFiles "crytic-export"
    cps <- mapM compileOne fp
    let (cs, ss) = NE.unzip cps
    when (length ss > 1) $
      putStrLn "WARNING: more than one SourceCaches was found after compile. Only the first one will be used."
    pure (concat cs, NE.head ss)

removeJsonFiles :: FilePath -> IO ()
removeJsonFiles dir =
  whenM (doesDirectoryExist dir) $ do
    files <- listDirectory dir
    forM_ files $ \file ->
      when (".json" `Data.List.isSuffixOf` file) $ do
        let path = dir </> file
        whenM (doesFileExist path) $ removeFile path

staticAddresses :: SolConf -> Set AbiValue
staticAddresses SolConf{contractAddr, deployer, sender} = do
  Set.map AbiAddress $ Set.union sender (Set.fromList [contractAddr, deployer, 0x0])

populateAddresses :: Set Addr -> Integer -> VM -> VM
populateAddresses addrs b vm =
  Set.foldl' (\vm' addr ->
    if deployed addr then vm'
    else vm' & set (EVM.env . EVM.contracts . at addr) (Just account)
 ) vm addrs
  where account = initialContract (RuntimeCode (ConcreteRuntimeCode mempty)) & set nonce 0 & set balance (fromInteger b)
        deployed addr = addr `member` vm._env._contracts

-- | Address to load the first library
addrLibrary :: Addr
addrLibrary = 0xff

-- | Generate a string to use as argument in solc to link libraries starting from addrLibrary
linkLibraries :: [String] -> String
linkLibraries [] = ""
linkLibraries ls = "--libraries " ++
  iconcatMap (\i x -> concat [x, ":", show $ addrLibrary + toEnum i, ","]) ls

-- | Filter methods using a whitelist/blacklist
filterMethods :: Text -> Filter -> NE.NonEmpty SolSignature -> [SolSignature]
filterMethods _ f@(Whitelist [])  _ = error $ show $ InvalidMethodFilters f
filterMethods contractName (Whitelist ic) ms =
  NE.filter (\s -> encodeSigWithName contractName s `elem` ic) ms
filterMethods contractName (Blacklist ig) ms =
  NE.filter (\s -> encodeSigWithName contractName s `notElem` ig) ms

-- | Filter methods with arguments, used for dapptest mode
filterMethodsWithArgs :: NE.NonEmpty SolSignature -> NE.NonEmpty SolSignature
filterMethodsWithArgs ms = case NE.filter (\(n, xs) -> T.isPrefixOf "invariant_" n || not (null xs)) ms of
                             [] -> error "No dapptest tests found"
                             fs -> NE.fromList fs

abiOf :: Text -> SolcContract -> NE.NonEmpty SolSignature
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
  :: Env -> Maybe Text -> [SolcContract]
  -> IO (VM, [SolSignature], [Text], SignatureMap)
loadSpecified env name cs = do
  let solConf = env.cfg.solConf

  -- Pick contract to load
  mainContract <- choose cs name
  when (isNothing name && length cs > 1 && not solConf.quiet) $
    putStrLn "Multiple contracts found, only analyzing the first"
  unless solConf.quiet $
    putStrLn $ "Analyzing contract: " <> T.unpack mainContract.contractName

  -- generate the complete abi mapping
  let abi = Map.elems mainContract.abiMap <&> \method -> (method.name, snd <$> method.inputs)
      (tests, funs) = partition (isPrefixOf solConf.prefix . fst) abi

  -- Filter ABI according to the config options
  let fabiOfc = if isDapptestMode solConf.testMode
                  then NE.toList $ filterMethodsWithArgs (abiOf solConf.prefix mainContract)
                  else filterMethods mainContract.contractName solConf.methodFilter $
                         abiOf solConf.prefix mainContract
  -- Filter again for dapptest tests or assertions checking if enabled
  let neFuns = filterMethods mainContract.contractName solConf.methodFilter (fallback NE.:| funs)
  -- Construct ABI mapping for World
  let abiMapping =
        if solConf.allContracts then
          M.fromList $ catMaybes $ cs <&> \contract ->
            let filtered = filterMethods contract.contractName
                                         solConf.methodFilter
                                         (abiOf solConf.prefix contract)
            in (getBytecodeMetadata contract.runtimeCode,) <$> NE.nonEmpty filtered
        else
          case NE.nonEmpty fabiOfc of
            Just ne -> M.singleton (getBytecodeMetadata mainContract.runtimeCode) ne
            Nothing -> mempty

  -- Set up initial VM, either with chosen contract or Etheno initialization file
  -- need to use snd to add to ABI dict
  let vm = initialVM solConf.allowFFI
             & block . gaslimit .~ unlimitedGasPerBlock
             & block . maxCodeSize .~ fromIntegral solConf.codeSize
  blank' <- maybe (pure vm) (loadEthenoBatch solConf.allowFFI) solConf.initialize
  let blank = populateAddresses (Set.insert solConf.deployer solConf.sender)
                                solConf.balanceAddr blank'

  unless (null mainContract.constructorInputs || isJust solConf.initialize) $
    throwM $ ConstructorArgs (show mainContract.constructorInputs)

  -- Select libraries
  ls <- mapM (choose cs . Just . T.pack) solConf.solcLibs

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
    Nothing -> do
      flip runReaderT env $ do
        -- library deployment
        vm0 <- deployContracts (zip [addrLibrary ..] ls) solConf.deployer blank

        -- additional contract deployment (by name)
        cs' <- mapM ((choose cs . Just) . T.pack . snd) solConf.deployContracts
        vm1 <- deployContracts (zip (map fst solConf.deployContracts) cs') solConf.deployer vm0

        -- additional contract deployment (bytecode)
        vm2 <- deployBytecodes solConf.deployBytecodes solConf.deployer vm1

        -- main contract deployment
        let deployment = execTx $ createTxWithValue
                                    mainContract.creationCode
                                    solConf.deployer
                                    solConf.contractAddr
                                    unlimitedGasPerBlock
                                    (fromIntegral solConf.balanceContract)
                                    (0, 0)
        vm3 <- execStateT deployment vm2
        when (isNothing $ currentContract vm3) $
          throwM $ DeploymentFailed solConf.contractAddr $ T.unlines $ extractEvents True env.dapp vm3

        -- Run
        let transaction = execTx $ uncurry basicTx
                                             setUpFunction
                                             solConf.deployer
                                             solConf.contractAddr
                                             unlimitedGasPerBlock
                                             (0, 0)
        vm4 <- if isDapptestMode solConf.testMode && setUpFunction `elem` abi
                  then execStateT transaction vm3
                  else return vm3

        case vm4._result of
          Just (VMFailure _) -> throwM SetUpCallFailed
          _ -> pure (vm4, neFuns, fst <$> tests, abiMapping)

  where choose [] _ = throwM NoContracts
        choose (c:_) Nothing = pure c
        choose _ (Just n) =
          maybe (throwM $ ContractNotFound n) pure $
            find (Data.Text.isSuffixOf (contractId n) . (.contractName)) cs
        contractId n | T.any (== ':') n = let (splitPath, splitName) = T.breakOn ":" n in
                                          rewritePathSeparators splitPath `T.append` splitName
                     | otherwise = ":" `append` n
        rewritePathSeparators = T.pack . joinPath . splitDirectories . T.unpack
        setUpFunction = ("setUp", [])

-- | Given the results of 'loadSolidity', assuming a single-contract test, get everything ready
-- for running a 'Campaign' against the tests found.
mkWorld :: SolConf -> EventMap -> SignatureMap -> Maybe ContractName -> SlitherInfo -> World
mkWorld SolConf{sender, testMode} em m c si =
  let ps = filterResults c si.payableFunctions
      as = if isAssertionMode testMode then filterResults c si.asserts else []
      cs = if isDapptestMode testMode then [] else filterResults c si.constantFunctions \\ as
      (hm, lm) = prepareHashMaps cs as $ filterFallbacks c si.fallbackDefined si.receiveDefined m
  in World sender hm lm ps em

filterFallbacks :: Maybe ContractName -> [ContractName] -> [ContractName] -> SignatureMap -> SignatureMap
filterFallbacks _ [] [] sm = M.map f sm
  where f ss = NE.fromList $ case NE.filter (/= fallback) ss of
                []  -> [fallback] -- No other alternative
                ss' -> ss'
filterFallbacks _ _ _ sm = sm

prepareHashMaps :: [FunctionHash] -> [FunctionHash] -> SignatureMap -> (SignatureMap, Maybe SignatureMap)
prepareHashMaps [] _  m = (m, Nothing)                                -- No constant functions detected
prepareHashMaps cs as m =
  (\case (hm, lm) | M.size hm > 0  && M.size lm > 0  -> (hm, Just lm) -- Usual case
                  | M.size hm > 0  && M.size lm == 0 -> (hm, Nothing) -- No low-priority functions detected
                  | M.size hm == 0 && M.size lm > 0  -> (m,  Nothing) -- No high-priority functions detected
                  | otherwise                        -> error "Error processing function hashmaps"
  ) (M.unionWith NEE.union (filterHashMap not cs m) (filterHashMap id as m), filterHashMap id cs m)
  where filterHashMap f xs = M.mapMaybe (NE.nonEmpty . NE.filter (\s -> f $ (hashSig . encodeSig $ s) `elem` xs))

-- | Given a file and an optional contract name, compile the file as solidity, then, if a name is
-- given, try to fine the specified contract (assuming it is in the file provided), otherwise, find
-- the first contract in the file. Take said contract and return an initial VM state with it loaded,
-- its ABI (as 'SolSignature's), and the names of its Echidna tests. NOTE: unlike 'loadSpecified',
-- contract names passed here don't need the file they occur in specified.
loadSolTests :: Env -> NE.NonEmpty FilePath -> Maybe Text -> IO (VM, World, [EchidnaTest])
loadSolTests env fp name = do
  let solConf = env.cfg.solConf
  (contracts, _) <- compileContracts solConf fp
  (vm, funs, testNames, _signatureMap) <- loadSpecified env name contracts
  let eventMap = Map.unions $ map (.eventMap) contracts
  let world = World solConf.sender M.empty Nothing [] eventMap
  let echidnaTests = createTests solConf.testMode True testNames vm._state._contract funs
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
