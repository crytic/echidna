{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Echidna.SourceAnalysis.FoundryTests where

import Control.Monad (when)
import Control.Monad.Random.Strict (MonadRandom)
import Data.Aeson ((.:), (.:?), (.!=), eitherDecode, withObject)
import Data.Aeson.Types (FromJSON(..), Value(Object))
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.List (find, sortOn)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Language.Haskell.TH (runIO)
import Language.Haskell.TH.Syntax (liftString)
import System.Directory (doesFileExist, doesDirectoryExist, findExecutable,
                         getTemporaryDirectory)
import System.Environment (getExecutablePath, lookupEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeDirectory)
import System.IO (hPutStrLn, stderr)
import System.Process (StdStream(..), readCreateProcessWithExitCode, proc, std_err)

import EVM.ABI (AbiValue, abiValueType)
import EVM.Types (Addr)

import Echidna.ABI (GenDict, genAbiValueM)
import Echidna.Types.Solidity (SolConf(..))
import Echidna.Types.Tx (Tx(..), TxCall(..))
import Echidna.Utility (measureIO) 

-- | Result of extracting transaction sequences from Foundry tests
newtype FoundryTestInfo = FoundryTestInfo
  { sequences :: [FoundryTestSequence]
  } deriving (Show)

-- | A single test sequence extracted from a Foundry test function
data FoundryTestSequence = FoundryTestSequence
  { source       :: String           -- ^ Source test function name (e.g., "TestContract.test_example")
  , transactions :: [TxWithHoles]    -- ^ Extracted transaction sequence (may contain holes)
  } deriving (Show)

-- | A transaction with optional "holes" - argument positions that need fuzzing
data TxWithHoles = TxWithHoles
  { tx    :: !Tx       -- ^ The transaction (with placeholder values for holes)
  , holes :: ![Int]    -- ^ Indices into the args list that need to be fuzzed
  } deriving (Show)

instance FromJSON FoundryTestInfo where
  parseJSON = withObject "FoundryTestInfo" $ \o -> do
    sequences <- o .:? "sequences" .!= []
    pure FoundryTestInfo {..}

instance FromJSON FoundryTestSequence where
  parseJSON = withObject "FoundryTestSequence" $ \o -> do
    source <- o .: "source"
    transactions <- o .: "transactions"
    pure FoundryTestSequence {..}

instance FromJSON TxWithHoles where
  parseJSON = withObject "TxWithHoles" $ \o -> do
    tx <- parseJSON (Object o)  -- Parse as Tx using its FromJSON
    holes <- o .:? "holes" .!= []
    pure TxWithHoles {..}

-- | Fill holes in a transaction with randomly generated values
fillHoles :: MonadRandom m => GenDict -> TxWithHoles -> m Tx
fillHoles genDict txh
  | null txh.holes = pure txh.tx
  | otherwise = case txh.tx.call of
      SolCall (funcName, args) -> do
        filledArgs <- fillArgsHoles genDict txh.holes args 0
        pure txh.tx { call = SolCall (funcName, filledArgs) }
      _ -> pure txh.tx

-- | Fill holes in an argument list
fillArgsHoles :: MonadRandom m => GenDict -> [Int] -> [AbiValue] -> Int -> m [AbiValue]
fillArgsHoles _ _ [] _ = pure []
fillArgsHoles genDict holes (arg:rest) idx
  | idx `elem` holes = do
      -- Generate a new value of the same type
      newVal <- genAbiValueM genDict (abiValueType arg)
      (newVal :) <$> fillArgsHoles genDict holes rest (idx + 1)
  | otherwise =
      (arg :) <$> fillArgsHoles genDict holes rest (idx + 1)

-- | Fill holes in a list of transactions
fillAllHoles :: MonadRandom m => GenDict -> [TxWithHoles] -> m [Tx]
fillAllHoles genDict = traverse (fillHoles genDict)

-- | Empty result when extraction is disabled or fails
emptyFoundryTestInfo :: FoundryTestInfo
emptyFoundryTestInfo = FoundryTestInfo []

-- | Format an address for passing to the Python script
formatAddr :: Addr -> String
formatAddr = show

-- | Name of the extraction script
scriptName :: String
scriptName = "extract_foundry_tests.py"

-- | Get or create the extraction script
-- First searches in common locations, then creates from embedded content if not found
getOrCreateScript :: IO FilePath
getOrCreateScript = do
  existing <- findScript
  case existing of
    Just path -> pure path
    Nothing -> do
      -- Write the embedded script to a temp directory
      tmpDir <- getTemporaryDirectory
      let scriptPath = tmpDir </> "echidna_" <> scriptName
      writeFile scriptPath embeddedScript
      pure scriptPath

-- | Find the extraction script in common locations
findScript :: IO (Maybe FilePath)
findScript = do
  -- Check for environment variable first
  envScriptsDir <- lookupEnv "ECHIDNA_SCRIPTS_DIR"

  -- Get possible paths
  execPath <- getExecutablePath
  let execDir = takeDirectory execPath
      -- Common locations to search
      defaultCandidates =
        [ "scripts" </> scriptName                              -- Current dir (development)
        , execDir </> ".." </> "scripts" </> scriptName          -- Relative to executable
        , execDir </> ".." </> "share" </> "echidna" </> "scripts" </> scriptName  -- Installation
        , execDir </> "scripts" </> scriptName                   -- Next to executable
        ]
      -- If env var is set, prepend it to the search list
      candidates = case envScriptsDir of
        Just dir -> (dir </> scriptName) : defaultCandidates
        Nothing  -> defaultCandidates
  -- Find the first path that exists
  findExistingFile candidates

-- | Find the first file that exists from a list of candidates
findExistingFile :: [FilePath] -> IO (Maybe FilePath)
findExistingFile paths = do
  results <- traverse (\p -> doesFileExist p >>= \exists -> pure (p, exists)) paths
  pure $ fst <$> find snd results

-- | Find the test directory in a Foundry project
-- Slither needs the test directory to find test functions,
-- and it will resolve imports (like ../src/Contract.sol) from there
findTestDir :: FilePath -> IO FilePath
findTestDir fp = do
  let dir = takeDirectory fp
      testDir = dir </> "test"
      parentTestDir = takeDirectory dir </> "test"
  -- Check if test/ directory exists relative to the file
  testDirExists <- doesDirectoryExist testDir
  if testDirExists
    then pure testDir
    else do
      -- Check parent directory (if fp is in src/)
      parentTestExists <- doesDirectoryExist parentTestDir
      if parentTestExists
        then pure parentTestDir
        else pure dir  -- Fallback to original directory

-- | Embedded Python script for extracting Foundry test sequences
-- The script is embedded at compile time from the assets directory
embeddedScript :: String
embeddedScript = $(runIO (readFile "lib/Echidna/SourceAnalysis/assets/extract_foundry_tests.py") >>= liftString)

-- | Extract transaction sequences from Foundry test functions
--
-- The optional targetContract parameter filters calls to only include
-- those made to the specified contract type. This is important because
-- Echidna needs to know which contract each call is targeting.
extractFoundryTests :: FilePath -> SolConf -> Maybe Text -> IO FoundryTestInfo
extractFoundryTests fp solConf targetContract
  | not solConf.prefillCorpus = pure emptyFoundryTestInfo
  | otherwise = findExecutable "python3" >>= \case
      Nothing -> do
        hPutStrLn stderr $
          "WARNING: python3 not found. Skipping Foundry test extraction for corpus prefill.\n"
          <> "Install Python 3 to enable this feature."
        pure emptyFoundryTestInfo
      Just pythonPath -> do
        scriptPath <- getOrCreateScript
        -- Find test directory so Slither can analyze test functions
        testDir <- findTestDir fp
        let args = [scriptPath, testDir]
                   ++ targetContractArgs
                   ++ senderArgs
                   ++ contractAddrArgs
                   ++ cryticArgsForScript
            -- Pass target contract to filter calls
            targetContractArgs = case targetContract of
              Just name -> ["--target-contract", T.unpack name]
              Nothing -> []
            -- Use the first sender address, or deployer as fallback
            senderAddr = case Set.lookupMin solConf.sender of
              Just addr -> addr
              Nothing -> solConf.deployer
            senderArgs = ["--sender", formatAddr senderAddr]
            contractAddrArgs = ["--contract-addr", formatAddr solConf.contractAddr]
            cryticArgsForScript = if null solConf.cryticArgs
              then []
              else "--crytic-args" : solConf.cryticArgs
        (exitCode, out, err) <- measureIO solConf.quiet
          ("Extracting Foundry test sequences from `" <> testDir <> "`") $
          readCreateProcessWithExitCode (proc pythonPath args) {std_err = Inherit} ""
        case exitCode of
          ExitSuccess ->
            case eitherDecode (BSL.pack out) of
              Right info -> do
                let numSeqs = length info.sequences
                    numTxs = sum $ map (\s -> length s.transactions) info.sequences
                when (numSeqs > 0) $ do
                  hPutStrLn stderr $
                    "Extracted " <> show numSeqs <> " test sequences with "
                    <> show numTxs <> " transactions for corpus prefill."
                  hPutStrLn stderr $ formatExtractedCorpus targetContract info
                pure info
              Left msg -> do
                hPutStrLn stderr $
                  "WARNING: Decoding Foundry test extraction output failed. Skipping prefill.\n"
                  <> msg
                pure emptyFoundryTestInfo
          ExitFailure _ -> do
            hPutStrLn stderr $
              "WARNING: Running Foundry test extraction failed. Skipping prefill.\n"
              <> err
            pure emptyFoundryTestInfo

-- | Convert extracted test sequences to corpus format (with holes)
foundryTestsToCorpusWithHoles :: FoundryTestInfo -> [(FilePath, [TxWithHoles])]
foundryTestsToCorpusWithHoles info =
  [ (testSeq.source, testSeq.transactions)
  | testSeq <- info.sequences
  , not (null testSeq.transactions)
  ]

-- | Convert extracted test sequences to corpus format, filling holes with random values
foundryTestsToCorpus :: MonadRandom m => GenDict -> FoundryTestInfo -> m [(FilePath, [Tx])]
foundryTestsToCorpus genDict info = traverse fillSeq (foundryTestsToCorpusWithHoles info)
  where
    fillSeq (source, txsWithHoles) = do
      txs <- fillAllHoles genDict txsWithHoles
      pure (source, txs)

-- | Format extracted sequences for display in the log
-- Groups by contract and shows each function with its call sequence
formatExtractedCorpus :: Maybe Text -> FoundryTestInfo -> String
formatExtractedCorpus targetContract info =
  let grouped = groupByContract info.sequences
      contractName = maybe "Target" T.unpack targetContract
  in unlines $
       ("Extracted corpus for " <> contractName <> ":")
       : concatMap formatContract (sortOn fst $ Map.toList grouped)
  where
    -- Group sequences by their test contract name
    groupByContract :: [FoundryTestSequence] -> Map.Map String [(String, [String])]
    groupByContract = foldr insertSeq Map.empty

    insertSeq seq' m =
      let (contract, func) = splitSource seq'.source
          calls = map formatCall seq'.transactions
      in Map.insertWith (++) contract [(func, calls)] m

    -- Split "ContractName.functionName" into (contract, function)
    splitSource :: String -> (String, String)
    splitSource s = case break (== '.') s of
      (c, '.':f) -> (c, f)
      (c, _)     -> (c, "unknown")

    -- Format a single Tx call (from TxWithHoles)
    formatCall :: TxWithHoles -> String
    formatCall txh = case txh.tx.call of
      SolCall (funcName, _args) ->
        let holeInfo = if null txh.holes then "" else " [holes: " <> show txh.holes <> "]"
        in T.unpack funcName <> holeInfo
      _                         -> "<unknown>"

    -- Format a contract's test functions
    formatContract :: (String, [(String, [String])]) -> [String]
    formatContract (contract, funcs) =
      ("  Contract: " <> contract)
      : concatMap formatFunc (sortOn fst funcs)

    formatFunc :: (String, [String]) -> [String]
    formatFunc (func, calls) =
      ("    Function: " <> func)
      : map ("      - " <>) calls
