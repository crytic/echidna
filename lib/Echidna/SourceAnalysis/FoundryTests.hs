{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Echidna.SourceAnalysis.FoundryTests where

import Control.Monad (when)
import Data.Aeson ((.:), (.:?), (.!=), eitherDecode, withObject)
import Data.Aeson.Types (FromJSON(..))
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.List (find, sortOn)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Language.Haskell.TH (runIO)
import Language.Haskell.TH.Syntax (liftString)
import System.Directory (doesFileExist, doesDirectoryExist, findExecutable,
                         getTemporaryDirectory, listDirectory)
import System.Environment (getExecutablePath, lookupEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeDirectory, takeExtension)
import System.IO (hPutStrLn, stderr)
import System.Process (StdStream(..), readCreateProcessWithExitCode, proc, std_err)

import Echidna.Types.Solidity (SolConf(..))
import Echidna.Types.Tx (Tx(..), TxCall(..))
import Echidna.Utility (measureIO)

-- | Result of extracting transaction sequences from Foundry tests
newtype FoundryTestInfo = FoundryTestInfo
  { sequences :: [FoundryTestSequence]
  } deriving (Show)

-- | A single test sequence extracted from a Foundry test function
data FoundryTestSequence = FoundryTestSequence
  { source       :: String  -- ^ Source test function name (e.g., "TestContract.test_example")
  , transactions :: [Tx]    -- ^ Extracted transaction sequence
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

-- | Empty result when extraction is disabled or fails
emptyFoundryTestInfo :: FoundryTestInfo
emptyFoundryTestInfo = FoundryTestInfo []

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

-- | Find test files in a Foundry project
-- Returns the test directory if it exists and contains .sol files,
-- otherwise returns the original file path
findTestPath :: FilePath -> IO FilePath
findTestPath fp = do
  let projectDir = takeDirectory fp
      testDir = projectDir </> "test"
      -- Also check parent directory in case fp is in src/
      parentTestDir = takeDirectory projectDir </> "test"
  -- Check if test/ directory exists relative to the file
  testDirExists <- doesDirectoryExist testDir
  if testDirExists then do
    testFiles <- listDirectory testDir
    let solFiles = filter (\f -> takeExtension f == ".sol") testFiles
    if not (null solFiles)
      then pure testDir
      else checkParentDir parentTestDir fp
  else checkParentDir parentTestDir fp
  where
    checkParentDir dir origPath = do
      exists <- doesDirectoryExist dir
      if exists then do
        testFiles <- listDirectory dir
        let solFiles = filter (\f -> takeExtension f == ".sol") testFiles
        if not (null solFiles)
          then pure dir
          else pure origPath
      else pure origPath

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
        -- Find test files (look for test/ directory in Foundry projects)
        testPath <- findTestPath fp
        let args = [scriptPath, testPath]
                   ++ targetContractArgs
                   ++ cryticArgsForScript
            -- Pass target contract to filter calls
            targetContractArgs = case targetContract of
              Just name -> ["--target-contract", T.unpack name]
              Nothing -> []
            cryticArgsForScript = if null solConf.cryticArgs
              then []
              else "--crytic-args" : solConf.cryticArgs
        (exitCode, out, err) <- measureIO solConf.quiet
          ("Extracting Foundry test sequences from `" <> testPath <> "`") $
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

-- | Convert extracted test sequences to corpus format
foundryTestsToCorpus :: FoundryTestInfo -> [(FilePath, [Tx])]
foundryTestsToCorpus info =
  [ (testSeq.source, testSeq.transactions)
  | testSeq <- info.sequences
  , not (null testSeq.transactions)
  ]

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

    -- Format a single Tx call
    formatCall :: Tx -> String
    formatCall tx = case tx.call of
      SolCall (funcName, _args) -> T.unpack funcName
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
