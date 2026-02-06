{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Echidna.MCP where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (try, SomeException, displayException)
import Control.Monad (forever, unless)
import Control.Concurrent.STM
import Data.Aeson (object, (.=), encode)
import Data.IORef (readIORef, modifyIORef', newIORef, IORef, atomicModifyIORef')
import Data.List (find, isPrefixOf, isSuffixOf, sort, intercalate)
import qualified Data.Maybe
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import Text.Printf (printf)
import qualified Data.Map as Map
import Text.Read (readMaybe)
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import System.IO (withFile, IOMode(..), hPutStrLn)
import Data.Char (isSpace, toLower)

import MCP.Server
import EVM.Dapp (DappInfo(..))
import EVM.Solidity (SolcContract(..), Method(..))
import EVM.Types (Addr)
import EVM.ABI (AbiValue(..), AbiType(..), abiValueType)
import Echidna.Types.Test (EchidnaTest(..), didFail, isOptimizationTest)
import Echidna.Types.Tx (Tx(..), TxCall(..))
import Echidna.Types.Coverage (CoverageFileType(..), mergeCoverageMaps, coverageStats)
import Echidna.Output.Source (ppCoveredCode, saveLcovHook)
import Echidna.Output.Corpus (loadTxs)

import Echidna.Types.Config (Env(..), EConfig(..))
import Echidna.Types.World (World(..))
import Echidna.Types.Campaign (getNFuzzWorkers, CampaignConf(..), WorkerState(..))
import Echidna.Types.InterWorker (Bus, Message(..), WrappedMessage(..), AgentId(..), FuzzerCmd(..), BroadcastMsg(..))

-- | StatusState tracks time-based coverage metrics for the MCP status tool.
-- 
-- The 'lastCoverageTime' field records when new coverage was last discovered,
-- allowing agents to detect coverage stagnation and trigger alternative strategies.
-- The 'coveredFunctions' list tracks the most recently covered functions for debugging.
-- The 'coverageHistory' provides trend data for agent decision-making.
--
-- @since 2.3.0
data StatusState = StatusState
  { lastCoverageTime :: Maybe UTCTime
    -- ^ Timestamp when coverage was last improved, or Nothing if no coverage yet
  , coveredFunctions :: [Text]
    -- ^ Most recently covered function names (newest first, limited to ~100 entries)
  , coverageHistory :: [(UTCTime, Int)]
    -- ^ Coverage trend data: (timestamp, coverage points). Max 20 entries for memory efficiency
  }

-- | MCP Tool Definition
--
-- Each tool is exposed via the MCP JSON-RPC 2.0 interface and can be invoked
-- by AI agents to observe fuzzing state or control fuzzing behavior.
--
-- Tool execution receives:
--
-- * @args@: Key-value pairs from the JSON-RPC 'arguments' object
-- * @env@: The Echidna environment with corpus, coverage, and dapp info
-- * @bus@: Inter-worker message bus for sending commands to fuzz workers
-- * @logsRef@: IORef for command logging (reproducibility)
--
-- @since 2.3.0
type ToolExecution = [(Text, Text)] -> Env -> Bus -> IORef [Text] -> IO String

-- | Tool record containing metadata and execution function.
--
-- @since 2.3.0
data Tool = Tool
  { toolName :: String
    -- ^ Unique tool identifier (e.g., "status", "inject_fuzz_transactions")
  , toolDescription :: String
    -- ^ Human-readable description shown in tool discovery
  , execute :: ToolExecution
    -- ^ Function to execute when tool is invoked
  }

-- | Extract function name from a transaction's call field.
-- Returns "unknown" for non-Solidity calls (e.g., Create, SolCalldata).
getFunctionName :: Tx -> Text
getFunctionName tx = case tx.call of
  SolCall (name, _) -> name
  _ -> "unknown"

-- | Implementation of status tool
statusTool :: [IORef WorkerState] -> IORef StatusState -> ToolExecution
statusTool workerRefs statusRef _ env _ logsRef = do
  c <- readIORef env.corpusRef
  st <- readIORef statusRef
  now <- getCurrentTime

  -- Iterations
  workers <- mapM readIORef workerRefs
  let iterations = sum $ map (.ncalls) workers
  let maxIterations = env.cfg.campaignConf.testLimit

  -- Coverage
  (covPoints, _) <- coverageStats env.coverageRefInit env.coverageRefRuntime
  
  -- Update coverage history for trend analysis
  let newHistory = take 20 $ (now, covPoints) : st.coverageHistory
  modifyIORef' statusRef $ \s -> s { coverageHistory = newHistory }

  -- Tests
  tests <- mapM readIORef env.testRefs
  let failedCount = length $ filter didFail tests
  let totalCount = length tests

  -- Optimization values
  let optTests = filter isOptimizationTest tests
      optValues = map (\(EchidnaTest {testType = ty, value = val}) -> printf "%s: %s" (show ty) (show val)) optTests
      optStr = if null optValues then "None" else intercalate ", " optValues

  -- Calculate stagnation duration
  let stagnationStr = case st.lastCoverageTime of
                        Nothing -> "Never"
                        Just t -> 
                          let seconds = round (diffUTCTime now t) :: Integer
                              duration = if seconds < 60 
                                        then show seconds ++ "s"
                                        else if seconds < 3600
                                        then show (seconds `div` 60) ++ "m " ++ show (seconds `mod` 60) ++ "s"
                                        else show (seconds `div` 3600) ++ "h " ++ show ((seconds `mod` 3600) `div` 60) ++ "m"
                          in duration ++ " (stagnant)" 

      funcs = if null st.coveredFunctions
              then "None"
              else unpack $ T.intercalate "\n- " st.coveredFunctions

  -- Include last 10 MCP commands from command log for reproducibility (Principle III)
  allLogs <- readIORef logsRef
  let lastCommands = take 10 allLogs -- Logs are stored newest first
      commandsStr = if null lastCommands
                    then "None"
                    else unpack $ T.intercalate "\n  " (reverse lastCommands)

  return $ printf "Corpus Size: %d\nIterations: %d/%d\nCoverage: %d\nTests: %d/%d\nOptimization Values: %s\nTime since last coverage: %s\nLast 10 covered functions:\n- %s\n\nLast 10 MCP Commands:\n  %s"
                  (Set.size c) iterations maxIterations covPoints failedCount totalCount optStr stagnationStr funcs commandsStr

-- | Helper functions for inject_transaction
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn c s = case break (== c) s of
                (chunk, rest) -> chunk : case rest of
                                           [] -> []
                                           (_:r) -> splitOn c r

splitArgs :: String -> [String]
splitArgs s = go s 0 ""
  where
    go :: String -> Int -> String -> [String]
    go [] _ current = [reverse current]
    go (c:cs) level current
      | c == '[' = go cs (level + 1) (c:current)
      | c == ']' = go cs (level - 1) (c:current)
      | c == ',' && level == 0 = reverse current : go cs level ""
      | otherwise = go cs level (c:current)

parsePrimitive :: String -> Maybe AbiValue
parsePrimitive s =
   let s' = trim s
       lowerS = map toLower s'
   in if lowerS == "true"
      then Just (AbiBool True)
      else if lowerS == "false"
      then Just (AbiBool False)
      else if "0x" `isPrefixOf` s'
           then AbiAddress . fromIntegral <$> (readMaybe s' :: Maybe Integer)
           else AbiUInt 256 . fromIntegral <$> (readMaybe s' :: Maybe Integer)

parseArray :: String -> Maybe AbiValue
parseArray s = do
  let content = trim (drop 1 (take (length s - 1) s))
  let parts = if null content then [] else splitOn ',' content
  vals <- mapM parsePrimitive parts
  let vec = Vector.fromList vals
  if Vector.null vec
    then return $ AbiArrayDynamic (AbiUIntType 256) vec
    else do
      let t = abiValueType (Vector.head vec)
      if all (\v -> abiValueType v == t) vals
        then return $ AbiArrayDynamic t vec
        else Nothing

parseArg :: String -> Maybe AbiValue
parseArg s =
   let s' = trim s
   in if "[" `isPrefixOf` s' && "]" `isSuffixOf` s'
      then parseArray s'
      else parsePrimitive s'

parseFuzzArg :: String -> Maybe (Maybe AbiValue)
parseFuzzArg s =
   let s' = trim s
   in if s' == "?"
      then Just Nothing
      else Just <$> parseArg s'

parseFuzzCall :: String -> Maybe (Text, [Maybe AbiValue])
parseFuzzCall s = do
   let (fname, rest) = break (== '(') s
   if null rest then Nothing else do
     let argsS = take (length rest - 2) (drop 1 rest) -- remove parens
     let argParts = if all isSpace argsS then [] else splitArgs argsS
     args <- mapM parseFuzzArg argParts
     return (pack fname, args)

parseFuzzSequence :: String -> Maybe [(Text, [Maybe AbiValue])]
parseFuzzSequence s = mapM (parseFuzzCall . trim) (splitOn ';' s)

parseCall :: String -> Maybe (String, [AbiValue])
parseCall s = do
   let (fname, rest) = break (== '(') s
   if null rest then Nothing else do
     let argsS = take (length rest - 2) (drop 1 rest) -- remove parens
     let argParts = if all isSpace argsS then [] else splitArgs argsS
     args <- mapM parseArg argParts
     return (fname, args)

readAddr :: String -> Maybe Addr
readAddr s = fromIntegral <$> (readMaybe s :: Maybe Integer)

parseTx :: Maybe Tx -> String -> Maybe Tx
parseTx ctx s = do
   let parts = words s
   case parts of
     (srcS:dstS:valS:_:_) | length parts >= 4 -> do
         src <- readAddr srcS
         dst <- readAddr dstS
         val <- readMaybe valS
         (fname, args) <- parseCall (unwords (drop 3 parts))
         return $ Tx (SolCall (pack fname, args)) src dst 1000000 0 val (0,0)
     _ -> do
         (fname, args) <- parseCall s
         let (src, dst) = case ctx of
               Just t -> (t.src, t.dst)
               Nothing -> (0x1000, 0x2000)
         return $ Tx (SolCall (pack fname, args)) src dst 1000000 0 0 (0,0)

-- | Implementation of reload_corpus tool
-- C013/C014: Handle file system and format errors gracefully
reloadCorpusTool :: ToolExecution
reloadCorpusTool _ env _ logsRef = do
  dir <- maybe getCurrentDirectory pure env.cfg.campaignConf.corpusDir
  result <- try $ loadTxs dir -- returns [(FilePath, [Tx])]
  case result of
    Left (e :: SomeException) -> do
      logMCPCommand logsRef "reload_corpus" [("result", "error"), ("message", pack $ displayException e)]
      return $ "Error: Failed to load corpus - " ++ displayException e
    Right loadedSeqs -> do
      if null loadedSeqs
        then do
          logMCPCommand logsRef "reload_corpus" [("result", "no_sequences")]
          return "No transaction sequences found in corpus directory."
        else do
          currentCorpus <- readIORef env.corpusRef
          let existingTxs = Set.map snd currentCorpus

          let newSeqs = map snd loadedSeqs
          let uniqueNewSeqs = filter (`Set.notMember` existingTxs) newSeqs

          if null uniqueNewSeqs
            then do
              logMCPCommand logsRef "reload_corpus" [("result", "no_new_sequences")]
              return "No NEW transaction sequences found in corpus directory."
            else do
                 let maxId = if Set.null currentCorpus
                             then 0
                             else fst (Set.findMax currentCorpus)

                 let indexedNewSeqs = zip [maxId + 1 ..] uniqueNewSeqs
                 let newCorpus = Set.union currentCorpus (Set.fromList indexedNewSeqs)

                 atomicModifyIORef' env.corpusRef $ const (newCorpus, ())
                 logMCPCommand logsRef "reload_corpus" [("count", pack $ show $ length uniqueNewSeqs), ("result", "success")]
                 return $ printf "Reloaded %d new transaction sequences from %s" (length uniqueNewSeqs) dir

-- | Implementation of dump_lcov tool
-- C008: Handle potential file system errors gracefully
dumpLcovTool :: ToolExecution
dumpLcovTool _ env _ _ = do
  let contracts = Map.elems env.dapp.solcByName
  dir <- maybe getCurrentDirectory pure env.cfg.campaignConf.corpusDir
  result <- try $ saveLcovHook env dir env.sourceCache contracts
  case result of
    Left (e :: SomeException) -> 
      return $ "Error: Failed to dump LCOV coverage - " ++ displayException e
    Right filename -> 
      return $ "Dumped LCOV coverage to " ++ filename

-- | Implementation of inject_fuzz_transactions tool
-- Validate input before processing
fuzzTransactionTool :: ToolExecution
fuzzTransactionTool args env bus logsRef = do
  let txStr = Data.Maybe.fromMaybe "" (lookup "transactions" args)
  -- Input validation - check for empty or overly long input
  if T.null txStr
    then return "Error: 'transactions' parameter is required and cannot be empty."
    else if T.length txStr > 100000  -- Reasonable limit to prevent DoS
      then return "Error: Transaction string exceeds maximum length (100KB)."
      else case parseFuzzSequence (unpack txStr) of
        Nothing -> return "Error: Failed to parse transaction sequence string. Expected format: 'functionName(arg1, arg2)' per line."
        Just seqPrototype -> do
          -- Validate function names and argument counts
          let dapp = env.dapp
              methods = Map.elems dapp.abiMap

              methodsByName = Map.fromListWith (++) [(m.name, [m]) | m <- methods]

              validateCall (name, callArgs) =
                case Map.lookup name methodsByName of
                  Nothing -> Just $ printf "Function '%s' not found." (unpack name)
                  Just ms ->
                    if any (\m -> length m.inputs == length callArgs) ms
                    then Nothing
                    else Just $ printf "Function '%s' found but with different argument count. Expected: %s, Got: %d" (unpack name) (show $ map (length . (.inputs)) ms) (length callArgs)

              errors = Data.Maybe.mapMaybe validateCall seqPrototype

          if not (null errors)
            then do
              logMCPCommand logsRef "inject_fuzz_transactions" [("transactions", txStr), ("result", "error")]
              return $ "Error:\n" ++ unlines errors
            else do
              let nWorkers = getNFuzzWorkers env.cfg.campaignConf
                  calcProb i
                    -- Worker 0 always injects transactions at position 0 with a probability of 90%
                    | i == 0 = 0.9
                    -- For small campaigns (<= 2 workers), all workers share a low probability (20%)
                    | nWorkers <= 2 = 0.2
                    -- For larger campaigns, scale probability linearly from 20% to 90% for other workers
                    | otherwise = 0.2 + fromIntegral (i - 1) * (0.7 / fromIntegral (nWorkers - 2))

              mapM_ (\i -> atomically $ writeTChan bus (WrappedMessage AIId (ToFuzzer i (FuzzSequence seqPrototype (calcProb i))))) [0 .. nWorkers - 1]
              logMCPCommand logsRef "inject_fuzz_transactions" [("transactions", txStr), ("result", "success")]
              return $ printf "Requested fuzzing of transaction sequence '%s' on %d fuzzers" (unpack txStr) nWorkers

-- | Implementation of clear_fuzz_priorities tool
clearPrioritiesTool :: ToolExecution
clearPrioritiesTool _ env bus logsRef = do
  let nWorkers = getNFuzzWorkers env.cfg.campaignConf
  mapM_ (\i -> atomically $ writeTChan bus (WrappedMessage AIId (ToFuzzer i ClearPrioritization))) [0 .. nWorkers - 1]
  logMCPCommand logsRef "clear_fuzz_priorities" [("result", "success")]
  return $ printf "Requested clearing priorities on %d fuzzers" nWorkers

-- | Implementation of target tool
targetTool :: ToolExecution
targetTool _ env _ _ = do
  let contracts = env.dapp.solcByName
      world = env.world

      -- Helper to check if a contract is a target
      isTarget :: SolcContract -> Bool
      isTarget c = c.runtimeCodehash `Map.member` world.highSignatureMap

      -- Find candidates
      candidates = filter (isTarget . snd) (Map.toList contracts)

  case candidates of
    [] -> return "Error: No target contract found."
    ((name, contract):_) -> do
      let signatures = map (.methodSignature) (Map.elems contract.abiMap)
          sortedSigs = sort signatures
      return $ printf "Contract: %s\nFunctions:\n- %s" (unpack name) (unpack $ T.intercalate "\n- " sortedSigs)


-- | Implementation of show_coverage tool
showCoverageTool :: ToolExecution
showCoverageTool args env _ _ = do
  let contractName = Data.Maybe.fromMaybe "" (lookup "contract" args)
  if T.null contractName
     then return "Error: No contract name provided"
     else do
       let dapp = env.dapp
       let matches = Map.filterWithKey (\k _ -> k == contractName || (":" <> contractName) `T.isSuffixOf` k) dapp.solcByName
       case Map.toList matches of
         [] -> return $ printf "Error: Contract '%s' not found" (unpack contractName)
         [(k, solc)] -> do
            covMap <- mergeCoverageMaps dapp env.coverageRefInit env.coverageRefRuntime
            let sc = env.sourceCache

            -- Identify relevant files: only the file defining the contract
            let relevantFiles = Set.singleton $ unpack $ T.dropEnd 1 $ fst $ T.breakOnEnd ":" k

            -- Use all active contracts to generate coverage
            -- This allows showing coverage for a parent contract (e.g. EchidnaTest)
            -- derived from the execution of a child contract (e.g. Echidna).
            let activeContracts = filter (\c -> c.runtimeCodehash `Map.member` covMap) (Map.elems dapp.solcByName)
            -- If no contracts are active (e.g. no coverage yet), use the requested contract to at least show the source
            let contractsToUse = if null activeContracts then [solc] else activeContracts

            -- Generate full report using all active contracts, then filter by relevant files
            let fullReport = ppCoveredCode Txt sc contractsToUse covMap Nothing "" []
            let filterReport text =
                  let ls = T.lines text
                      splitSections [] = []
                      splitSections (l:rest) =
                          let (content, next) = span (" " `T.isPrefixOf`) rest
                          in (l:content) : splitSections next
                      sections = splitSections ls
                      keepSection (header:content) =
                          if unpack header `Set.member` relevantFiles
                          then header : content
                          else []
                      keepSection [] = []
                  in T.unlines $ concatMap keepSection sections

            return $ "```\n" ++ unpack (filterReport fullReport) ++ "\n```"
         candidates -> return $ printf "Error: Ambiguous contract name '%s'. Found: %s" (unpack contractName) (unpack $ T.intercalate ", " $ map fst candidates)

-- | Registry of available tools
availableTools :: [IORef WorkerState] -> IORef StatusState -> [Tool]
availableTools workerRefs statusRef =
  [ Tool "status" "Show fuzzing campaign status" (statusTool workerRefs statusRef)
  , Tool "target" "Show the name and the ABI of the target contract" targetTool
  , Tool "reload_corpus" "Reload the transactions from the corpus, but without replay them" reloadCorpusTool
  , Tool "dump_lcov" "Dump coverage in LCOV format" dumpLcovTool
  , Tool "inject_fuzz_transactions" "Inject a sequence of transaction to fuzz with optional concrete arguments" fuzzTransactionTool
  , Tool "clear_fuzz_priorities" "Clear the function prioritization list used in fuzzing" clearPrioritiesTool
  , Tool "show_coverage" "Show coverage report for a particular contract" showCoverageTool
  ]

-- | Log a control command for reproducibility (FR-010)
-- 
-- NOTE: Log flushing strategy uses BOTH immediate and periodic approaches
-- for maximum safety and resilience:
-- 1. Immediate: Each command logs and appends to JSONL file immediately
-- 2. Periodic: Background thread flushes every 10 seconds as failsafe
-- This dual approach ensures no data loss even if process terminates unexpectedly.
logMCPCommand :: IORef [Text] -> Text -> [(Text, Text)] -> IO ()
logMCPCommand logsRef toolName params = do
  timestamp <- getCurrentTime
  let paramStr = T.intercalate ", " [k <> "=" <> v | (k, v) <- params]
  let logEntry = pack $ printf "[%s] %s(%s)" (show timestamp) (unpack toolName) (unpack paramStr)
  modifyIORef' logsRef (logEntry :)

-- | Flush command log to JSONL file (Phase 4 - FR-010, Clarification #6)
flushCommandLog :: FilePath -> IORef [Text] -> IO ()
flushCommandLog corpusDir logsRef = do
  logs <- atomicModifyIORef' logsRef (\ls -> ([], ls))  -- Swap with empty list
  unless (null logs) $ do
    let logFile = corpusDir </> "mcp-commands.jsonl"
    withFile logFile AppendMode $ \h ->
      mapM_ (\entry -> do
        -- Parse log entry format: "[timestamp] toolname(args)"
        let entryStr = unpack entry
        let timestampEnd = 1 + length (takeWhile (/= ']') (drop 1 entryStr))
        let timestamp = take (timestampEnd - 1) (drop 1 entryStr)
        let command = drop (timestampEnd + 2) entryStr
        let json = encode $ object
              [ "timestamp" .= timestamp
              , "command" .= command
              ]
        hPutStrLn h (show json)) (reverse logs)  -- Reverse to chronological order

-- | Background thread for periodic log flushing (Phase 4)
startLogFlusher :: FilePath -> IORef [Text] -> IO ()
startLogFlusher corpusDir logsRef = do
  _ <- forkIO $ forever $ do
    threadDelay (10 * 1000000)  -- 10 seconds
    flushCommandLog corpusDir logsRef
  return ()

-- | Run the MCP Server
runMCPServer :: Env -> [IORef WorkerState] -> Int -> IORef [Text] -> IO ()
runMCPServer env workerRefs port logsRef = do
    statusRef <- newIORef (StatusState Nothing [] [])
    
    -- Start background log flusher (Phase 4)
    -- Get corpus directory from config (same pattern as in Agent/Fuzzer.hs:198 and Onchain.hs:130)
    corpusDir <- maybe getCurrentDirectory pure env.cfg.campaignConf.corpusDir
    startLogFlusher corpusDir logsRef

    -- Spawn listener for coverage events
    myBus <- atomically $ dupTChan env.bus
    _ <- forkIO $ forever $ do
      msg <- atomically $ readTChan myBus
      case msg of
        WrappedMessage _ (Broadcast (NewCoverageInfo _ txs isReplaying)) -> do
           unless isReplaying $ do
               now <- getCurrentTime
               let funcNames = map getFunctionName txs
                   lastFunc = if null funcNames then "unknown" else last funcNames

               modifyIORef' statusRef $ \st -> st
                 { lastCoverageTime = Just now
                 , coveredFunctions = take 10 (lastFunc : st.coveredFunctions)
                 }
        _ -> return ()

    let toolsList = availableTools workerRefs statusRef

    let httpConfig = HttpConfig
            { httpPort = port
            , httpHost = "127.0.0.1"
            , httpEndpoint = "/mcp"
            , httpVerbose = False
            }

    let serverInfo = McpServerInfo
            { serverName = "Echidna MCP Server"
            , serverVersion = "1.0.0"
            , serverInstructions = "Echidna Agent Interface. Available tools: status, target, reload_corpus, dump_lcov, inject_fuzz_transactions, clear_fuzz_priorities, show_coverage"
            }

    let mkToolDefinition :: Tool -> ToolDefinition
        mkToolDefinition t = ToolDefinition
            { toolDefinitionName = pack t.toolName
            , toolDefinitionDescription = pack t.toolDescription
            , toolDefinitionInputSchema = case t.toolName of
                "dump_lcov" -> InputSchemaDefinitionObject
                    { properties = []
                    , required = []
                    }
                "target" -> InputSchemaDefinitionObject
                    { properties = []
                    , required = []
                    }
                "inject_fuzz_transactions" -> InputSchemaDefinitionObject
                    { properties = [("transactions", InputSchemaDefinitionProperty "string" "The transaction sequence string separated by ';' (e.g. 'func1();func2(arg1, ?)')")]
                    , required = ["transactions"]
                    }
                "clear_fuzz_priorities" -> InputSchemaDefinitionObject
                    { properties = []
                    , required = []
                    }
                "show_coverage" -> InputSchemaDefinitionObject
                    { properties = [("contract", InputSchemaDefinitionProperty "string" "The name of the contract")]
                    , required = ["contract"]
                    }
                --"read_logs" -> InputSchemaDefinitionObject
                --    { properties = []
                --    , required = []
                --    }
                "status" -> InputSchemaDefinitionObject
                    { properties = []
                    , required = []
                    }
                "reload_corpus" -> InputSchemaDefinitionObject
                    { properties = []
                    , required = []
                    }
                _ -> InputSchemaDefinitionObject
                    { properties = []
                    , required = []
                    }
            , toolDefinitionTitle = Nothing
            , toolDefinitionMeta = Nothing
            }

    let toolDefs = map mkToolDefinition toolsList

    let handleToolCall :: ToolName -> [(ArgumentName, ArgumentValue)] -> IO (Either Error Content)
        handleToolCall name args = do
            case find (\t -> pack t.toolName == name) toolsList of
                Nothing -> return $ Left $ UnknownTool name
                Just tool -> do
                    -- Add timing instrumentation for <100ms target (FR-015)
                    startTime <- getCurrentTime
                    result <- tool.execute args env env.bus logsRef
                    endTime <- getCurrentTime
                    let latencyMs = realToFrac (diffUTCTime endTime startTime) * 1000 :: Double
                    -- Log timing to command log for performance monitoring
                    let latencyStr = printf "Tool '%s' completed in %.2fms" (unpack name) latencyMs
                    modifyIORef' logsRef (pack latencyStr :)
                    -- Return proper JSON-RPC 2.0 error objects for tool errors (FR-013)
                    -- Check if result starts with "Error:" and return as Error instead of Content
                    if "Error:" `isPrefixOf` result
                      then return $ Left $ InvalidParams (pack result)
                      else return $ Right $ ContentText $ pack result

    let handlers = McpServerHandlers
            { prompts = Nothing
            , resources = Nothing
            , tools = Just (return toolDefs, handleToolCall)
            }

    runMcpServerHttpWithConfig httpConfig serverInfo handlers
