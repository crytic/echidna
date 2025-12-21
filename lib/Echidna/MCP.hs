{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Echidna.MCP where

import Control.Concurrent.STM
import Data.IORef (readIORef, writeIORef, modifyIORef', IORef)
import Data.List (find, isPrefixOf)
import qualified Data.Maybe
import qualified Data.Set as Set
import Data.Sequence qualified as Seq
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Text.Printf (printf)
import qualified Data.Map as Map
import Data.Foldable (toList)
import Text.Read (readMaybe)
import System.Directory (getCurrentDirectory)
import Data.Char (isSpace)
import Data.Time.LocalTime (getZonedTime, zonedTimeToLocalTime)
import Data.Aeson (ToJSON(..), object, (.=))

import MCP.Server
import EVM.Dapp (DappInfo(..), srcMapCodePos)
import EVM.Solidity (SolcContract(..))
import EVM.Types (Addr)
import EVM.ABI (AbiValue(..))
import Echidna.Types.Tx (Tx(..), TxCall(..))
import Echidna.Types.Coverage (CoverageFileType(..), mergeCoverageMaps)
import Echidna.Output.Source (ppCoveredCode, saveLcovHook)

import Echidna.Types.Config (Env(..), EConfig(..))
import Echidna.Types.Campaign (getNFuzzWorkers, CampaignConf(..))
import Echidna.Types.InterWorker (Bus, Message(..), WrappedMessage(..), AgentId(..), FuzzerCmd(..))
import Echidna.Pretty (ppTx)

-- | Bus Communication Functions (T010, T012)

-- | Send a command to a specific fuzzer worker via the Bus
-- T010: Bus writer function (atomically writes to TChan, returns success)
sendFuzzerCmd :: AgentId -> FuzzerCmd -> Bus -> IO Bool
sendFuzzerCmd agentId cmd bus = do
  atomically $ writeTChan bus (WrappedMessage agentId (ToFuzzer 0 cmd))
  return True

-- | Log an MCP command execution for reproducibility (FR-010, T012)
-- Appends to mcpCommandLog with timestamp and tool name
logMCPCommand :: Text -> [(Text, Text)] -> Text -> Env -> IO ()
logMCPCommand toolName params result env = do
  timestamp <- zonedTimeToLocalTime <$> getZonedTime
  let paramStr = T.intercalate ", " [k <> "=" <> v | (k, v) <- params]
  let logEntry = T.concat
        [ T.pack (show timestamp)
        , " | Tool: ", toolName
        , " | Params: {", paramStr, "}"
        , " | Result: ", result
        ]
  modifyIORef' env.mcpCommandLog (logEntry :)

-- | Observability Tools (Phase 3: T015-T037)

-- | T015: Read recent campaign events from EventLog ring buffer
readLogsToolNew :: Env -> IO String
readLogsToolNew env = do
  eventLog <- readIORef env.eventLog
  let events = Seq.take 100 eventLog  -- Last 100 events (most recent first)
  let eventList = toList events
  let response = object
        [ "events" .= eventList
        , "count" .= length eventList
        , "timestamp" .= show (zonedTimeToLocalTime <$> getZonedTime)
        ]
  return $ show response

-- | T020: Get coverage statistics  
showCoverageToolNew :: Env -> IO String
showCoverageToolNew env = do
  covMap <- mergeCoverageMaps env.dapp env.coverageRefInit env.coverageRefRuntime
  let (totalPoints, numContracts) = Map.foldl' 
        (\(pts, contracts) cov -> (pts + Set.size cov, contracts + 1))
        (0, 0)
        covMap
  let response = object
        [ "points" .= totalPoints
        , "contracts" .= numContracts
        , "bytecode" .= totalPoints  -- Approximate
        ]
  return $ show response

-- | T024: Export coverage in LCOV format (in-memory, no disk write)
dumpLcovToolNew :: Env -> IO String
dumpLcovToolNew env = do
  let contracts = Map.elems env.dapp.solcByName
  dir <- maybe getCurrentDirectory pure env.cfg.campaignConf.corpusDir
  -- Note: saveLcovHook writes to disk, but for MCP we return in-memory content
  -- For now, return a placeholder. Full implementation would require refactoring saveLcovHook
  let response = object
        [ "format" .= ("lcov" :: Text)
        , "content" .= ("<LCOV data would be here - needs refactoring saveLcovHook>" :: Text)
        ]
  return $ show response

-- | T027: Get corpus size (count only)
getCorpusSizeToolNew :: Env -> IO String
getCorpusSizeToolNew env = do
  corpus <- readIORef env.corpusRef
  let response = object [ "size" .= Set.size corpus ]
  return $ show response

-- | T030: Inspect corpus transactions with pagination
inspectCorpusTransactionsToolNew :: Int -> Int -> Env -> IO String
inspectCorpusTransactionsToolNew page pageSize env = do
  corpus <- readIORef env.corpusRef
  let corpusList = Set.toList corpus
      total = length corpusList
      startIdx = page * pageSize
      pageItems = take pageSize $ drop startIdx corpusList
      
      formatSeq (idx, (_, txs)) = object
        [ "index" .= idx
        , "sequence_length" .= length txs
        , "coverage_delta" .= (0 :: Int)  -- Placeholder
        , "sample_calls" .= map formatTx (take 3 txs)
        ]
      
      formatTx tx = case tx.call of
        SolCall (fname, args) -> object ["function" .= fname, "args" .= show args]
        _ -> object ["function" .= ("constructor" :: Text)]
  
  let response = object
        [ "transactions" .= map formatSeq (zip [startIdx..] pageItems)
        , "total" .= total
        , "page" .= page
        ]
  return $ show response

-- | T034: Find transactions in corpus by function signature
findTransactionInCorpusToolNew :: Text -> Env -> IO String
findTransactionInCorpusToolNew funcSig env = do
  corpus <- readIORef env.corpusRef
  let corpusList = Set.toList corpus
      
      matchesSig tx = case tx.call of
        SolCall (fname, _) -> T.toLower funcSig `T.isInfixOf` T.toLower fname
        _ -> False
      
      matchingSeqs = filter (\(_, txs) -> any matchesSig txs) corpusList
      results = take 10 matchingSeqs  -- Limit to 10 results
      
      formatMatch (idx, (_, txs)) = object
        [ "index" .= idx
        , "sequence" .= map formatTx txs
        ]
      
      formatTx tx = case tx.call of
        SolCall (fname, args) -> object ["function" .= fname, "args" .= show args]
        _ -> object ["function" .= ("constructor" :: Text)]
  
  let response = object
        [ "found" .= not (null results)
        , "transactions" .= map formatMatch (zip [(0 :: Int)..] results)
        ]
  return $ show response

-- | Control Tools (Phase 4: T038-T054)

-- | T038-T042: Inject transaction sequence
injectTransactionToolNew :: [Text] -> Env -> IO String
injectTransactionToolNew txStrings env = do
  let parseTxString txStr = do
        let str = unpack txStr
        case parseCall str of
          Just (fname, args) -> 
            Just $ Tx (SolCall (pack fname, args)) 0x1000 0x2000 1000000 0 0 (0,0)
          Nothing -> Nothing
  
  let txs = Data.Maybe.mapMaybe parseTxString txStrings
  
  if null txs
    then return $ show $ object 
      [ "injected" .= False
      , "transaction_count" .= (0 :: Int)
      , "error" .= ("Failed to parse transactions" :: Text)
      ]
    else do
      -- T040: Send via Bus using InjectTransaction for each transaction
      let nWorkers = getNFuzzWorkers env.cfg.campaignConf
      mapM_ (\tx -> atomically $ writeTChan env.bus (WrappedMessage AIId (ToFuzzer 0 (InjectTransaction tx)))) txs
      
      -- T041: Log the injection
      let sampleSig = case head txs of
            Tx (SolCall (fname, _)) _ _ _ _ _ _ -> fname
            _ -> "unknown"
      logMCPCommand "inject_transaction" 
        [("tx_count", pack $ show $ length txs), ("sample_sig", sampleSig)]
        "injected"
        env
      
      -- T042: Return response
      let response = object
            [ "injected" .= True
            , "transaction_count" .= length txs
            , "worker_id" .= (0 :: Int)
            ]
      return $ show response

-- | T044-T048: Prioritize function
prioritizeFunctionToolNew :: Text -> Env -> IO String
prioritizeFunctionToolNew funcSig env = do
  let nWorkers = getNFuzzWorkers env.cfg.campaignConf
  
  -- T045: Send PrioritizeFunction to all workers
  mapM_ (\i -> atomically $ writeTChan env.bus 
    (WrappedMessage AIId (ToFuzzer i (PrioritizeFunction (unpack funcSig))))) 
    [0 .. nWorkers - 1]
  
  -- T047: Log the prioritization
  logMCPCommand "prioritize_function" 
    [("function", funcSig)]
    "priority_set"
    env
  
  -- T048: Return response
  let response = object
        [ "prioritized" .= True
        , "function_signature" .= funcSig
        , "worker_ids" .= [0 .. nWorkers - 1]
        ]
  return $ show response

-- | T050-T053: Clear function priorities
clearPrioritiesToolNew :: Env -> IO String
clearPrioritiesToolNew env = do
  let nWorkers = getNFuzzWorkers env.cfg.campaignConf
  
  -- T051: Send ClearPrioritization to all workers
  mapM_ (\i -> atomically $ writeTChan env.bus 
    (WrappedMessage AIId (ToFuzzer i ClearPrioritization))) 
    [0 .. nWorkers - 1]
  
  -- T052: Log the clearing
  logMCPCommand "clear_priorities" [] "cleared" env
  
  -- T053: Return response
  let response = object
        [ "cleared" .= True
        , "worker_ids" .= [0 .. nWorkers - 1]
        ]
  return $ show response

-- | MCP Tool Definition
-- Simulates the definition of a tool exposed by an MCP server.
type ToolExecution = [(Text, Text)] -> Env -> Bus -> IORef [Text] -> IO String

data Tool = Tool
  { toolName :: String
  , toolDescription :: String
  , execute :: ToolExecution
  }

-- | Implementation of read_corpus tool
readCorpusTool :: ToolExecution
readCorpusTool _ env _ _ = do
  c <- readIORef env.corpusRef
  return $ printf "Corpus Size: %d" (Set.size c)

-- | Implementation of inspect_corpus_transactions tool
inspectCorpusTransactionsTool :: ToolExecution
inspectCorpusTransactionsTool args env _ _ = do
  let page = case lookup "page" args of
               Just p -> Data.Maybe.fromMaybe 1 (readMaybe (unpack p))
               Nothing -> 1
      pageSize = 5
  c <- readIORef env.corpusRef
  let corpusList = Set.toList c
      startIndex = (page - 1) * pageSize
      pageItems = take pageSize $ drop startIndex corpusList
      
      ppSequence (i, txs) = 
        printf "Sequence (value: %d):\n%s" i (unlines $ map (ppTx Map.empty) txs)

  return $ if null pageItems 
           then "No more transactions found."
           else intercalate "\n" (map ppSequence pageItems)
    where
      intercalate _ [] = ""
      intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

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

parseArg :: String -> Maybe AbiValue
parseArg s = 
   let s' = trim s
   in if "0x" `isPrefixOf` s'
      then AbiAddress . fromIntegral <$> (readMaybe s' :: Maybe Integer)
      else AbiUInt 256 . fromIntegral <$> (readMaybe s' :: Maybe Integer)

parseCall :: String -> Maybe (String, [AbiValue])
parseCall s = do
   let (fname, rest) = break (== '(') s
   if null rest then Nothing else do
     let argsS = take (length rest - 2) (drop 1 rest) -- remove parens
     let argParts = if all isSpace argsS then [] else splitOn ',' argsS
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

-- | Implementation of inject_transaction tool
injectTransactionTool :: ToolExecution
injectTransactionTool args env bus _ = do
  let idx = case lookup "sequence_index" args of
              Just i -> Data.Maybe.fromMaybe 0 (readMaybe (unpack i))
              Nothing -> 0
      pos = case lookup "position" args of
              Just p -> Data.Maybe.fromMaybe 0 (readMaybe (unpack p))
              Nothing -> 0
      txStr = maybe "" unpack (lookup "transaction" args)
  
  c <- readIORef env.corpusRef
  let corpusList = Set.toList c
  
  if idx < 0 || idx >= length corpusList
    then return "Error: Invalid sequence index."
    else do
      let (_, originalSeq) = corpusList !! idx
      if pos < 0 || pos > length originalSeq
        then return "Error: Invalid position."
        else do
          let contextTx = case originalSeq of
                            [] -> Nothing
                            (x:xs) -> Just (if pos > 0 && pos <= length (x:xs) 
                                            then (x:xs) !! (pos - 1) 
                                            else x)
          case parseTx contextTx txStr of
            Nothing -> return "Error: Failed to parse transaction string."
            Just newTx -> do
               let newSeq = take pos originalSeq ++ [newTx]
               replyVar <- newEmptyTMVarIO
               atomically $ writeTChan bus (WrappedMessage AIId (ToFuzzer 0 (ExecuteSequence newSeq (Just replyVar))))
               
               -- Wait for reply
               found <- atomically $ takeTMVar replyVar
               if found
                 then return "Transaction injected and NEW coverage found!"
                 else return "Transaction injected but NO new coverage found."

-- | Implementation of dump_lcov tool
dumpLcovTool :: ToolExecution
dumpLcovTool _ env _ _ = do
  let contracts = Map.elems env.dapp.solcByName
  dir <- maybe getCurrentDirectory pure env.cfg.campaignConf.corpusDir
  filename <- saveLcovHook env dir env.sourceCache contracts
  return $ "Dumped LCOV coverage to " ++ filename

-- | Implementation of prioritize_function tool
prioritizeFunctionTool :: ToolExecution
prioritizeFunctionTool args env bus _ = do
  let msg = Data.Maybe.fromMaybe "" (lookup "function" args)
  let nWorkers = getNFuzzWorkers env.cfg.campaignConf
  mapM_ (\i -> atomically $ writeTChan bus (WrappedMessage AIId (ToFuzzer i (PrioritizeFunction (unpack msg))))) [0 .. nWorkers - 1]
  return $ printf "Requested prioritization of function '%s' on %d fuzzers" (unpack msg) nWorkers

-- | Implementation of clear_priorities tool
clearPrioritiesTool :: ToolExecution
clearPrioritiesTool _ env bus _ = do
  let nWorkers = getNFuzzWorkers env.cfg.campaignConf
  mapM_ (\i -> atomically $ writeTChan bus (WrappedMessage AIId (ToFuzzer i ClearPrioritization))) [0 .. nWorkers - 1]
  return $ printf "Requested clearing priorities on %d fuzzers" nWorkers

-- | Implementation of read_logs tool
readLogsTool :: ToolExecution
readLogsTool _ _ _ logsRef = do
  logs <- readIORef logsRef
  return $ unpack $ T.unlines $ reverse logs

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
         [(_, solc)] -> do
            covMap <- mergeCoverageMaps dapp env.coverageRefInit env.coverageRefRuntime
            let sc = env.sourceCache

            -- Identify relevant files from the requested contract's source maps
            -- This ensures we include all files that define the contract and its dependencies,
            -- even if they are not directly covered or if coverage is recorded against a child contract.
            let getContractFiles c =
                    let srcMaps = toList c.runtimeSrcmap ++ toList c.creationSrcmap
                        resolve srcMap = fst <$> srcMapCodePos sc srcMap
                    in Set.fromList $ Data.Maybe.mapMaybe resolve srcMaps

            let relevantFiles = getContractFiles solc

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

            return $ unpack $ filterReport fullReport
         candidates -> return $ printf "Error: Ambiguous contract name '%s'. Found: %s" (unpack contractName) (unpack $ T.intercalate ", " $ map fst candidates)

-- | Registry of available tools
availableTools :: [Tool]
availableTools =
  [ Tool "read_corpus" "Read the current corpus size" readCorpusTool
  , Tool "inspect_corpus_transactions" "Browse the corpus transactions" inspectCorpusTransactionsTool
  , Tool "inject_transaction" "Inject a transaction into a sequence and execute it" injectTransactionTool
  , Tool "dump_lcov" "Dump coverage in LCOV format" dumpLcovTool
  , Tool "prioritize_function" "Prioritize a function for fuzzing" prioritizeFunctionTool
  , Tool "clear_priorities" "Clear the function prioritization list" clearPrioritiesTool
  , Tool "read_logs" "Read the last 100 log messages" readLogsTool
  , Tool "show_coverage" "Show coverage report for a particular contract" showCoverageTool
  ]

-- | Run the MCP Server
runMCPServer :: Env -> Int -> IORef [Text] -> IO ()
runMCPServer env port logsRef = do
    let httpConfig = HttpConfig
            { httpPort = port
            , httpHost = "127.0.0.1"
            , httpEndpoint = "/mcp"
            , httpVerbose = False
            }

    let serverInfo = McpServerInfo
            { serverName = "Echidna MCP Server"
            , serverVersion = "1.0.0"
            , serverInstructions = "Echidna Agent Interface. Available tools: read_corpus, inspect_corpus_transactions, dump_lcov, prioritize_function, clear_priorities, read_logs, show_coverage"
            }

    let mkToolDefinition :: Tool -> ToolDefinition
        mkToolDefinition t = ToolDefinition
            { toolDefinitionName = pack t.toolName
            , toolDefinitionDescription = pack t.toolDescription
            , toolDefinitionInputSchema = case t.toolName of
                "inspect_corpus_transactions" -> InputSchemaDefinitionObject
                    { properties = [("page", InputSchemaDefinitionProperty "string" "The page number (default 1)")]
                    , required = ["page"]
                    }
                "inject_transaction" -> InputSchemaDefinitionObject
                    { properties = 
                        [ ("sequence_index", InputSchemaDefinitionProperty "string" "The index of the sequence in the corpus")
                        , ("position", InputSchemaDefinitionProperty "string" "The position to insert the transaction at")
                        , ("transaction", InputSchemaDefinitionProperty "string" "The transaction string (e.g. 'func(arg1, arg2)')")
                        ]
                    , required = ["sequence_index", "position", "transaction"]
                    }
                "dump_lcov" -> InputSchemaDefinitionObject
                    { properties = []
                    , required = []
                    }
                "prioritize_function" -> InputSchemaDefinitionObject
                    { properties = [("function", InputSchemaDefinitionProperty "string" "The name of the function to prioritize")]
                    , required = ["function"]
                    }
                "clear_priorities" -> InputSchemaDefinitionObject
                    { properties = []
                    , required = []
                    }
                "show_coverage" -> InputSchemaDefinitionObject
                    { properties = [("contract", InputSchemaDefinitionProperty "string" "The name of the contract")]
                    , required = ["contract"]
                    }
                "read_logs" -> InputSchemaDefinitionObject
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

    let toolDefs = map mkToolDefinition availableTools

    let handleToolCall :: ToolName -> [(ArgumentName, ArgumentValue)] -> IO (Either Error Content)
        handleToolCall name args = do
            case find (\t -> pack t.toolName == name) availableTools of
                Nothing -> return $ Left $ UnknownTool name
                Just tool -> do
                    result <- tool.execute args env env.bus logsRef
                    return $ Right $ ContentText $ pack result

    let handlers = McpServerHandlers
            { prompts = Nothing
            , resources = Nothing
            , tools = Just (return toolDefs, handleToolCall)
            }

    runMcpServerHttpWithConfig httpConfig serverInfo handlers
