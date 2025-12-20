{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Echidna.MCP where

import Control.Concurrent (forkIO)
import Control.Monad (forever, unless)
import Control.Concurrent.STM
import Data.IORef (readIORef, modifyIORef', newIORef, IORef)
import Data.List (find, isPrefixOf)
import qualified Data.Maybe
import qualified Data.Set as Set
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import Text.Printf (printf)
import qualified Data.Map as Map
import Text.Read (readMaybe)
import System.Directory (getCurrentDirectory)
import Data.Char (isSpace)

import MCP.Server
import EVM.Dapp (DappInfo(..))
import EVM.Solidity (SolcContract(..))
import EVM.Types (Addr)
import EVM.ABI (AbiValue(..))
import Echidna.Types.Tx (Tx(..), TxCall(..))
import Echidna.Types.Coverage (CoverageFileType(..), mergeCoverageMaps, coverageStats)
import Echidna.Output.Source (ppCoveredCode, saveLcovHook)

import Echidna.Types.Config (Env(..), EConfig(..))
import Echidna.Types.Campaign (getNFuzzWorkers, CampaignConf(..), WorkerState(..))
import Echidna.Types.InterWorker (Bus, Message(..), WrappedMessage(..), AgentId(..), FuzzerCmd(..), BroadcastMsg(..))
import Echidna.Pretty (ppTx)

-- | Status state to track coverage info
data StatusState = StatusState
  { lastCoverageTime :: Maybe UTCTime
  , coveredFunctions :: [Text]
  }

-- | MCP Tool Definition
-- Simulates the definition of a tool exposed by an MCP server.
type ToolExecution = [(Text, Text)] -> Env -> Bus -> IORef [Text] -> IO String

data Tool = Tool
  { toolName :: String
  , toolDescription :: String
  , execute :: ToolExecution
  }

-- | Helper to get function name from Tx
getFunctionName :: Tx -> Text
getFunctionName tx = case tx.call of
  SolCall (name, _) -> name
  _ -> "unknown"

-- | Implementation of status tool
statusTool :: [IORef WorkerState] -> IORef StatusState -> ToolExecution
statusTool workerRefs statusRef _ env _ _ = do
  c <- readIORef env.corpusRef
  st <- readIORef statusRef
  now <- getCurrentTime

  -- Iterations
  workers <- mapM readIORef workerRefs
  let iterations = sum $ map (.ncalls) workers
  let maxIterations = env.cfg.campaignConf.testLimit

  -- Coverage
  (covPoints, _) <- coverageStats env.coverageRefInit env.coverageRefRuntime

  let timeStr = case st.lastCoverageTime of
                  Nothing -> "Never"
                  Just t -> show (round (diffUTCTime now t) :: Integer)

      funcs = if null st.coveredFunctions
              then "None"
              else unpack $ T.intercalate "\n- " st.coveredFunctions

  return $ printf "Corpus Size: %d\nIterations: %d/%d\nCoverage: %d\nTime since last coverage: %s\nLast 10 covered functions:\n- %s"
                  (Set.size c) iterations maxIterations covPoints timeStr funcs

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
     let argParts = if all isSpace argsS then [] else splitOn ',' argsS
     args <- mapM parseFuzzArg argParts
     return (pack fname, args)

parseFuzzSequence :: String -> Maybe [(Text, [Maybe AbiValue])]
parseFuzzSequence s = mapM (parseFuzzCall . trim) (splitOn ';' s)

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

-- | Implementation of inject_fuzz_transactions tool
fuzzTransactionTool :: ToolExecution
fuzzTransactionTool args env bus _ = do
  let txStr = Data.Maybe.fromMaybe "" (lookup "transactions" args)
  case parseFuzzSequence (unpack txStr) of
    Nothing -> return "Error: Failed to parse transaction sequence string."
    Just seqPrototype -> do
      let nWorkers = getNFuzzWorkers env.cfg.campaignConf
      mapM_ (\i -> atomically $ writeTChan bus (WrappedMessage AIId (ToFuzzer i (FuzzSequence seqPrototype)))) [0 .. nWorkers - 1]
      return $ printf "Requested fuzzing of transaction sequence '%s' on %d fuzzers" (unpack txStr) nWorkers

-- | Implementation of clear_fuzz_priorities tool
clearPrioritiesTool :: ToolExecution
clearPrioritiesTool _ env bus _ = do
  let nWorkers = getNFuzzWorkers env.cfg.campaignConf
  mapM_ (\i -> atomically $ writeTChan bus (WrappedMessage AIId (ToFuzzer i ClearPrioritization))) [0 .. nWorkers - 1]
  return $ printf "Requested clearing priorities on %d fuzzers" nWorkers

-- | Implementation of read_logs tool
readLogsTool :: ToolExecution
readLogsTool _ _ _ logsRef = do
  logs <- readIORef logsRef
  -- Get last 100 logs
  -- logs is [Newest, ..., Oldest]
  -- We want to take the 100 newest, and show them in chronological order
  let logsToShow = reverse $ take 100 logs
  return $ unpack $ T.unlines logsToShow

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

            return $ unpack $ filterReport fullReport
         candidates -> return $ printf "Error: Ambiguous contract name '%s'. Found: %s" (unpack contractName) (unpack $ T.intercalate ", " $ map fst candidates)

-- | Registry of available tools
availableTools :: [IORef WorkerState] -> IORef StatusState -> [Tool]
availableTools workerRefs statusRef =
  [ Tool "status" "Show fuzzing campaign status" (statusTool workerRefs statusRef)
  --, Tool "inspect_corpus_transactions" "Browse the corpus transactions" inspectCorpusTransactionsTool
  --, Tool "inject_transaction" "Inject a transaction into a sequence and execute it" injectTransactionTool
  , Tool "dump_lcov" "Dump coverage in LCOV format" dumpLcovTool
  , Tool "inject_fuzz_transactions" "Inject a sequence of transaction to fuzz with optional concrete arguments" fuzzTransactionTool
  , Tool "clear_fuzz_priorities" "Clear the function prioritization list used in fuzzing" clearPrioritiesTool
  --, Tool "read_logs" "Read the last 100 log messages" readLogsTool
  , Tool "show_coverage" "Show coverage report for a particular contract" showCoverageTool
  ]

-- | Run the MCP Server
runMCPServer :: Env -> [IORef WorkerState] -> Int -> IORef [Text] -> IO ()
runMCPServer env workerRefs port logsRef = do
    statusRef <- newIORef (StatusState Nothing [])

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
            , serverInstructions = "Echidna Agent Interface. Available tools: status, dump_lcov, inject_fuzz_transactions, clear_fuzz_priorities, show_coverage"
            }

    let mkToolDefinition :: Tool -> ToolDefinition
        mkToolDefinition t = ToolDefinition
            { toolDefinitionName = pack t.toolName
            , toolDefinitionDescription = pack t.toolDescription
            , toolDefinitionInputSchema = case t.toolName of
                -- "inspect_corpus_transactions" -> InputSchemaDefinitionObject
                --     { properties = [("page", InputSchemaDefinitionProperty "string" "The page number (default 1)")]
                --     , required = ["page"]
                --     }
                -- "inject_transaction" -> InputSchemaDefinitionObject
                --     { properties =
                --         [ ("sequence_index", InputSchemaDefinitionProperty "string" "The index of the sequence in the corpus")
                --         , ("position", InputSchemaDefinitionProperty "string" "The position to insert the transaction at")
                --         , ("transaction", InputSchemaDefinitionProperty "string" "The transaction string (e.g. 'func(arg1, arg2)')")
                --         ]
                --     , required = ["sequence_index", "position", "transaction"]
                --     }
                "dump_lcov" -> InputSchemaDefinitionObject
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
                    result <- tool.execute args env env.bus logsRef
                    return $ Right $ ContentText $ pack result

    let handlers = McpServerHandlers
            { prompts = Nothing
            , resources = Nothing
            , tools = Just (return toolDefs, handleToolCall)
            }

    runMcpServerHttpWithConfig httpConfig serverInfo handlers
