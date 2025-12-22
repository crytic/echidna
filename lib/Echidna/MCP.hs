{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Echidna.MCP where

import Control.Concurrent (forkIO)
import Control.Monad (forever, unless)
import Control.Concurrent.STM
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

  -- Tests
  tests <- mapM readIORef env.testRefs
  let failedCount = length $ filter didFail tests
  let totalCount = length tests

  -- Optimization values
  let optTests = filter isOptimizationTest tests
      optValues = map (\(EchidnaTest {testType = ty, value = val}) -> printf "%s: %s" (show ty) (show val)) optTests
      optStr = if null optValues then "None" else intercalate ", " optValues

  let timeStr = case st.lastCoverageTime of
                  Nothing -> "Never"
                  Just t -> show (round (diffUTCTime now t) :: Integer)

      funcs = if null st.coveredFunctions
              then "None"
              else unpack $ T.intercalate "\n- " st.coveredFunctions

  return $ printf "Corpus Size: %d\nIterations: %d/%d\nCoverage: %d\nTests: %d/%d\nOptimization Values: %s\nTime since last coverage: %s\nLast 10 covered functions:\n- %s"
                  (Set.size c) iterations maxIterations covPoints failedCount totalCount optStr timeStr funcs

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
reloadCorpusTool :: ToolExecution
reloadCorpusTool _ env _ _ = do
  dir <- maybe getCurrentDirectory pure env.cfg.campaignConf.corpusDir
  loadedSeqs <- loadTxs dir -- returns [(FilePath, [Tx])]

  if null loadedSeqs
    then return "No transaction sequences found in corpus directory."
    else do
      currentCorpus <- readIORef env.corpusRef
      let existingTxs = Set.map snd currentCorpus

      let newSeqs = map snd loadedSeqs
      let uniqueNewSeqs = filter (`Set.notMember` existingTxs) newSeqs

      if null uniqueNewSeqs
        then return "No NEW transaction sequences found in corpus directory."
        else do
             let maxId = if Set.null currentCorpus
                         then 0
                         else fst (Set.findMax currentCorpus)

             let indexedNewSeqs = zip [maxId + 1 ..] uniqueNewSeqs
             let newCorpus = Set.union currentCorpus (Set.fromList indexedNewSeqs)

             atomicModifyIORef' env.corpusRef $ const (newCorpus, ())
             return $ printf "Reloaded %d new transaction sequences from %s" (length uniqueNewSeqs) dir

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
        then return $ "Error:\n" ++ unlines errors
        else do
          let nWorkers = getNFuzzWorkers env.cfg.campaignConf
              calcProb i
                | i == 0 = 0.0
                | nWorkers <= 2 = 0.2
                | otherwise = 0.2 + fromIntegral (i - 1) * (0.7 / fromIntegral (nWorkers - 2))

          mapM_ (\i -> atomically $ writeTChan bus (WrappedMessage AIId (ToFuzzer i (FuzzSequence seqPrototype (calcProb i))))) [0 .. nWorkers - 1]
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

            return $ unpack $ filterReport fullReport
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
                    result <- tool.execute args env env.bus logsRef
                    return $ Right $ ContentText $ pack result

    let handlers = McpServerHandlers
            { prompts = Nothing
            , resources = Nothing
            , tools = Just (return toolDefs, handleToolCall)
            }

    runMcpServerHttpWithConfig httpConfig serverInfo handlers
