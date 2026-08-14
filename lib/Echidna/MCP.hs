-- | An MCP server exposing a running campaign, so that something outside it —
-- an agent, or a person driving one — can watch what the fuzzer is doing and
-- steer it.
--
-- The server is read-mostly: most tools answer from the 'Env' the workers
-- share, without touching them. The ones that do steer send a command over the
-- inter-worker bus, which a worker picks up between sequences; see
-- 'Echidna.Worker.Command'.
module Echidna.MCP (runMCPServer) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
  (atomically, dupTChan, newEmptyTMVarIO, readTChan, takeTMVar, writeTChan)
import Control.Monad (forM_, forever, void)
import Data.Aeson (Value(..), object, (.=))
import Data.Aeson.Text (encodeToLazyText)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.List (find, nub, sort)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isNothing, mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text, pack, unpack)
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Data.Version (showVersion)
import MCP.Server
  ( Content(ContentText), Error(UnknownTool), HttpConfig(..)
  , McpServerHandlers(..), McpServerInfo(..), Schema, SchemaType(..)
  , defaultHttpConfig, describedSchema, mcpApplication, mkToolDefinition
  , noHandlers, schema, toolError, toolResult )
import Network.Wai.Handler.Warp qualified as Warp
import Paths_echidna (version)
import System.Directory (getCurrentDirectory)
import System.Timeout (timeout)

import EVM.Dapp (DappInfo(..))
import EVM.Solidity (Method(..), SolcContract(..))

import Echidna (loadInitialCorpus)
import Echidna.MCP.Parse (parseFuzzSequence)
import Echidna.Output.Source (ppCoveredCode, saveLcovSnapshot)
import Echidna.Types.Campaign
  ( CampaignConf(..), SampleStats(..), WorkerState(..), getNFuzzWorkers
  , mergeSampleStats, takeStrict )
import Echidna.Types.Config (EConfig(..), Env(..))
import Echidna.Types.Corpus qualified as Corpus
import Echidna.Types.Coverage
  (CoverageFileType(..), coverageStats, mergeCoverageMaps)
import Echidna.Types.InterWorker
  ( AgentId(..), BroadcastMsg(..), FuzzerCmd(..), Message(..), Reply(..)
  , WrappedMessage(..) )
import Echidna.Types.Signature (SolCallPrototype)
import Echidna.Types.Solidity (SolConf(..))
import Echidna.Types.Test (EchidnaTest(..), didFail, isOptimizationTest)
import Echidna.Types.Tx (Tx(..), TxCall(..), maxGasPerBlock)
import Echidna.Types.World (World(..))

-- | One of the tools the server exposes.
data Tool = Tool
  { name :: Text
  , description :: Text
  , inputSchema :: Schema
  , run :: ToolRun
  }

-- | What a tool does when it is called: read whatever arguments it was given,
-- and answer with a report, or with what was wrong with the request. A 'Left'
-- reaches the client as a failed tool call rather than as a report saying it
-- failed, so a model does not have to read prose to notice.
type ToolRun = Map Text Value -> IO (Either Text Text)

-- | What the server remembers between calls: the coverage the campaign has
-- been finding. Only visible as it goes past on the bus, so something has to
-- be listening the whole time.
data StatusState = StatusState
  { lastCoverageTime :: !(Maybe UTCTime)
  , recentFunctions :: ![Text]
  }

-- | How many recently-covered function names to keep.
maxRecentFunctions :: Int
maxRecentFunctions = 10

-- | How long to wait for a worker to answer a command. Generous: a worker only
-- looks at the bus between sequences, and it has a replay of its own to run
-- before it can reply.
replyTimeout :: Int
replyTimeout = 300_000_000 -- 300 s

-- | Serve MCP over HTTP on the given port until the process ends. Blocks, so
-- run it on its own thread.
runMCPServer :: Env -> [IORef WorkerState] -> Int -> IO ()
runMCPServer env workerRefs port = do
  statusRef <- newIORef (StatusState Nothing [])
  void $ forkIO $ trackCoverage env statusRef

  let available = campaignTools env workerRefs statusRef
      handlers = noHandlers
        { tools = Just (const (pure (map definition available)), call available) }
  -- Warp directly rather than 'runMcpServerHttpWithConfig', which announces
  -- itself with a bare 'putStrLn'. The campaign says it is listening through a
  -- 'Echidna.Types.Worker.ServerLog' event instead, so the line is timestamped
  -- and prefixed like every other one, and Warp's settings stay in our hands.
  Warp.runSettings
    (Warp.setHost "127.0.0.1" $ Warp.setPort port Warp.defaultSettings)
    (mcpApplication httpConfig serverInfo handlers)
  where
  httpConfig = defaultHttpConfig
    { httpPort = port
    , httpHost = "127.0.0.1"
      -- The campaign is reachable from a browser on this machine, so reject
      -- any request carrying an Origin: a page cannot be allowed to drive the
      -- fuzzer through DNS rebinding. Clients that send no Origin at all --
      -- every CLI agent -- are unaffected.
    , httpAllowedOrigins = Just []
    }

  serverInfo = McpServerInfo
    { serverName = "Echidna"
    , serverVersion = pack (showVersion version)
    , serverInstructions =
        "Watch and steer a running Echidna fuzzing campaign. `status` reports \
        \what the campaign has reached so far and `target` what it can call. \
        \`inject_fuzz_transactions` biases the fuzzer towards a sequence of \
        \calls and `sample` starts recording what one function returns, both \
        \of which take effect within a few seconds and show up in `status`. \
        \`execute_sequence` answers what a specific sequence does without \
        \disturbing the campaign."
    }

  definition tool = mkToolDefinition tool.name tool.description tool.inputSchema

  call available _ctx wanted args =
    case find ((== wanted) . (.name)) available of
      Nothing -> pure $ Left $ UnknownTool wanted
      Just tool ->
        Right . either toolError (toolResult . pure . ContentText) <$> tool.run args

-- | Follow the bus, remembering the coverage that goes past.
trackCoverage :: Env -> IORef StatusState -> IO ()
trackCoverage env statusRef = do
  bus <- atomically $ dupTChan env.bus
  forever $ atomically (readTChan bus) >>= \case
    -- Coverage from replaying the corpus is coverage the campaign already had,
    -- so it says nothing about whether the fuzzer is still making progress.
    WrappedMessage _ (Broadcast (NewCoverageInfo _ txs False)) -> do
      now <- getCurrentTime
      modifyIORef' statusRef $ \tracked -> tracked
        { lastCoverageTime = Just now
          -- 'takeStrict' rather than 'take': this is updated on every coverage
          -- event and read only when a client asks, so a lazy tail would
          -- retain every sequence the campaign ever found coverage with.
        , recentFunctions =
            takeStrict maxRecentFunctions (lastCall txs : tracked.recentFunctions)
        }
    _ -> pure ()

-- | The function the last transaction of a sequence called, which is the one
-- that reached whatever the sequence reached.
lastCall :: [Tx] -> Text
lastCall txs = case reverse txs of
  tx : _ | SolCall (fname, _) <- tx.call -> fname
  _ -> "unknown"

campaignTools :: Env -> [IORef WorkerState] -> IORef StatusState -> [Tool]
campaignTools env workerRefs statusRef =
  [ Tool
      { name = "status"
      , description =
          "How the campaign is going, as JSON: corpus_size, iterations, \
          \iteration_limit, coverage_points, tests_failed, tests_total, \
          \optimization_values, time_since_last_coverage_sec, \
          \recent_covered_functions, and samples for whatever `sample` was \
          \asked to record."
      , inputSchema = noArgs
      , run = const (status env workerRefs statusRef)
      }
  , Tool
      { name = "target"
      , description = "The name of the contract under test and the functions \
          \it exposes, as canonical signatures."
      , inputSchema = noArgs
      , run = const (target env)
      }
  , Tool
      { name = "reload_corpus"
      , description = "Read the campaign's corpus directory again and add any \
          \sequence it does not already have. The sequences are not replayed, \
          \so they only take effect as the fuzzer mutates them."
      , inputSchema = noArgs
      , run = const (reloadCorpus env)
      }
  , Tool
      { name = "dump_lcov"
      , description = "Write the coverage reached so far to a timestamped \
          \LCOV file and return its path."
      , inputSchema = noArgs
      , run = const (dumpLcov env)
      }
  , Tool
      { name = "show_coverage"
      , description = "The coverage of one contract's source, line by line. \
          \Lines are marked * for executed, r for reverted, o for out of gas, \
          \e for any other error, and left blank when never reached."
      , inputSchema =
          argsSchema [("contract", "Contract name, e.g. 'Token'.")] []
      , run = showCoverage env
      }
  , Tool
      { name = "inject_fuzz_transactions"
      , description = "Spend part of the fuzzer's budget on a specific \
          \ordering of calls. Every fuzzing worker starts generating this \
          \sequence some of the time instead of mutating the corpus, with \
          \random calls mixed in, so what is pinned is the ordering rather \
          \than the whole sequence. Use it for an ordering worth reaching \
          \that the mutators are unlikely to stumble into."
      , inputSchema = argsSchema
          [ ( "transactions"
            , "Calls separated by ';', with '?' for an argument the fuzzer \
              \should choose, e.g. 'approve(0x10, ?); transferFrom(?, ?, 100)'."
            )
          ] []
      , run = injectFuzzTransactions env
      }
  , Tool
      { name = "clear_fuzz_priorities"
      , description = "Forget every sequence injected with \
          \inject_fuzz_transactions, returning the fuzzer to the corpus."
      , inputSchema = noArgs
      , run = const (clearFuzzPriorities env)
      }
  , Tool
      { name = "execute_sequence"
      , description = "Run a sequence of calls and report what each one did, \
          \as JSON: whether it completed, reverted or failed an assertion, the \
          \gas it burned and the events it emitted. The campaign is left \
          \exactly as it was — no coverage recorded, nothing added to the \
          \corpus, no test falsified — so this answers a question about the \
          \contract without changing what the fuzzer does next."
      , inputSchema = argsSchema
          [ ( "transactions"
            , "Calls separated by ';', all arguments concrete, e.g. \
              \'supply(1000); borrow(500)'."
            )
          ]
          [ ( "trace"
            , "Include the EVM trace of the last call in the report. \
              \Defaults to false."
            , SchemaBoolean
            )
          ]
      , run = executeSequence env
      }
  , Tool
      { name = "sample"
      , description = "Record what one function does as the campaign calls it: \
          \how often, how often it did not return successfully, the range its \
          \return value spanned, and a tail of recent revert summaries. \
          \Results appear under `samples` in `status`. Pass 'off' to stop \
          \recording."
      , inputSchema = argsSchema
          [ ( "function"
            , "Function name or canonical signature, e.g. 'totalSupply' or \
              \'transfer(address,uint256)'. Pass 'off' to stop sampling."
            )
          ] []
      , run = sample env
      }
  ]

-- | How the campaign is going.
status :: Env -> [IORef WorkerState] -> IORef StatusState -> IO (Either Text Text)
status env workerRefs statusRef = do
  corpus <- readIORef env.corpusRef
  workers <- traverse readIORef workerRefs
  tests <- traverse readIORef env.testRefs
  (points, _) <- coverageStats env.coverageRefInit env.coverageRefRuntime
  tracked <- readIORef statusRef
  now <- getCurrentTime

  let samples = Map.unionsWith mergeSampleStats (map (.sampledFunctions) workers)
      secondsSince t = round (diffUTCTime now t) :: Integer
  pure $ Right $ encodeJson $ object
    [ "corpus_size" .= Corpus.corpusSize corpus
    , "iterations" .= sum (map (.ncalls) workers)
    , "iteration_limit" .= env.cfg.campaignConf.testLimit
    , "coverage_points" .= points
    , "tests_failed" .= length (filter didFail tests)
    , "tests_total" .= length tests
    , "optimization_values" .= map optimizationJson (filter isOptimizationTest tests)
    , "time_since_last_coverage_sec" .= (secondsSince <$> tracked.lastCoverageTime)
    , "recent_covered_functions" .= tracked.recentFunctions
    , "samples" .= map sampleJson (Map.toList samples)
    ]
  where
  optimizationJson test = object
    [ "type" .= show test.testType
    , "value" .= show test.value
    ]

  sampleJson (sig, stats) = object
    [ "function" .= sig
    , "calls" .= stats.sampleCalls
    , "reverts" .= stats.sampleReverts
    , "return_range" .= (rangeJson <$> stats.sampleReturnRange)
    , "recent_reverts" .= stats.sampleRecentReverts
    ]

  rangeJson (lo, hi) = object ["min" .= show lo, "max" .= show hi]

-- | The contract under test and what can be called on it.
target :: Env -> IO (Either Text Text)
target env = pure $ case Map.toList targets of
  [] -> Left "No target contract found."
  (fullName, contract) : _ -> Right $
    "Contract: " <> fullName <> "\nFunctions:\n"
      <> T.unlines (map ("- " <>) (sort (signatures contract)))
  where
  -- A contract Echidna knows how to build calls against is one whose ABI made
  -- it into the world's signature map.
  targets = Map.filter
    ((`Map.member` env.world.highSignatureMap) . (.runtimeCodehash))
    env.dapp.solcByName
  signatures contract = map (.methodSignature) (Map.elems contract.abiMap)

-- | Pick up whatever was written to the corpus directory since the campaign
-- started.
reloadCorpus :: Env -> IO (Either Text Text)
reloadCorpus env = case env.cfg.campaignConf.corpusDir of
  Nothing -> pure $ Left "This campaign has no corpus directory to reload from."
  Just dir -> do
    -- Read the same places the campaign read at startup, rather than the corpus
    -- directory itself: what is directly in there is those subdirectories.
    loaded <- map snd <$> loadInitialCorpus env
    if null loaded
      then pure $ Left $ "No transaction sequences under " <> pack dir <> "."
      else do
        added <- atomicModifyIORef' env.corpusRef (addNew loaded)
        pure $ Right $
          "Added " <> showT added <> " of " <> showT (length loaded)
            <> " transaction sequences from " <> pack dir <> "."
  where
  -- Deciding what is new and adding it has to happen in one step: workers add
  -- to the corpus while this runs, and a read followed by a write would drop
  -- whatever they managed to add in between.
  addNew loaded corpus =
    let known = Set.map snd corpus
        fresh = Set.toList (Set.fromList loaded `Set.difference` known)
        -- The key is the weight the mutators draw a corpus entry by, and it
        -- grows as the campaign goes on, so a sequence arriving from disk takes
        -- the weight of the best one already there: as likely to be drawn as
        -- anything the campaign found itself, and no likelier. Never zero,
        -- which 'Echidna.Mutator.Corpus.selectFromCorpus' would never draw.
        weight = maybe 1 (max 1 . fst) (Set.lookupMax corpus)
    in (corpus <> Set.fromList (map (weight,) fresh), length fresh)

-- | Write out the coverage reached so far.
dumpLcov :: Env -> IO (Either Text Text)
dumpLcov env = do
  dir <- outputDir env
  file <- saveLcovSnapshot env dir
  pure $ Right $ "Wrote LCOV coverage to " <> pack file <> "."

-- | One contract's source, marked up with what the campaign reached.
showCoverage :: Env -> ToolRun
showCoverage env args = case Map.toList . matching <$> requiredText "contract" args of
  Left err -> pure $ Left err
  Right [] -> pure $ Left "No contract by that name."
  Right candidates@(_ : _ : _) -> pure $ Left $
    "That name matches " <> T.intercalate ", " (map fst candidates)
      <> ". Pass the full name."
  Right [(fullName, contract)] -> do
    covMap <- mergeCoverageMaps env.dapp env.coverageRefInit env.coverageRefRuntime
    let
      -- Report on every contract that has coverage, not just the one asked
      -- about: a line of a base contract is reached by executing the derived
      -- one, so restricting the report to this contract would show its
      -- inherited code as unreached. Filtering by file below cuts the report
      -- back down to the one that defines it.
      covered = filter ((`Map.member` covMap) . (.runtimeCodehash))
                       (Map.elems env.dapp.solcByName)
      contracts = if null covered then [contract] else covered
      report = ppCoveredCode Txt env.dapp.sources contracts covMap Nothing "" []
    pure $ Right $ "```\n" <> fileSection (sourceFile fullName) report <> "```"
  where
  -- solcByName keys are "path:Contract", so a bare contract name matches on
  -- the suffix.
  matching wanted = Map.filterWithKey
    (\k _ -> k == wanted || (":" <> wanted) `T.isSuffixOf` k)
    env.dapp.solcByName
  sourceFile = T.dropEnd 1 . fst . T.breakOnEnd ":"

-- | The part of a text coverage report that covers one file. The report is
-- sections of indented lines, each under an unindented file name.
fileSection :: Text -> Text -> Text
fileSection file report = T.unlines $ concat
  [ header : body
  | header : body <- sections (T.lines report)
  , header == file
  ]
  where
  sections [] = []
  sections (l:ls) =
    let (body, rest) = span (" " `T.isPrefixOf`) ls in (l : body) : sections rest

-- | Ask every fuzzing worker to spend part of its budget on a sequence.
injectFuzzTransactions :: Env -> ToolRun
injectFuzzTransactions env args = case prototypesFrom env args of
  Left err -> pure $ Left err
  Right prototypes -> do
    workers <- commandFuzzers env $ \workerId ->
      FuzzSequence prototypes (injectionProbability nFuzzWorkers workerId)
    pure $ Right $ "Fuzzing that sequence on " <> showT workers <> " workers."
  where nFuzzWorkers = getNFuzzWorkers env.cfg.campaignConf

-- | How often a worker should use an injected sequence rather than one of its
-- own. Worker 0 almost always uses it, so an answer arrives quickly, while the
-- rest are spread from barely to almost always: a campaign that abandons the
-- corpus entirely stops finding anything the injected ordering does not reach.
injectionProbability :: Int -> Int -> Double
injectionProbability nFuzzWorkers workerId
  | workerId == 0 = 0.9
  | nFuzzWorkers <= 2 = 0.2
  | otherwise =
      0.2 + fromIntegral (workerId - 1) * (0.7 / fromIntegral (nFuzzWorkers - 2))

-- | Return every fuzzing worker to the corpus.
clearFuzzPriorities :: Env -> IO (Either Text Text)
clearFuzzPriorities env = do
  workers <- commandFuzzers env (const ClearPrioritization)
  pure $ Right $ "Cleared injected sequences on " <> showT workers <> " workers."

-- | Replay a concrete sequence and report on what it did.
executeSequence :: Env -> ToolRun
executeSequence env args = case prototypesFrom env args >>= concreteTxs env of
  Left err -> pure $ Left err
  Right txs
    | getNFuzzWorkers env.cfg.campaignConf == 0 ->
        pure $ Left "This campaign has no fuzzing worker to replay a sequence on."
    | otherwise -> do
        report <- askFuzzer0 env $ ExecuteSequence txs (flag "trace" args)
        pure $ maybe (Left timedOut) Right report
  where
  timedOut = "The worker did not answer within "
    <> showT (replyTimeout `div` 1_000_000) <> " seconds."

-- | Start or stop recording what a function does.
sample :: Env -> ToolRun
sample env args = case requiredText "function" args of
  Left err -> pure $ Left err
  Right wanted
    | T.toLower wanted == "off" -> do
        workers <- commandFuzzers env (const ClearSampling)
        pure $ Right $ "Stopped sampling on " <> showT workers <> " workers."
    | otherwise -> case matching wanted of
        [] -> pure $ Left $ "No function '" <> wanted <> "' in the contract ABI."
        [sig] -> do
          workers <- commandFuzzers env (const (EnableSampling sig))
          pure $ Right $
            "Sampling " <> sig <> " on " <> showT workers <> " workers."
        sigs -> pure $ Left $
          "'" <> wanted <> "' matches " <> T.intercalate ", " sigs
            <> ". Pass the full signature."
  where
  matching wanted = nub
    [ m.methodSignature
    | m <- concatMap (Map.elems . (.abiMap)) (Map.elems env.dapp.solcByName)
    , m.methodSignature == wanted || m.name == wanted
    ]

-- | Send every fuzzing worker a command, built from its worker id. Answers
-- with how many were told.
commandFuzzers :: Env -> (Int -> FuzzerCmd) -> IO Int
commandFuzzers env mkCmd = do
  let workers = getNFuzzWorkers env.cfg.campaignConf
  forM_ [0 .. workers - 1] $ \workerId -> tell env (ToFuzzer workerId (mkCmd workerId))
  pure workers

-- | Ask worker 0 to run a command, and wait for its answer.
--
-- One worker rather than all of them: what is being asked does not depend on
-- which worker answers, and the worker stops fuzzing until it has. Worker 0 is
-- a fuzzing worker whenever there is one at all.
askFuzzer0 :: Env -> (Reply Text -> FuzzerCmd) -> IO (Maybe Text)
askFuzzer0 env mkCmd = do
  replyVar <- newEmptyTMVarIO
  tell env $ ToFuzzer 0 (mkCmd (Reply replyVar))
  timeout replyTimeout $ atomically $ takeTMVar replyVar

tell :: Env -> Message -> IO ()
tell env message =
  atomically $ writeTChan env.bus (WrappedMessage ServerId message)

-- | Parse the @transactions@ argument and check it against the ABI.
prototypesFrom :: Env -> Map Text Value -> Either Text [SolCallPrototype]
prototypesFrom env args = do
  sequenceText <- requiredText "transactions" args
  prototypes <- maybe (Left "Could not parse that transaction sequence.") Right $
    parseFuzzSequence (unpack sequenceText)
  case abiComplaints env prototypes of
    [] -> Right prototypes
    complaints -> Left (T.unlines complaints)

-- | What is wrong with a sequence, as far as the ABI is concerned. Checked by
-- name and arity only, the same way a prototype is resolved when it is fuzzed;
-- see 'Echidna.Transaction.matchingContracts'.
abiComplaints :: Env -> [SolCallPrototype] -> [Text]
abiComplaints env = mapMaybe complain
  where
  arities = Map.fromListWith (<>)
    [ (m.name, [length m.inputs]) | m <- Map.elems env.dapp.abiMap ]

  complain (fname, args) = case Map.lookup fname arities of
    Nothing -> Just $ "No function '" <> fname <> "' in the contract ABI."
    Just ns
      | length args `elem` ns -> Nothing
      | otherwise -> Just $
          "'" <> fname <> "' takes "
            <> T.intercalate " or " (map showT (sort (nub ns)))
            <> " arguments, not " <> showT (length args) <> "."

-- | Turn a fully concrete sequence into transactions to replay.
concreteTxs :: Env -> [SolCallPrototype] -> Either Text [Tx]
concreteTxs env prototypes
  | any (any isNothing . snd) prototypes =
      Left "Every argument has to be concrete here: '?' is only for \
           \inject_fuzz_transactions."
  | otherwise = Right (map toTx prototypes)
  where
  -- Every call goes to the contract under test. A sequence naming a function
  -- of some other deployed contract would be sent to the wrong address, but
  -- there is no way to spell an address in the sequence syntax to fix that
  -- with, and the ABI check above has already accepted it.
  toTx (fname, args) = Tx
    { call = SolCall (fname, catMaybes args)
    , src = fromMaybe 0 (Set.lookupMin env.cfg.solConf.sender)
    , dst = env.cfg.solConf.contractAddr
    , gas = maxGasPerBlock
    , gasprice = 0
    , value = 0
    , delay = (0, 0)
    }

-- | Where a file a client asked for goes: next to the corpus if there is one,
-- so it lands with everything else the campaign wrote, and in the working
-- directory otherwise. A campaign with no corpus directory still gets to answer
-- a request for a file.
outputDir :: Env -> IO FilePath
outputDir env = maybe getCurrentDirectory pure env.cfg.campaignConf.corpusDir

-- | A tool that takes no arguments.
noArgs :: Schema
noArgs = schema (SchemaObject [] [])

-- | An object schema: required string arguments, then optional ones of
-- whatever type.
argsSchema :: [(Text, Text)] -> [(Text, Text, SchemaType)] -> Schema
argsSchema required optional = schema $ SchemaObject
  (  [ (n, describedSchema d (SchemaString Nothing)) | (n, d) <- required ]
  <> [ (n, describedSchema d ty) | (n, d, ty) <- optional ]
  )
  (map fst required)

-- | A text argument that has to be there and non-blank.
requiredText :: Text -> Map Text Value -> Either Text Text
requiredText key args = case Map.lookup key args of
  Just (String t) | not (T.null (T.strip t)) -> Right (T.strip t)
  _ -> Left $ "The '" <> key <> "' argument is required."

-- | An optional flag, declared as a boolean but also accepted as the strings
-- clients send in its place.
flag :: Text -> Map Text Value -> Bool
flag key args = case Map.lookup key args of
  Just (Bool b) -> b
  Just (String t) -> T.toLower t == "true"
  _ -> False

encodeJson :: Value -> Text
encodeJson = LT.toStrict . encodeToLazyText

showT :: Show a => a -> Text
showT = pack . show
