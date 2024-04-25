{-# Language TemplateHaskell #-}
{-# Language UndecidableInstances #-}
{-# Language ImplicitParams #-}
{-# LANGUAGE TypeFamilies #-}
{-# Language DataKinds #-}
{-# Language RecordWildCards #-}

module Echidna.TTY where

import Prelude hiding (lookup, Word)

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.List

import EVM
import EVM.ABI (decodeAbiValue, emptyAbi, abiTypeSolidity, AbiType(..))
import EVM.SymExec (maxIterationsReached, symCalldata)
import EVM.Expr (simplify)
import EVM.Dapp (DappInfo(..), emptyDapp, dappInfo, {-Test, extractSig, Test(..),-} srcMap, unitTestMethods)
import Echidna.Debug
import EVM.Fetch (Fetcher)
import EVM.Fetch qualified as Fetch
import EVM.Format (showWordExact, showWordExplanation, contractNamePart,
  contractPathPart, showTraceTree, prettyIfConcreteWord, formatExpr)
--import EVM.Hexdump (prettyHex)
import EVM.Solvers (SolverGroup)
import EVM.Op
import EVM.Solidity hiding (storageLayout)
import EVM.Types hiding (padRight, Max, RealWorld)
import EVM.UnitTest
import EVM.Stepper (Stepper)
import EVM.Stepper qualified as Stepper
--import EVM.StorageLayout
--import EVM.TTYCenteredList qualified as Centered

import Optics.Core
import Optics.State
import Optics.TH

import Control.Monad.Operational qualified as Operational
import Control.Monad.State.Strict hiding (state)
import Control.Monad.ST (RealWorld)
import Data.Aeson.Optics
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.List (sort, find)
import Data.Maybe (isJust, fromJust, fromMaybe, isNothing)
import Data.Map (Map, insert, lookupLT, singleton, filter, (!?))
import Data.Map qualified as Map
import Data.Text (Text, pack)
import Data.Text qualified as T
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Vector qualified as Vec
import Data.Vector.Storable qualified as SVec
import Data.Version (showVersion)
import Graphics.Vty qualified as V
import System.Console.Haskeline qualified as Readline
--import Paths_hevm qualified as Paths
import Text.Wrap

data Name
  = AbiPane
  | StackPane
  | BytecodePane
  | TracePane
  | SolidityPane
  | TestPickerPane
  | BrowserPane
  | Pager
  deriving (Eq, Show, Ord)

type UiWidget = Widget Name

data UiVmState = UiVmState
  { vm         :: VM Concrete RealWorld
  , step       :: Int
  , snapshots  :: Map Int (VM Concrete RealWorld, Stepper Concrete RealWorld ())
  , stepper    :: Stepper Concrete RealWorld ()
  , showMemory :: Bool
  , testOpts   :: UnitTestOptions RealWorld
  }

data UiTestPickerState = UiTestPickerState
  { tests :: List Name (Text, Text)
  , dapp  :: DappInfo
  , opts  :: UnitTestOptions RealWorld
  }

data UiBrowserState = UiBrowserState
  { contracts :: List Name (Addr, Contract)
  , vm        :: UiVmState
  }

data UiState
  = ViewVm UiVmState
  | ViewContracts UiBrowserState
  | ViewPicker UiTestPickerState
  | ViewHelp UiVmState

makeFieldLabelsNoPrefix ''UiVmState
makeFieldLabelsNoPrefix ''UiTestPickerState
makeFieldLabelsNoPrefix ''UiBrowserState
makePrisms ''UiState

-- caching VM states lets us backstep efficiently
snapshotInterval :: Int
snapshotInterval = 50

type Pred a = a -> Bool

data StepMode
  = Step !Int                                -- ^ Run a specific number of steps
  | StepUntil (Pred (VM Concrete RealWorld)) -- ^ Finish when a VM predicate holds

-- | Each step command in the terminal should finish immediately
-- with one of these outcomes.
data Continuation a
     = Stopped a              -- ^ Program finished
     | Continue (Stepper Concrete RealWorld a)   -- ^ Took one step; more steps to go


-- | This turns a @Stepper@ into a state action usable
-- from within the TTY loop, yielding a @StepOutcome@ depending on the @StepMode@.
interpret
  :: (?maxIter :: Maybe Integer)
  => StepMode
  -> Stepper Concrete RealWorld a
  -> StateT UiVmState IO (Continuation a)
interpret mode =

  -- Like the similar interpreters in @EVM.UnitTest@ and @EVM.VMTest@,
  -- this one is implemented as an "operational monad interpreter".

  eval . Operational.view
  where
    eval
      :: Operational.ProgramView (Stepper.Action Concrete RealWorld) a
      -> StateT UiVmState IO (Continuation a)

    eval (Operational.Return x) =
      pure (Stopped x)

    eval (action Operational.:>>= k) =
      case action of

        {-Stepper.Run -> do
          -- Have we reached the final result of this action?
          use (#vm % #result) >>= \case
            Just _ -> do
              -- Yes, proceed with the next action.
              vm <- use #vm
              interpret mode (k vm)
            Nothing -> do
              -- No, keep performing the current action
              keepExecuting mode (Stepper.run >>= k)-}

        -- Stepper wants to keep executing?
        Stepper.Exec -> do
          -- Have we reached the final result of this action?
          use (#vm % #result) >>= \case
            Just r ->
              -- Yes, proceed with the next action.
              interpret mode (k r)
            Nothing -> do
              -- No, keep performing the current action
              keepExecuting mode (Stepper.exec >>= k)

        -- Stepper is waiting for user input from a query
        Stepper.Ask (PleaseChoosePath _ cont) -> do
          -- ensure we aren't stepping past max iterations
          vm <- use #vm
          case maxIterationsReached vm ?maxIter of
            Nothing -> pure $ Continue (k ())
            Just n -> interpret mode (Stepper.evm (cont (not n)) >>= k)

        -- Stepper wants to make a query and wait for the results?
        Stepper.Wait (PleaseAskSMT (Lit c) _ continue) ->
          interpret mode (Stepper.evm (continue (Case (c > 0))) >>= k)
        Stepper.Wait q -> do
          do m <- liftIO (?fetcher q)
             interpret mode (Stepper.evm m >>= k)

        -- Stepper wants to make a query and wait for the results?
        Stepper.IOAct q -> do
          Brick.zoom (toLensVL #vm) (StateT (runStateT q)) >>= interpret mode . k

        -- Stepper wants to modify the VM.
        Stepper.EVM m -> do
          vm <- use #vm
          let (r, vm1) = runState m vm
          assign #vm vm1
          interpret mode (Stepper.exec >> (k r))

keepExecuting :: (?maxIter :: Maybe Integer)
              => StepMode
              -> Stepper Concrete RealWorld a
              -> StateT UiVmState IO (Continuation a)
keepExecuting mode restart = case mode of
  Step 0 -> do
    -- We come here when we've continued while stepping,
    -- either from a query or from a return;
    -- we should pause here and wait for the user.
    pure (Continue restart)

  Step i -> do
    -- Run one instruction and recurse
    stepOneOpcode restart
    interpret (Step (i - 1)) restart

  StepUntil p -> do
    vm <- use #vm
    if p vm
      then
        interpret (Step 0) restart
      else do
        -- Run one instruction and recurse
        stepOneOpcode restart
        interpret (StepUntil p) restart

isUnitTestContract :: Text -> DappInfo -> Bool
isUnitTestContract name dapp =
  elem name (map fst dapp.unitTests)

mkVty :: IO V.Vty
mkVty = do
  vty <- V.mkVty V.defaultConfig
  V.setMode (V.outputIface vty) V.BracketedPaste True
  return vty

runFromVM :: SolverGroup -> Fetch.RpcInfo -> Maybe Integer -> DappInfo -> VM Concrete RealWorld -> IO (VM Concrete RealWorld)
runFromVM solvers rpcInfo maxIter' dappinfo vm = do

  let
    opts = UnitTestOptions
      { solvers       = solvers
      , rpcInfo       = rpcInfo
      , verbose       = Nothing
      , maxIter       = maxIter'
      , askSmtIters   = 1
      , smtTimeout    = Nothing
      --, smtDebug      = False
      , solver        = Nothing
      --, maxDepth      = Nothing
      , match         = ""
      --, fuzzRuns      = 1
      --, replay        = error "irrelevant"
      --, vmModifier    = id
      , testParams    = error "irrelevant"
      , dapp          = dappinfo
      , ffiAllowed    = False
      --, covMatch       = Nothing
      }
    ui0 = initUiVmState vm opts (void Stepper.execFully)

  v <- mkVty
  ui2 <- customMain v mkVty Nothing (app opts) (ViewVm ui0)
  case ui2 of
    ViewVm ui -> return ui.vm
    _ -> error "internal error: customMain returned prematurely"


initUiVmState :: VM Concrete RealWorld -> UnitTestOptions RealWorld -> Stepper Concrete RealWorld () -> UiVmState
initUiVmState vm0 opts script =
  UiVmState
    { vm           = vm0
    , stepper      = script
    , step         = 0
    , snapshots    = singleton 0 (vm0, script)
    , showMemory   = False
    , testOpts     = opts
    }

{-
-- filters out fuzztests, unless they have
-- explicitly been given an argument by `replay`
debuggableTests :: UnitTestOptions -> (Text, [(Test, [AbiType])]) -> [(Text, Text)]
debuggableTests UnitTestOptions{..} (contractname, tests) = undefined --case replay of
  --Nothing -> [(contractname, extractSig $ fst x) | x <- tests, not $ isFuzzTest x]
  --Just (sig, _) -> [(contractname, extractSig $ fst x) | x <- tests, extractSig (fst x) == sig]

isFuzzTest :: (Test, [AbiType]) -> Bool
isFuzzTest _ = False
--isFuzzTest (SymbolicTest _, _) = False
--isFuzzTest (ConcreteTest _, []) = False
--isFuzzTest (ConcreteTest _, _) = True
--isFuzzTest (InvariantTest _, _) = True

main :: UnitTestOptions -> FilePath -> Maybe BuildOutput -> IO ()
main opts root buildOutput = do
  let
    dapp = maybe emptyDapp (dappInfo root) buildOutput
    ui = ViewPicker $ UiTestPickerState
      { tests =
          list
            TestPickerPane
            (Vec.fromList
             (concatMap
              (debuggableTests opts)
              dapp.unitTests))
            1
      , dapp = dapp
      , opts = opts
      }
  v <- mkVty
  _ <- customMain v mkVty Nothing (app opts) (ui :: UiState)
  return ()
-}

takeStep
  :: (?maxIter :: Maybe Integer)
  => UiVmState
  -> StepMode
  -> EventM n UiState ()
takeStep ui mode =
  liftIO nxt >>= \case
    (Stopped (), _) ->
      pure ()
    (Continue steps, ui') ->
      put (ViewVm (ui' & set #stepper steps))
  where
    m = interpret mode ui.stepper
    nxt = runStateT m ui

backstepUntil
  :: (?maxIter :: Maybe Integer)
  => (UiVmState -> Pred (VM Concrete RealWorld)) -> EventM n UiState ()
backstepUntil p = get >>= \case
  ViewVm s ->
    case s.step of
      0 -> pure ()
      n -> do
        s1 <- liftIO $ backstep s
        let
          -- find a previous vm that satisfies the predicate
          snapshots' = Data.Map.filter (p s1 . fst) s1.snapshots
        case lookupLT n snapshots' of
          -- If no such vm exists, go to the beginning
          Nothing ->
            let
              (step', (vm', stepper')) = fromJust $ lookupLT (n - 1) s.snapshots
              s2 = s1
                & set #vm vm'
                & set (#vm % #cache) s1.vm.cache
                & set #step step'
                & set #stepper stepper'
            in takeStep s2 (Step 0)
          -- step until the predicate doesn't hold
          Just (step', (vm', stepper')) ->
            let
              s2 = s1
                & set #vm vm'
                & set (#vm % #cache) s1.vm.cache
                & set #step step'
                & set #stepper stepper'
            in takeStep s2 (StepUntil (not . p s1))
  _ -> pure ()

backstep
  :: (?maxIter :: Maybe Integer)
  => UiVmState -> IO UiVmState
backstep s =
  case s.step of
    -- We're already at the first step; ignore command.
    0 -> pure s
    -- To step backwards, we revert to the previous snapshot
    -- and execute n - 1 `mod` snapshotInterval steps from there.

    -- We keep the current cache so we don't have to redo
    -- any blocking queries, and also the memory view.
    n ->
      let
        (step, (vm, stepper)) = fromJust $ lookupLT n s.snapshots
        s1 = s
          & set #vm vm
          & set (#vm % #cache) s.vm.cache
          & set #step step
          & set #stepper stepper
        stepsToTake = n - step - 1

      in
        runStateT (interpret (Step stepsToTake) stepper) s1 >>= \case
          (Continue steps, ui') -> pure $ ui' & set #stepper steps
          _ -> error "unexpected end"

appEvent
  :: (?maxIter :: Maybe Integer) =>
  BrickEvent Name e ->
  EventM Name UiState ()

-- Contracts: Down - list down
appEvent (VtyEvent e@(V.EvKey V.KDown [])) = get >>= \case
  ViewContracts _s -> do
    Brick.zoom
      (traverseOf $ _ViewContracts % #contracts)
      (handleListEvent e)
    pure ()
  ViewPicker _s -> do
    Brick.zoom
      (traverseOf $ _ViewPicker % #tests)
      (handleListEvent e)
    pure()
  _ -> pure ()

-- Contracts: Up - list up
-- Page: Up - scroll
appEvent (VtyEvent e@(V.EvKey V.KUp [])) = get >>= \case
  ViewContracts _s -> do
    Brick.zoom
      (traverseOf $ _ViewContracts % #contracts)
      (handleListEvent e)
  ViewPicker _s -> do
    Brick.zoom
      (traverseOf $ _ViewPicker % #tests)
      (handleListEvent e)
    pure()
  _ -> pure ()

-- Vm Overview: Esc - return to test picker or exit
-- Any: Esc - return to Vm Overview or Exit
appEvent (VtyEvent (V.EvKey V.KEsc [])) = get >>= \case
  ViewVm s -> do
    let opts = s ^. #testOpts
        dapp = opts.dapp
        tests = concatMap (debuggableTests opts) dapp.unitTests
    case tests of
      [] -> halt
      ts ->
        put $ ViewPicker $ UiTestPickerState
          { tests = list TestPickerPane (Vec.fromList ts) 1
          , dapp  = dapp
          , opts  = opts
          }
  ViewHelp s -> put (ViewVm s)
  ViewContracts s -> put (ViewVm $ s ^. #vm)
  _ -> halt

-- Vm Overview: Enter - open contracts view
-- UnitTest Picker: Enter - select from list
appEvent (VtyEvent (V.EvKey V.KEnter [])) = get >>= \case
  ViewVm s ->
    put . ViewContracts $ UiBrowserState
      { contracts =
          list
            BrowserPane
            (Vec.fromList (Map.toList s.vm.env.contracts))
            2
      , vm = s
      }
  ViewPicker s ->
    case listSelectedElement s.tests of
      Nothing -> error "nothing selected"
      Just (_, x) -> do
        let initVm  = initialUiVmStateForTest s.opts x
        put (ViewVm initVm)
  _ -> pure ()

-- Vm Overview: m - toggle memory pane
appEvent (VtyEvent (V.EvKey (V.KChar 'm') [])) = get >>= \case
  ViewVm s -> put (ViewVm $ over #showMemory not s)
  _ -> pure ()

-- Vm Overview: h - open help view
appEvent (VtyEvent (V.EvKey (V.KChar 'h') [])) = get >>= \case
  ViewVm s -> put (ViewHelp s)
  _ -> pure ()

-- Vm Overview: spacebar - read input
appEvent (VtyEvent (V.EvKey (V.KChar ' ') [])) =
  let
    loop = do
      Readline.getInputLine "% " >>= \case
        Just hey -> Readline.outputStrLn hey
        Nothing  -> pure ()
      Readline.getInputLine "% " >>= \case
        Just hey' -> Readline.outputStrLn hey'
        Nothing   -> pure ()
   in do
    s <- get
    suspendAndResume $ do
      Readline.runInputT Readline.defaultSettings loop
      pure s

-- todo refactor to zipper step forward
-- Vm Overview: n - step
appEvent (VtyEvent (V.EvKey (V.KChar 'n') [])) = get >>= \case
  ViewVm s ->
    when (isNothing (s ^. #vm % #result)) $
      takeStep s (Step 1)
  _ -> pure ()

-- Vm Overview: N - step
appEvent (VtyEvent (V.EvKey (V.KChar 'N') [])) = get >>= \case
  ViewVm s ->
    when (isNothing (s ^. #vm % #result)) $
      takeStep s (StepUntil (isNextSourcePosition s))
  _ -> pure ()

-- Vm Overview: C-n - step
appEvent (VtyEvent (V.EvKey (V.KChar 'n') [V.MCtrl])) = get >>= \case
  ViewVm s ->
    when (isNothing (s ^. #vm % #result)) $
      takeStep s (StepUntil (isNextSourcePositionWithoutEntering s))
  _ -> pure ()

-- Vm Overview: e - step
appEvent (VtyEvent (V.EvKey (V.KChar 'e') [])) = get >>= \case
  ViewVm s ->
    when (isNothing (s ^. #vm % #result)) $
      takeStep s (StepUntil (isExecutionHalted s))
  _ -> pure ()

-- Vm Overview: a - step
appEvent (VtyEvent (V.EvKey (V.KChar 'a') [])) = get >>= \case
  ViewVm s ->
    -- We keep the current cache so we don't have to redo
    -- any blocking queries.
    let
      (vm, stepper) = fromJust (Map.lookup 0 s.snapshots)
      s' = s
        & set #vm vm
        & set (#vm % #cache) s.vm.cache
        & set #step 0
        & set #stepper stepper

    in takeStep s' (Step 0)
  _ -> pure ()

-- Vm Overview: p - backstep
appEvent (VtyEvent (V.EvKey (V.KChar 'p') [])) = get >>= \case
  ViewVm s ->
    case s.step of
      0 ->
        -- We're already at the first step; ignore command.
        pure ()
      n -> do
        -- To step backwards, we revert to the previous snapshot
        -- and execute n - 1 `mod` snapshotInterval steps from there.

        -- We keep the current cache so we don't have to redo
        -- any blocking queries, and also the memory view.
        let
          (step, (vm, stepper)) = fromJust $ lookupLT n s.snapshots
          s1 = s
            & set #vm vm -- set the vm to the one from the snapshot
            & set (#vm % #cache) s.vm.cache -- persist the cache
            & set #step step
            & set #stepper stepper
          stepsToTake = n - step - 1

        takeStep s1 (Step stepsToTake)
  _ -> pure ()

-- Vm Overview: P - backstep to previous source
appEvent (VtyEvent (V.EvKey (V.KChar 'P') [])) =
  backstepUntil isNextSourcePosition

-- Vm Overview: c-p - backstep to previous source avoiding CALL and CREATE
appEvent (VtyEvent (V.EvKey (V.KChar 'p') [V.MCtrl])) =
  backstepUntil isNextSourcePositionWithoutEntering

-- Vm Overview: 0 - choose no jump
appEvent (VtyEvent (V.EvKey (V.KChar '0') [])) = get >>= \case
  ViewVm s ->
    case view (#vm % #result) s of
      Just (HandleEffect (Choose (PleaseChoosePath _ contin))) ->
        takeStep (s & set #stepper (Stepper.evm (contin True) >> s.stepper))
          (Step 1)
      _ -> pure ()
  _ -> pure ()

-- Vm Overview: 1 - choose jump
appEvent (VtyEvent (V.EvKey (V.KChar '1') [])) = get >>= \case
  ViewVm s ->
    case s.vm.result of
      Just (HandleEffect (Choose (PleaseChoosePath _ contin))) ->
        takeStep (s & set #stepper (Stepper.evm (contin False) >> s.stepper))
          (Step 1)
      _ -> pure ()
  _ -> pure ()

-- Page: C-f - Page down
appEvent (VtyEvent (V.EvKey (V.KChar 'f') [V.MCtrl])) =
  vScrollPage (viewportScroll TracePane) Down

-- Page: C-b - Page up
appEvent (VtyEvent (V.EvKey (V.KChar 'b') [V.MCtrl])) =
  vScrollPage (viewportScroll TracePane) Up

-- UnitTest Picker: (main) - render list
appEvent (VtyEvent e) = do
  Brick.zoom (traverseOf (_ViewPicker % #tests))
    (handleListEvent e)

-- Default
appEvent _ = pure ()

app :: UnitTestOptions RealWorld -> App UiState () Name
app UnitTestOptions{..} =
  let ?maxIter = maxIter
  in App
  { appDraw = drawUi
  , appChooseCursor = neverShowCursor
  , appHandleEvent = appEvent
  , appStartEvent = pure ()
  , appAttrMap = const (attrMap V.defAttr myTheme)
  }

initialUiVmStateForTest
  :: UnitTestOptions RealWorld
  -> (Text, Text)
  -> UiVmState
initialUiVmStateForTest opts@UnitTestOptions{..} (theContractName, theTestName) = initUiVmState vm0 opts script
  where
    --cd = case test of
    --  SymbolicTest _ -> symCalldata theTestName types [] (AbstractBuf "txdata")
    --  _ -> (error "unreachable", error "unreachable")
    (test, types) = fromJust $ find (\(test',_) -> extractSig test' == theTestName) $ unitTestMethods testContract
    testContract = fromJust $ Map.lookup theContractName dapp.solcByName
    vm0 =
      initialUnitTestVm opts testContract
    script = do
      Stepper.evm . pushTrace . EntryTrace $
        "test " <> theTestName <> " (" <> theContractName <> ")"
      initializeUnitTest opts testContract
      let args = case replay of
                       Nothing -> emptyAbi
                       Just (sig, callData) ->
                         if theTestName == sig
                         then decodeAbiValue (AbiTupleType (Vec.fromList types)) callData
                         else emptyAbi
      void (runUnitTest opts theTestName args)
        
        {-ConcreteTest _ -> do
          let args = case replay of
                       Nothing -> emptyAbi
                       Just (sig, callData) ->
                         if theTestName == sig
                         then decodeAbiValue (AbiTupleType (Vec.fromList types)) callData
                         else emptyAbi
          void (runUnitTest opts theTestName args)
        SymbolicTest _ -> do
          void (execSymTest opts theTestName cd)
        InvariantTest _ -> do
          targets <- getTargetContracts opts
          let randomRun = initialExplorationStepper opts theTestName [] targets (fromMaybe 20 maxDepth)
          void $ case replay of
            Nothing -> randomRun
            Just (sig, cd') ->
              if theTestName == sig
              then initialExplorationStepper opts theTestName (decodeCalls cd') targets (length (decodeCalls cd'))
              else randomRun
       -}

myTheme :: [(AttrName, V.Attr)]
myTheme =
  [ (selectedAttr, V.defAttr `V.withStyle` V.standout)
  , (dimAttr, V.defAttr `V.withStyle` V.dim)
  , (borderAttr, V.defAttr `V.withStyle` V.dim)
  , (wordAttr, fg V.yellow)
  , (boldAttr, V.defAttr `V.withStyle` V.bold)
  , (activeAttr, V.defAttr `V.withStyle` V.standout)
  ]

drawUi :: UiState -> [UiWidget]
drawUi (ViewVm s) = drawVm s
drawUi (ViewPicker s) = drawTestPicker s
drawUi (ViewContracts s) = drawVmBrowser s
drawUi (ViewHelp _) = drawHelpView

drawHelpView :: [UiWidget]
drawHelpView =
    [ center . borderWithLabel version .
      padLeftRight 4 . padTopBottom 2 .  str $
        "Esc    Exit the debugger\n\n" <>
        "a      Step to start\n" <>
        "e      Step to end\n" <>
        "n      Step fwds by one instruction\n" <>
        "N      Step fwds to the next source position\n" <>
        "C-n    Step fwds to the next source position skipping CALL & CREATE\n" <>
        "p      Step back by one instruction\n\n" <>
        "P      Step back to the previous source position\n\n" <>
        "C-p    Step back to the previous source position skipping CALL & CREATE\n\n" <>
        "m      Toggle memory pane\n" <>
        "0      Choose the branch which does not jump \n" <>
        "1      Choose the branch which does jump \n" <>
        "Down   Step to next entry in the callstack / Scroll memory pane\n" <>
        "Up     Step to previous entry in the callstack / Scroll memory pane\n" <>
        "C-f    Page memory pane fwds\n" <>
        "C-b    Page memory pane back\n\n" <>
        "Enter  Contracts browser"
    ]
    where
      version =
        txt "Hevm " <+>
        --str (showVersion Paths.version) <+>
        txt " - Key bindings"

drawTestPicker :: UiTestPickerState -> [UiWidget]
drawTestPicker ui =
  [ center . borderWithLabel (txt "Unit tests") .
      hLimit 80 $ undefined
        --renderList
        --  (\selected (x, y) ->
        --     withHighlight selected $
        --       txt " Debug " <+> txt (contractNamePart x) <+> txt "::" <+> txt y)
        --  True
        --  ui.tests
  ]

drawVmBrowser :: UiBrowserState -> [UiWidget]
drawVmBrowser ui =
  [ hBox
      [ borderWithLabel (txt "Contracts") .
          hLimit 60 $
            renderList
              (\selected (k, c') ->
                 withHighlight selected . txt . mconcat $
                   [ fromMaybe "<unknown contract>" $
                       Map.lookup (maybeHash c') dapp.solcByHash <&> (.contractName) . snd
                   , "\n"
                   , "  ", pack (show k)
                   ])
              True
              ui.contracts
      , case snd <$> Map.lookup (maybeHash c) dapp.solcByHash of
          Nothing ->
            hBox
              [ borderWithLabel (txt "Contract information") . padBottom Max . padRight Max $ vBox
                  [ txt ("Codehash: " <> pack (show c.codehash))
                  , txt ("Nonce: "    <> showWordExact c.nonce)
                  , txt ("Balance: "  <> showWordExact c.balance)
                  --, txt ("Storage: "  <> storageDisplay (view storage c)) -- TODO: fix this
                  ]
                ]
          Just sol ->
            hBox
              [ borderWithLabel (txt "Contract information") . padBottom Max . padRight (Pad 2) $ vBox
                  [ txt "Name: " <+> txt (contractNamePart sol.contractName)
                  , txt "File: " <+> txt (contractPathPart sol.contractName)
                  , txt " "
                  , txt "Constructor inputs:"
                  , vBox . flip map sol.constructorInputs $
                      \(name, abiType) -> txt ("  " <> name <> ": " <> abiTypeSolidity abiType)
                  , txt "Public methods:"
                  , vBox . flip map (sort (Map.elems sol.abiMap)) $
                      \method -> txt ("  " <> method.methodSignature)
                  --, txt ("Storage:" <> storageDisplay (view storage c)) -- TODO: fix this
                  ]
              , borderWithLabel (txt "Storage slots") . padBottom Max . padRight Max $ vBox
                  (map txt (storageLayout dapp sol))
              ]
      ]
  ]
  where
    dapp = ui.vm.testOpts.dapp
    (_, (_, c)) = fromJust $ listSelectedElement ui.contracts
--        currentContract  = view (dappSolcByHash . ix ) dapp
    maybeHash ch = fromJust (error "Internal error: cannot find concrete codehash for partially symbolic code") (maybeLitWord ch.codehash)

drawVm :: UiVmState -> [UiWidget]
drawVm ui =
  -- EVM debugging needs a lot of space because of the 256-bit words
  -- in both the bytecode and the stack .
  --
  -- If on a very tall display, prefer a vertical layout.
  --
  -- Actually the horizontal layout would be preferrable if the display
  -- is both very tall and very wide, but this is okay for now.
  [ ifTallEnough (20 * 4)
      ( vBox
        [ vLimit 20 $ drawBytecodePane ui
        , vLimit 20 $ drawStackPane ui
        , drawSolidityPane ui
        , vLimit 20 $ drawTracePane ui
        , vLimit 2 drawHelpBar
        ]
      )
      ( vBox
        [ hBox
          [ vLimit 20 $ drawBytecodePane ui
          , vLimit 20 $ drawStackPane ui
          ]
        , hBox
          [ drawSolidityPane ui
          , drawTracePane ui
          ]
        , vLimit 2 drawHelpBar
        ]
      )
  ]

drawHelpBar :: UiWidget
drawHelpBar = hBorder <=> hCenter help
  where
    help =
      hBox (map (\(k, v) -> txt k <+> dim (txt (" (" <> v <> ")  "))) helps)

    helps =
      [
        ("n", "step")
      , ("p", "step back")
      , ("a", "step to start")
      , ("e", "step to end")
      , ("m", "toggle memory")
      , ("Esc", "exit")
      , ("h", "more help")
      ]

stepOneOpcode :: Stepper Concrete RealWorld a -> StateT UiVmState IO ()
stepOneOpcode restart = do
  n <- use #step
  when (n > 0 && n `mod` snapshotInterval == 0) $ do
    vm <- use #vm
    modifying #snapshots (insert n (vm, void restart))
  modifying #vm (execState exec1)
  modifying #step (+ 1)

isNewTraceAdded
  :: UiVmState -> Pred (VM Concrete RealWorld)
isNewTraceAdded ui vm =
  let
    currentTraceTree = length <$> traceForest ui.vm
    newTraceTree = length <$> traceForest vm
  in currentTraceTree /= newTraceTree

isNextSourcePosition
  :: UiVmState -> Pred (VM Concrete RealWorld)
isNextSourcePosition ui vm =
  let dapp = ui.testOpts.dapp
      initialPosition = currentSrcMap dapp ui.vm
  in currentSrcMap dapp vm /= initialPosition

isNextSourcePositionWithoutEntering
  :: UiVmState -> Pred (VM Concrete RealWorld)
isNextSourcePositionWithoutEntering ui vm =
  let
    dapp            = ui.testOpts.dapp
    vm0             = ui.vm
    initialPosition = currentSrcMap dapp vm0
    initialHeight   = length vm0.frames
  in
    case currentSrcMap dapp vm of
      Nothing ->
        False
      Just here ->
        let
          moved = Just here /= initialPosition
          deeper = length vm.frames > initialHeight
          boring =
            case srcMapCode dapp.sources here of
              Just bs ->
                BS.isPrefixOf "contract " bs
              Nothing ->
                True
        in
           moved && not deeper && not boring

isExecutionHalted :: UiVmState -> Pred (VM Concrete RealWorld)
isExecutionHalted _ vm = isJust vm.result

currentSrcMap :: DappInfo -> VM Concrete RealWorld -> Maybe SrcMap
currentSrcMap dapp vm = do
  this <- currentContract vm
  i <- this.opIxMap SVec.!? vm.state.pc
  srcMap dapp this i

drawStackPane :: UiVmState -> UiWidget
drawStackPane ui =
  let
    gasText = showWordExact (num ui.vm.state.gas)
    labelText = txt ("Gas available: " <> gasText <> "; stack:")
    stackList = list StackPane (Vec.fromList $ zip [(1 :: Int)..] (simplify <$> ui.vm.state.stack)) 2
  in hBorderWithLabel labelText <=>
    renderList
      (\_ (i, w) ->
         vBox
           [ withHighlight True (str ("#" ++ show i ++ " "))
               <+> ourWrap (Text.unpack $ prettyIfConcreteWord w)
           , dim (txt ("   " <> case maybeLitWord w of
                       Nothing -> ""
                       Just u -> showWordExplanation u ui.testOpts.dapp))
           ])
      False
      stackList

message :: VM Concrete RealWorld -> String
message vm =
  case vm.result of
    Just (VMSuccess (ConcreteBuf msg)) ->
      "VMSuccess: " <> (show $ ByteStringS msg)
    Just (VMSuccess (msg)) ->
      "VMSuccess: <symbolicbuffer> " <> (show msg)
    Just (VMFailure (Revert msg)) ->
      "VMFailure: " <> (show msg)
    Just (VMFailure err) ->
      "VMFailure: " <> show err
    Just (Unfinished p) ->
      "Could not continue execution: " <> show p
    Just (HandleEffect e) ->
      "Handling side effect: " <> show e
    Nothing ->
      "Executing EVM code in " <> show vm.state.contract


drawBytecodePane :: UiVmState -> UiWidget
drawBytecodePane ui =
  let
    vm = ui.vm
    move = maybe id listMoveTo $ vmOpIx vm
  in
    hBorderWithLabel (str $ message vm) <=> undefined
    {-Centered.renderList
      (\active x -> if not active
                    then withDefAttr dimAttr (opWidget x)
                    else withDefAttr boldAttr (opWidget x))
      False
      (move $ list BytecodePane
        (maybe mempty (.codeOps) (currentContract vm))
        1) -}


dim :: Widget n -> Widget n
dim = withDefAttr dimAttr

withHighlight :: Bool -> Widget n -> Widget n
withHighlight False = withDefAttr dimAttr
withHighlight True  = withDefAttr boldAttr

prettyIfConcrete :: Expr Buf -> String
prettyIfConcrete (ConcreteBuf x) = prettyHex 40 x
prettyIfConcrete x = T.unpack $ formatExpr $ simplify x

drawTracePane :: UiVmState -> UiWidget
drawTracePane s =
  let vm = s.vm
      dapp = s.testOpts.dapp
      traceList =
        list
          TracePane
          (Vec.fromList
            . Text.lines
            . showTraceTree dapp
            $ vm)
          1

  in case s.showMemory of
    True -> viewport TracePane Vertical $
        hBorderWithLabel (txt "Calldata")
        <=> ourWrap (prettyIfConcrete vm.state.calldata)
        <=> hBorderWithLabel (txt "Returndata")
        <=> ourWrap (prettyIfConcrete vm.state.returndata)
        <=> hBorderWithLabel (txt "Output")
        <=> ourWrap (maybe "" show vm.result)
        <=> hBorderWithLabel (txt "Cache")
        <=> ourWrap (show vm.cache.path)
        <=> hBorderWithLabel (txt "Path Conditions")
        <=> (ourWrap $ show $ vm.constraints)
        <=> hBorderWithLabel (txt "Memory")
        <=> (ourWrap (prettyIfConcrete vm.state.memory))
    False ->
      hBorderWithLabel (txt "Trace")
      <=> renderList
            (\_ x -> txt x)
            False
            (listMoveTo (length traceList) traceList)

ourWrap :: String -> Widget n
ourWrap = strWrapWith settings
  where
    settings = WrapSettings
      { preserveIndentation = True
      , breakLongWords = True
      , fillStrategy = NoFill
      , fillScope = FillAfterFirst
      }

solidityList :: VM Concrete RealWorld -> DappInfo -> List Name (Int, ByteString)
solidityList vm dapp =
  list SolidityPane
    (case currentSrcMap dapp vm of
        Nothing -> mempty
        Just x ->
          fromMaybe
            (error "Internal Error: unable to find line for source map")
            (preview (
              ix x.file
              % to (Vec.imap (,)))
            dapp.sources.lines))
    1

drawSolidityPane :: UiVmState -> UiWidget
drawSolidityPane ui =
  let dapp = ui.testOpts.dapp
      dappSrcs = dapp.sources
      vm = ui.vm
  in case currentSrcMap dapp vm of
    Nothing -> padBottom Max (hBorderWithLabel (txt "<no source map>"))
    Just sm ->
          let
            rows = dappSrcs.lines !? sm.file
            subrange :: Int -> Maybe (Int, Int)
            subrange i = do
              rs <- rows
              lineSubrange rs (sm.offset, sm.length) i
            fileName :: Maybe Text
            fileName = T.pack . fst <$> (dapp.sources.files !? sm.file)
            lineNo :: Maybe Int
            lineNo = ((\a -> Just (a - 1)) . snd) =<< srcMapCodePos dapp.sources sm
          in vBox
            [ hBorderWithLabel $
                txt (fromMaybe "<unknown>" fileName)
                  <+> str (":" ++ (maybe "?" show lineNo))

                  -- Show the AST node type if present
                  <+> txt (" (" <> fromMaybe "?"
                                    (dapp.astSrcMap sm
                                       >>= preview (key "name" % _String)) <> ")")
            , undefined {- Centered.renderList
                (\_ (i, line) ->
                   let s = case decodeUtf8 line of "" -> " "; y -> y
                   in case subrange i of
                        Nothing -> withHighlight False (txt s)
                        Just (a, b) ->
                          let (x, y, z) = ( Text.take a s
                                          , Text.take b (Text.drop a s)
                                          , Text.drop (a + b) s
                                          )
                          in hBox [ withHighlight False (txt x)
                                  , withHighlight True (txt y)
                                  , withHighlight False (txt z)
                                  ])
                False
                ((maybe id listMoveTo lineNo)
                  (solidityList vm dapp)) -}
            ]

ifTallEnough :: Int -> Widget n -> Widget n -> Widget n
ifTallEnough need w1 w2 =
  Widget Greedy Greedy $ do
    c <- getContext
    if view (lensVL availHeightL) c > need
      then render w1
      else render w2

opWidget :: (Integral a, Show a) => (a, Op) -> Widget n
opWidget = txt . pack . opString

selectedAttr :: AttrName; selectedAttr = attrName "selected"
dimAttr :: AttrName; dimAttr = attrName "dim"
wordAttr :: AttrName; wordAttr = attrName "word"
boldAttr :: AttrName; boldAttr = attrName "bold"
activeAttr :: AttrName; activeAttr = attrName "active"

