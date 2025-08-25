{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Echidna.SymExec.Helpers where

import Control.Monad.IO.Unlift (MonadUnliftIO, liftIO)
import Data.Map qualified as Map
import GHC.IORef (IORef, readIORef)
import Optics.Core ((%~))

import EVM.Fetch qualified as Fetch
import EVM.Solvers (SolverGroup)
import EVM.SymExec (Postcondition)
import EVM.Types (Addr, Frame(..), FrameState(..), VMType(..), Expr(..), Block(..), W256, Prop(..), Query(..))
import qualified EVM.Types as EVM

import Echidna.Types.Cache (ContractCache, SlotCache)

-- | Sets result to Nothing, and sets gas to ()
vmMakeSymbolic :: W256 -> W256 -> EVM.VM Concrete s -> EVM.VM Symbolic s
vmMakeSymbolic maxTimestampDiff maxNumberDiff vm
  = EVM.VM
  { result         = Nothing
  , state          = frameStateMakeSymbolic vm.state
  , frames         = map frameMakeSymbolic vm.frames
  , env            = vm.env
  , block          = blockMakeSymbolic vm.block
  , tx             = vm.tx
  , logs           = vm.logs
  , traces         = vm.traces
  , cache          = vm.cache
  , burned         = ()
  , iterations     = vm.iterations
  , constraints    = addBlockConstraints maxTimestampDiff maxNumberDiff vm.block vm.constraints
  , config         = vm.config
  , forks          = vm.forks
  , currentFork    = vm.currentFork
  , labels         = vm.labels
  , osEnv          = vm.osEnv
  , freshVar       = vm.freshVar
  , exploreDepth   = 0
  , keccakPreImgs  = vm.keccakPreImgs
  }

blockMakeSymbolic :: Block -> Block
blockMakeSymbolic b
  = b {
      timestamp = Var "symbolic_block_timestamp"
    , number = Var "symbolic_block_number"
  }

addBlockConstraints :: W256 -> W256 -> Block -> [Prop] -> [Prop]
addBlockConstraints maxTimestampDiff maxNumberDiff block cs =
  cs ++ [
   PGEq (Var "symbolic_block_timestamp") (block.timestamp), PLEq (Sub (Var "symbolic_block_timestamp") (block.timestamp)) $ Lit maxTimestampDiff,
   PGEq (Var "symbolic_block_number") (block.number), PLEq (Sub (Var "symbolic_block_number") (block.number)) $ Lit maxNumberDiff
  ]

frameStateMakeSymbolic :: FrameState Concrete s -> FrameState Symbolic s
frameStateMakeSymbolic fs
  = FrameState
  { contract     = fs.contract
  , codeContract = fs.codeContract
  , code         = fs.code
  , pc           = fs.pc
  , stack        = fs.stack
  , memory       = fs.memory
  , memorySize   = fs.memorySize
  , calldata     = fs.calldata
  , callvalue    = fs.callvalue
  , caller       = fs.caller
  , gas          = ()
  , returndata   = fs.returndata
  , static       = fs.static
  , overrideCaller = fs.overrideCaller
  , resetCaller  = fs.resetCaller
  }

frameMakeSymbolic :: Frame Concrete s -> Frame Symbolic s
frameMakeSymbolic fr = Frame { context = fr.context, state = frameStateMakeSymbolic fr.state }

nonReverts :: Postcondition s
nonReverts _ = \case
  EVM.Success _ _ _ _  -> PBool False -- We want to explore non-reverting transactions, so we make them true here
  _                -> PBool True

cachedOracle :: forall t m s . MonadUnliftIO m => IORef ContractCache -> IORef SlotCache -> SolverGroup -> Fetch.RpcInfo -> Fetch.Fetcher t m s
cachedOracle contractCacheRef slotCacheRef solvers info q = do
  case q of
    PleaseFetchContract addr _ continue -> do
      cache <- liftIO $ readIORef contractCacheRef
      case Map.lookup addr cache of
          Just (Just contract) -> pure $ continue contract
          _                    -> oracle q
    PleaseFetchSlot addr slot continue -> do
      cache <- liftIO $ readIORef slotCacheRef
      case Map.lookup addr cache >>= Map.lookup slot of
        Just (Just value) -> pure $ continue value
        _                 -> oracle q
    _ -> oracle q

  where oracle = Fetch.oracle solvers info
