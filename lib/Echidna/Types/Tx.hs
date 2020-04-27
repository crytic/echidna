{-# LANGUAGE TemplateHaskell #-}

module Echidna.Types.Tx where

import Prelude hiding (Word)

import Control.Lens.TH (makePrisms, makeLenses)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.ByteString (ByteString)
import EVM (VMResult(..), Error(..))
import EVM.Concrete (Word)
import EVM.Types (Addr)

import Echidna.Orphans.JSON ()
import Echidna.Types.Signature (SolCall)

-- | A transaction call is either a @CREATE@, a fully instrumented 'SolCall', or
-- an abstract call consisting only of calldata.
data TxCall = SolCreate   ByteString
            | SolCall     SolCall
            | SolCalldata ByteString
  deriving (Show, Ord, Eq)
makePrisms ''TxCall
$(deriveJSON defaultOptions ''TxCall)

-- | A transaction is either a @CREATE@ or a regular call with an origin, destination, and value.
-- Note: I currently don't model nonces or signatures here.
data Tx = Tx { _call  :: TxCall       -- | Call
             , _src   :: Addr         -- | Origin
             , _dst   :: Addr         -- | Destination
             , _gas'  :: Word         -- | Gas
             , _gasprice' :: Word     -- | Gas price
             , _value :: Word         -- | Value
             , _delay :: (Word, Word) -- | (Time, # of blocks since last call)
             } deriving (Eq, Ord, Show)
makeLenses ''Tx
$(deriveJSON defaultOptions ''Tx)

data TxResult = Success
              | ErrorBalanceTooLow 
              | ErrorUnrecognizedOpcode
              | ErrorSelfDestruction
              | ErrorStackUnderrun
              | ErrorBadJumpDestination
              | ErrorRevert
              | ErrorNoSuchContract
              | ErrorOutOfGas
              | ErrorBadCheatCode
              | ErrorStackLimitExceeded
              | ErrorIllegalOverflow
              | ErrorQuery
              | ErrorStateChangeWhileStatic
              | ErrorInvalidMemoryAccess
              | ErrorCallDepthLimitReached
              | ErrorMaxCodeSizeExceeded
              | ErrorPrecompileFailure      deriving (Eq, Ord, Show)
$(deriveJSON defaultOptions ''TxResult)

data TxConf = TxConf { _propGas       :: Word
                     -- ^ Gas to use evaluating echidna properties
                     , _txGas         :: Word
                     -- ^ Gas to use in generated transactions
                     , _maxGasprice   :: Word
                     -- ^ Maximum gasprice to be checked for a transaction
                     , _maxTimeDelay  :: Word
                     -- ^ Maximum time delay between transactions (seconds)
                     , _maxBlockDelay :: Word
                     -- ^ Maximum block delay between transactions
                     , _maxValue      :: Word
                     -- ^ Maximum value to use in transactions  
                     }
makeLenses 'TxConf

-- | Transform a VMResult into a more hash friendly sum type
getResult :: VMResult -> TxResult
getResult (VMSuccess _)                         = Success
getResult (VMFailure (BalanceTooLow _ _ ))      = ErrorBalanceTooLow
getResult (VMFailure (UnrecognizedOpcode _))    = ErrorUnrecognizedOpcode
getResult (VMFailure SelfDestruction )          = ErrorSelfDestruction
getResult (VMFailure StackUnderrun )            = ErrorStackUnderrun
getResult (VMFailure BadJumpDestination )       = ErrorBadJumpDestination
getResult (VMFailure (Revert _))                = ErrorRevert
getResult (VMFailure (NoSuchContract _))        = ErrorNoSuchContract
getResult (VMFailure (OutOfGas _ _))            = ErrorOutOfGas
getResult (VMFailure (BadCheatCode _))          = ErrorBadCheatCode
getResult (VMFailure StackLimitExceeded)        = ErrorStackLimitExceeded
getResult (VMFailure IllegalOverflow)           = ErrorIllegalOverflow
getResult (VMFailure (Query _))                 = ErrorQuery
getResult (VMFailure StateChangeWhileStatic)    = ErrorStateChangeWhileStatic
getResult (VMFailure InvalidMemoryAccess)       = ErrorInvalidMemoryAccess
getResult (VMFailure CallDepthLimitReached)     = ErrorCallDepthLimitReached
getResult (VMFailure (MaxCodeSizeExceeded _ _)) = ErrorMaxCodeSizeExceeded
getResult (VMFailure PrecompileFailure)         = ErrorPrecompileFailure
