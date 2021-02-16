{-# LANGUAGE TemplateHaskell #-}

module Echidna.Types.Tx where

import Prelude hiding (Word)

import Control.Lens.TH (makePrisms, makeLenses)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.ByteString (ByteString)
import Data.Text (Text)
import EVM (VMResult(..), Error(..))
import EVM.Concrete (Word, w256)
import EVM.Types (Addr, W256)
import EVM.ABI (AbiValue)

import Echidna.Orphans.JSON ()
import Echidna.Types.Signature (SolCall)

-- | A transaction call is either a @CREATE@, a fully instrumented 'SolCall', or
-- an abstract call consisting only of calldata.
data TxCall = SolCreate   ByteString
            | SolCall     SolCall
            | SolCalldata ByteString
            | NoCall
  deriving (Show, Ord, Eq)
makePrisms ''TxCall
$(deriveJSON defaultOptions ''TxCall)

maxGasPerBlock :: Integer
maxGasPerBlock = 12500000 -- https://cointelegraph.com/news/ethereum-miners-vote-to-increase-gas-limit-causing-community-debate

unlimitedGasPerBlock :: Integer
unlimitedGasPerBlock = 0xffffffff

defaultTimeDelay :: Integer
defaultTimeDelay = 604800

defaultBlockDelay :: Integer
defaultBlockDelay = 60480

initialTimestamp :: Word
initialTimestamp = 1524785992 -- Thu Apr 26 23:39:52 UTC 2018

initialBlockNumber :: Word
initialBlockNumber = 4370000  -- Initial byzantium block

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

basicTx :: Text         -- | Function name
        -> [AbiValue]   -- | Function args
        -> Addr         -- | msg.sender
        -> Addr         -- | Destination contract
        -> Word         -- | Gas limit
        -> (Word, Word) -- | Block increment
        -> Tx
basicTx f a s d g = basicTxWithValue f a s d g 0

basicTxWithValue :: Text         -- | Function name
                 -> [AbiValue]   -- | Function args
                 -> Addr         -- | msg.sender
                 -> Addr         -- | Destination contract
                 -> Word         -- | Gas limit
                 -> Word         -- | Value
                 -> (Word, Word) -- | Block increment
                 -> Tx
basicTxWithValue f a s d g = Tx (SolCall (f, a)) s d g 0

createTx :: ByteString   -- | Constructor bytecode
         -> Addr         -- | Creator
         -> Addr         -- | Destination address
         -> Word         -- | Gas limit
         -> (Word, Word) -- | Block increment
         -> Tx
createTx bc s d g = createTxWithValue bc s d g 0

createTxWithValue :: ByteString  -- | Constructor bytecode
                  -> Addr        -- | Creator
                  -> Addr        -- | Destination address
                  -> Word        -- | Gas limit
                  -> Word        -- | Value
                  -> (Word, Word) -- | Block increment
                  -> Tx
createTxWithValue bc s d g = Tx (SolCreate bc) s d g 0

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
              | ErrorPrecompileFailure
              | ErrorUnexpectedSymbolic
              | ErrorDeadPath
              | ErrorChoose -- not entirely sure what this is
  deriving (Eq, Ord, Show)
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
getResult (VMFailure UnexpectedSymbolicArg)     = ErrorUnexpectedSymbolic
getResult (VMFailure DeadPath)                  = ErrorDeadPath
getResult (VMFailure (Choose _))                = ErrorChoose -- not entirely sure what this is

makeSingleTx :: Addr -> Addr -> W256 -> TxCall -> [Tx]
makeSingleTx a d v (SolCall c) = [Tx (SolCall c) a d (fromInteger maxGasPerBlock) 0 (w256 v) (0, 0)]
makeSingleTx _ _ _ _           = error "invalid usage of makeSingleTx"
