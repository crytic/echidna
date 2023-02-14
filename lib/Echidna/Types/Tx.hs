{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Echidna.Types.Tx where

import Prelude hiding (Word)

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON, object, withObject, (.=), (.:))
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Word (Word64)

import EVM (VMResult(..), Error(..))
import EVM.ABI (encodeAbiValue, AbiValue(..))
import EVM.Types (Addr, W256)

import Echidna.Orphans.JSON ()
import Echidna.Types.Buffer (viewBuffer)
import Echidna.Types.Signature (SolCall)

-- | A transaction call is either a @CREATE@, a fully instrumented 'SolCall', or
-- an abstract call consisting only of calldata.
data TxCall = SolCreate   ByteString
            | SolCall     SolCall
            | SolCalldata ByteString
            | NoCall
  deriving (Show, Ord, Eq)
$(deriveJSON defaultOptions ''TxCall)

maxGasPerBlock :: Word64
maxGasPerBlock = 12500000 -- https://cointelegraph.com/news/ethereum-miners-vote-to-increase-gas-limit-causing-community-debate

unlimitedGasPerBlock :: Word64
unlimitedGasPerBlock = 0xffffffff

defaultTimeDelay :: W256
defaultTimeDelay = 604800

defaultBlockDelay :: W256
defaultBlockDelay = 60480

initialTimestamp :: W256
initialTimestamp = 1524785992 -- Thu Apr 26 23:39:52 UTC 2018

initialBlockNumber :: W256
initialBlockNumber = 4370000  -- Initial byzantium block

-- | A transaction is either a @CREATE@ or a regular call with an origin, destination, and value.
-- Note: I currently don't model nonces or signatures here.
data Tx = Tx { call  :: TxCall       -- | Call
             , src   :: Addr         -- | Origin
             , dst   :: Addr         -- | Destination
             , gas   :: Word64       -- | Gas
             , gasprice :: W256     -- | Gas price
             , value :: W256         -- | Value
             , delay :: (W256, W256) -- | (Time, # of blocks since last call)
             } deriving (Eq, Ord, Show)

instance ToJSON Tx where
  toJSON Tx{..} = object
    [ "call" .= call
    , "src" .= src
    , "dst" .= dst
    , "gas" .= gas
    , "gasprice" .= gasprice
    , "value" .= value
    , "delay" .= delay
    ]

instance FromJSON Tx where
  -- For compatibility we try to parse the old corpus format. Will be removed
  -- in the next major release.
  parseJSON = withObject "Tx" $ \o -> legacyParseTx o <|> parseTx o
    where
    parseTx o =
      Tx <$> o .: "call"
         <*> o .: "src"
         <*> o .: "dst"
         <*> o .: "gas"
         <*> o .: "gasprice"
         <*> o .: "value"
         <*> o .: "delay"
    legacyParseTx o =
      Tx <$> o .: "_call"
         <*> o .: "_src"
         <*> o .: "_dst"
         -- We changed gas to Word64, keep compatible with already existing
         -- corpuses that still store gas as a hex string
         <*> (fromIntegral <$> (o .: "_gas'" :: Parser W256))
         <*> o .: "_gasprice'"
         <*> o .: "_value"
         <*> o .: "_delay"

basicTx :: Text         -- | Function name
        -> [AbiValue]   -- | Function args
        -> Addr         -- | Sender
        -> Addr         -- | Destination contract
        -> Word64       -- | Gas limit
        -> (W256, W256) -- | Block increment
        -> Tx
basicTx f a s d g = basicTxWithValue f a s d g 0

basicTxWithValue :: Text         -- | Function name
                 -> [AbiValue]   -- | Function args
                 -> Addr         -- | Sender
                 -> Addr         -- | Destination contract
                 -> Word64       -- | Gas limit
                 -> W256         -- | Value
                 -> (W256, W256) -- | Block increment
                 -> Tx
basicTxWithValue f a s d g = Tx (SolCall (f, a)) s d g 0

createTx :: ByteString   -- | Constructor bytecode
         -> Addr         -- | Creator
         -> Addr         -- | Destination address
         -> Word64       -- | Gas limit
         -> (W256, W256) -- | Block increment
         -> Tx
createTx bc s d g = createTxWithValue bc s d g 0

createTxWithValue :: ByteString   -- | Constructor bytecode
                  -> Addr         -- | Creator
                  -> Addr         -- | Destination address
                  -> Word64       -- | Gas limit
                  -> W256         -- | Value
                  -> (W256, W256) -- | Block increment
                  -> Tx
createTxWithValue bc s d g = Tx (SolCreate bc) s d g 0

data TxResult = ReturnTrue
              | ReturnFalse
              | Stop
              | ErrorBalanceTooLow
              | ErrorUnrecognizedOpcode
              | ErrorSelfDestruction
              | ErrorStackUnderrun
              | ErrorBadJumpDestination
              | ErrorRevert
              | ErrorOutOfGas
              | ErrorBadCheatCode
              | ErrorStackLimitExceeded
              | ErrorIllegalOverflow
              | ErrorQuery
              | ErrorStateChangeWhileStatic
              | ErrorInvalidFormat
              | ErrorInvalidMemoryAccess
              | ErrorCallDepthLimitReached
              | ErrorMaxCodeSizeExceeded
              | ErrorPrecompileFailure
              | ErrorUnexpectedSymbolic
              | ErrorDeadPath
              | ErrorChoose -- not entirely sure what this is
              | ErrorWhiffNotUnique
              | ErrorSMTTimeout
              | ErrorFFI
              | ErrorNonceOverflow
  deriving (Eq, Ord, Show)
$(deriveJSON defaultOptions ''TxResult)

data TxConf = TxConf { propGas       :: Word64
                     -- ^ Gas to use evaluating echidna properties
                     , txGas         :: Word64
                     -- ^ Gas to use in generated transactions
                     , maxGasprice   :: W256
                     -- ^ Maximum gasprice to be checked for a transaction
                     , maxTimeDelay  :: W256
                     -- ^ Maximum time delay between transactions (seconds)
                     , maxBlockDelay :: W256
                     -- ^ Maximum block delay between transactions
                     , maxValue      :: W256
                     -- ^ Maximum value to use in transactions
                     }

-- | Transform a VMResult into a more hash friendly sum type
getResult :: VMResult -> TxResult
getResult (VMSuccess b) | viewBuffer b == Just (encodeAbiValue (AbiBool True))  = ReturnTrue
                        | viewBuffer b == Just (encodeAbiValue (AbiBool False)) = ReturnFalse
                        | otherwise                                             = Stop

getResult (VMFailure (BalanceTooLow _ _ ))      = ErrorBalanceTooLow
getResult (VMFailure (UnrecognizedOpcode _))    = ErrorUnrecognizedOpcode
getResult (VMFailure SelfDestruction )          = ErrorSelfDestruction
getResult (VMFailure StackUnderrun )            = ErrorStackUnderrun
getResult (VMFailure BadJumpDestination )       = ErrorBadJumpDestination
getResult (VMFailure (Revert _))                = ErrorRevert
getResult (VMFailure (OutOfGas _ _))            = ErrorOutOfGas
getResult (VMFailure (BadCheatCode _))          = ErrorBadCheatCode
getResult (VMFailure StackLimitExceeded)        = ErrorStackLimitExceeded
getResult (VMFailure IllegalOverflow)           = ErrorIllegalOverflow
getResult (VMFailure (Query _))                 = ErrorQuery
getResult (VMFailure StateChangeWhileStatic)    = ErrorStateChangeWhileStatic
getResult (VMFailure InvalidFormat)             = ErrorInvalidFormat
getResult (VMFailure InvalidMemoryAccess)       = ErrorInvalidMemoryAccess
getResult (VMFailure CallDepthLimitReached)     = ErrorCallDepthLimitReached
getResult (VMFailure (MaxCodeSizeExceeded _ _)) = ErrorMaxCodeSizeExceeded
getResult (VMFailure PrecompileFailure)         = ErrorPrecompileFailure
getResult (VMFailure (UnexpectedSymbolicArg{})) = ErrorUnexpectedSymbolic
getResult (VMFailure DeadPath)                  = ErrorDeadPath
getResult (VMFailure (Choose _))                = ErrorChoose -- not entirely sure what this is
getResult (VMFailure (NotUnique _))             = ErrorWhiffNotUnique
getResult (VMFailure SMTTimeout)                = ErrorSMTTimeout
getResult (VMFailure (FFI _))                   = ErrorFFI
getResult (VMFailure NonceOverflow)             = ErrorNonceOverflow

makeSingleTx :: Addr -> Addr -> W256 -> TxCall -> [Tx]
makeSingleTx a d v (SolCall c) = [Tx (SolCall c) a d maxGasPerBlock 0 v (0, 0)]
makeSingleTx _ _ _ _           = error "invalid usage of makeSingleTx"
