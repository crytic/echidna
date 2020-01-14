{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Echidna.Types where

import Prelude hiding (Word)

import Control.Lens.TH (makeLenses)
import Control.Monad.Catch (Exception)
import Data.Aeson (ToJSON(..), object)
import Data.ByteString (ByteString)
import Data.Foldable (toList)
import Data.Has (Has(..))
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.Map (Map, mapKeys)
import Data.Maybe (mapMaybe, maybeToList)
import Data.Set (Set)
import Data.Text (Text)
import Data.Vector.Instances ()
import Echidna.Solidity.Types (SolSignature, SolCall, SolTest)
import Echidna.Solidity.Pretty (ppSolCall)
import Echidna.Util (hashMapBy)
import EVM (Error, VM)
import EVM.ABI (AbiType, AbiValue(..), abiValueType)
import EVM.Concrete (Word)
import EVM.Types (W256, Addr)
import Numeric (showHex)

import qualified Data.List.NonEmpty as NE

-- | We throw this when our execution fails due to something other than reversion.
data ExecException = IllegalExec Error | UnknownFailure Error

instance Show ExecException where
  show (IllegalExec e) = "VM attempted an illegal operation: " ++ show e
  show (UnknownFailure e) = "VM failed for unhandled reason, " ++ show e
    ++ ". This shouldn't happen. Please file a ticket with this error message and steps to reproduce!"

instance Exception ExecException

-- | Configuration necessary for generating new 'SolCalls'. Don't construct this by hand! Use 'mkConf'.
data GenDict = GenDict { _pSynthA    :: Float
                         -- ^ Fraction of time to use dictionary vs. synthesize
                       , _constants  :: HashMap AbiType [AbiValue]
                         -- ^ Constants to use, sorted by type
                       , _wholeCalls :: HashMap SolSignature [SolCall]
                         -- ^ Whole calls to use, sorted by type
                       , _defSeed    :: Int
                         -- ^ Default seed to use if one is not provided in EConfig
                       , _rTypes     :: Text -> Maybe AbiType
                         -- ^ Return types of any methods we scrape return values from
                       }
makeLenses 'GenDict

-- | A transaction is either a @CREATE@ or a regular call with an origin, destination, and value.
-- Note: I currently don't model nonces or signatures here.
data Tx = Tx { _call  :: Either SolCall ByteString -- | Either a call or code for a @CREATE@
             , _src   :: Addr                      -- | Origin
             , _dst   :: Addr                      -- | Destination
             , _gas'  :: Word                      -- | Gas
             , _gasprice' :: Word                  -- | Gas price
             , _value :: Word                      -- | Value
             , _delay :: (Word, Word)              -- | (Time, # of blocks since last call)
             } deriving (Eq, Ord, Show)
makeLenses ''Tx

instance ToJSON Tx where
  toJSON (Tx c s d g gp v (t, b)) = object
    [ ("call",        toJSON $ either ppSolCall (const "<CREATE>") c)
    -- from/to are Strings, since JSON doesn't support hexadecimal notation
    , ("from",        toJSON $ show s)
    , ("to",          toJSON $ show d)
    , ("value",       toJSON $ show v)
    , ("gas",         toJSON $ show g)
    , ("gasprice",    toJSON $ show gp)
    , ("time delay",  toJSON $ show t)
    , ("block delay", toJSON $ show b)
    ]

-- | State of a particular Echidna test. N.B.: \"Solved\" means a falsifying call sequence was found.
data TestState = Open Int             -- ^ Maybe solvable, tracking attempts already made
               | Large Int [Tx]       -- ^ Solved, maybe shrinable, tracking shrinks tried + best solve
               | Passed               -- ^ Presumed unsolvable
               | Solved [Tx]          -- ^ Solved with no need for shrinking
               | Failed ExecException -- ^ Broke the execution environment
                 deriving Show

instance Eq TestState where
  (Open i)    == (Open j)    = i == j
  (Large i l) == (Large j m) = i == j && l == m
  Passed      == Passed      = True
  (Solved l)  == (Solved m)  = l == m
  _           == _           = False

instance ToJSON TestState where
  toJSON s = object $ ("passed", toJSON passed) : maybeToList desc where
    (passed, desc) = case s of Open _    -> (True, Nothing)
                               Passed    -> (True, Nothing)
                               Large _ l -> (False, Just ("callseq", toJSON l))
                               Solved  l -> (False, Just ("callseq", toJSON l))
                               Failed  e -> (False, Just ("exception", toJSON $ show e))

-- | The state of a fuzzing campaign.
data Campaign = Campaign { _tests       :: [(SolTest, TestState)]
                           -- ^ Tests being evaluated
                         , _coverage    :: Map W256 (Set Int)
                           -- ^ Coverage captured (NOTE: we don't always record this)
                         , _genDict     :: GenDict
                           -- ^ Generation dictionary
                         }
makeLenses ''Campaign

instance ToJSON Campaign where
  toJSON (Campaign ts co _) = object $ ("tests", toJSON $ mapMaybe format ts)
    : if co == mempty then [] else [("coverage",) . toJSON . mapKeys (`showHex` "") $ toList <$> co] where
      format (Right _,      Open _) = Nothing
      format (Right (n, _), s)      = Just ("assertion in " <> n, toJSON s)
      format (Left (n, _),  s)      = Just (n,                    toJSON s)

instance Has GenDict Campaign where
  hasLens = genDict

-- We need the above since hlint doesn't notice DeriveAnyClass in StandaloneDeriving.
deriving instance Hashable AbiType

-- | Construct a 'GenDict' from some dictionaries, a 'Float', a default seed, and a typing rule for
-- return values
mkGenDict :: Float      -- ^ Percentage of time to mutate instead of synthesize. Should be in [0,1]
          -> [AbiValue] -- ^ A list of 'AbiValue' constants to use during dictionary-based generation
          -> [SolCall]  -- ^ A list of complete 'SolCall's to mutate
          -> Int        -- ^ A default seed
          -> (Text -> Maybe AbiType)
          -- ^ A return value typing rule
          -> GenDict
mkGenDict p vs cs = GenDict p (hashMapBy abiValueType vs) (hashMapBy (fmap $ fmap abiValueType) cs)

-- | Configuration for running an Echidna 'Campaign'.
data CampaignConf = CampaignConf { testLimit     :: Int
                                   -- ^ Maximum number of function calls to execute while fuzzing
                                 , stopOnFail    :: Bool
                                   -- ^ Whether to stop the campaign immediately if any property fails
                                 , seqLen        :: Int
                                   -- ^ Number of calls between state resets (e.g. \"every 10 calls,
                                   -- reset the state to avoid unrecoverable states/save memory\"
                                 , shrinkLimit   :: Int
                                   -- ^ Maximum number of candidate sequences to evaluate while shrinking
                                 , knownCoverage :: Maybe (Map W256 (Set Int))
                                   -- ^ If applicable, initially known coverage. If this is 'Nothing',
                                   -- Echidna won't collect coverage information (and will go faster)
                                 , seed          :: Maybe Int
                                 , dictFreq      :: Float
                                 }

-- | Configuration for loading Solidity for Echidna testing.
data SolConf = SolConf { _contractAddr    :: Addr             -- ^ Contract address to use
                       , _deployer        :: Addr             -- ^ Contract deployer address to use
                       , _sender          :: NE.NonEmpty Addr -- ^ Sender addresses to use
                       , _balanceAddr     :: Integer          -- ^ Initial balance of deployer and senders
                       , _balanceContract :: Integer          -- ^ Initial balance of contract to test
                       , _prefix          :: Text             -- ^ Function name prefix used to denote tests
                       , _cryticArgs      :: [String]         -- ^ Args to pass to crytic
                       , _solcArgs        :: String           -- ^ Args to pass to @solc@
                       , _solcLibs        :: [String]         -- ^ List of libraries to load, in order.
                       , _quiet           :: Bool             -- ^ Suppress @solc@ output, errors, and warnings
                       , _checkAsserts    :: Bool             -- ^ Test if we can cause assertions to fail
                       }
makeLenses ''SolConf

-- | Configuration for evaluating Echidna tests.
data TestConf = TestConf { classifier :: Text -> VM -> Bool
                           -- ^ Given a VM state and test name, check if a test just passed (typically
                           -- examining '_result'.)
                         , testSender :: Addr -> Addr
                           -- ^ Given the address of a test, return the address to send test evaluation
                           -- transactions from.
                         }

-- | Configuration for the Echidna UI.
data UIConf = UIConf { _dashboard :: Bool
                     , _maxTime   :: Maybe Int
                     , _finished  :: Campaign -> Int -> String
                     }

-- | An address involved with a 'Transaction' is either the sender, the recipient, or neither of those things.
data Role = Sender | Receiver | Ambiguous

-- | Rules for pretty-printing addresses based on their role in a transaction.
type Names = Role -> Addr -> String
