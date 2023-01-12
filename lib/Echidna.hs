module Echidna where

import Control.Monad.Catch (MonadThrow(..))
import Data.HashMap.Strict (toList)
import Data.Map.Strict (keys)
import Data.List (nub, find)
import Data.List.NonEmpty qualified as NE

import EVM
import EVM.ABI (AbiValue(AbiAddress))
import EVM.Solidity (SourceCache, SolcContract)

import Echidna.ABI
import Echidna.Types.Config hiding (cfg)
import Echidna.Types.Solidity
import Echidna.Types.Campaign
import Echidna.Types.Random
import Echidna.Types.Signature
import Echidna.Types.Test
import Echidna.Types.Tx
import Echidna.Types.World
import Echidna.Solidity
import Echidna.Processor
import Echidna.Output.Corpus
import Echidna.RPC (loadEtheno, extractFromEtheno)

-- | This function is used to prepare, process, compile and initialize smart contracts for testing.
-- It takes:
-- * A config record
-- * A list of contract files paths for the smart contract code
-- * A contract name (if any)
-- * A seed used during the random generation
-- and returns:
-- * A VM with the contract deployed and ready for testing
-- * A World with all the required data for generating random transctions
-- * A list of Echidna tests to check
-- * A prepopulated dictionary (if any)
-- * A list of transaction sequences to initialize the corpus
prepareContract :: EConfig -> NE.NonEmpty FilePath -> Maybe ContractName -> Seed
                -> IO (VM, SourceCache, [SolcContract], World, [EchidnaTest], Maybe GenDict, [[Tx]])
prepareContract cfg fs c g = do
  ctxs1 <- loadTxs (fmap (++ "/reproducers/") cd)
  ctxs2 <- loadTxs (fmap (++ "/coverage/") cd)
  let ctxs = ctxs1 ++ ctxs2

  let solConf = cfg._sConf

  -- compile and load contracts
  (cs, scs) <- Echidna.Solidity.contracts solConf fs
  p <- loadSpecified solConf c cs

  -- run processors
  si <- runSlither (NE.head fs) solConf._cryticArgs
  case find (< minSupportedSolcVersion) $ solcVersions si of
    Just outdatedVersion -> throwM $ OutdatedSolcVersion outdatedVersion
    Nothing -> return ()

  -- load tests
  let (v, w, ts) = prepareForTest solConf p c si

  -- get signatures
  let sigs = nub $ concatMap (NE.toList . snd) (toList $ w._highSignatureMap)

  let ads = addresses solConf
  let ads' = AbiAddress <$> keys v._env._contracts
  let constants' = enhanceConstants si ++ timeConstants ++ extremeConstants ++ NE.toList ads ++ ads'

  -- load transactions from init sequence (if any)
  es' <- maybe (return []) loadEtheno it
  let txs = ctxs ++ maybe [] (const [extractFromEtheno es' sigs]) it

  -- start ui and run tests
  let sc = selectSourceCache c scs
  pure (v, sc, cs, w, ts, Just $ mkGenDict df constants' [] g (returnTypes cs), txs)
  where cd = cfg._cConf._corpusDir
        df = cfg._cConf._dictFreq
        it = cfg._sConf._initialize
