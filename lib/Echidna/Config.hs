module Echidna.Config where

import Control.Lens
import Control.Monad.Fail qualified as M (MonadFail(..))
import Control.Monad.Reader (Reader, ReaderT(..), runReader)
import Control.Monad.State (StateT(..), runStateT, modify')
import Control.Monad.Trans (lift)
import Data.Aeson
import Data.Aeson.KeyMap (keys)
import Data.Bool (bool)
import Data.ByteString qualified as BS
import Data.HashSet (fromList, insert, difference)
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (isPrefixOf)
import Data.Yaml qualified as Y

import EVM (VM(..))
import EVM.Types (W256)

import Echidna.Test
import Echidna.Types.Campaign
import Echidna.Mutator.Corpus (defaultMutationConsts)
import Echidna.Types.Solidity
import Echidna.Types.Tx (TxConf(TxConf), maxGasPerBlock, defaultTimeDelay, defaultBlockDelay)
import Echidna.Types.Test (TestConf(..))
import Echidna.Types.Config

instance FromJSON EConfig where
  -- retrieve the config from the key usage annotated parse
  parseJSON x = (.econfig) <$> parseJSON @EConfigWithUsage x

instance FromJSON EConfigWithUsage where
  -- this runs the parser in a StateT monad which keeps track of the keys
  -- utilized by the config parser
  -- we can then compare the set difference between the keys found in the config
  -- file and the keys used by the parser to comopute which keys were set in the
  -- config and not used and which keys were unset in the config and defaulted
  parseJSON o = do
    let v' = case o of
                  Object v -> v
                  _        -> mempty
    (c, ks) <- runStateT (parser v') $ fromList []
    let found = fromList (keys v')
    return $ EConfigWithUsage c (found `difference` ks) (ks `difference` found)
    -- this parser runs in StateT and comes equipped with the following
    -- equivalent unary operators:
    -- x .:? k (Parser) <==> x ..:? k (StateT)
    -- x .!= v (Parser) <==> x ..!= v (StateT)
    -- tl;dr use an extra initial . to lift into the StateT parser
    where parser v =
            let useKey k = modify' $ insert k
                x ..:? k = useKey k >> lift (x .:? k)
                x ..!= y = fromMaybe y <$> x
                -- Parse as unbounded Integer and see if it fits into W256
                getWord256 k def = do
                  value :: Integer <- fromMaybe (fromIntegral (def :: W256)) <$> v ..:? k
                  if value > fromIntegral (maxBound :: W256) then
                    fail $ show k <> ": value does not fit in 256 bits"
                  else
                    pure $ fromIntegral value

                -- TxConf
                xc = TxConf <$> v ..:? "propMaxGas" ..!= maxGasPerBlock
                            <*> v ..:? "testMaxGas" ..!= maxGasPerBlock
                            <*> getWord256 "maxGasprice" 0
                            <*> getWord256 "maxTimeDelay" defaultTimeDelay
                            <*> getWord256 "maxBlockDelay" defaultBlockDelay
                            <*> getWord256 "maxValue" 100000000000000000000 -- 100 eth

                -- TestConf
                tc = do
                  psender <- v ..:? "psender" ..!= 0x10000
                  fprefix <- v ..:? "prefix"  ..!= "echidna_"
                  let goal fname = if (fprefix <> "revert_") `isPrefixOf` fname then ResRevert else ResTrue
                      classify fname vm = maybe ResOther classifyRes vm._result == goal fname
                  return $ TestConf classify (const psender)

                -- CampaignConf
                cov = v ..:? "coverage" <&> \case Just False -> Nothing
                                                  _          -> Just mempty
                cc = CampaignConf <$> v ..:? "testLimit"   ..!= defaultTestLimit
                                  <*> v ..:? "stopOnFail"  ..!= False
                                  <*> v ..:? "estimateGas" ..!= False
                                  <*> v ..:? "seqLen"      ..!= defaultSequenceLength
                                  <*> v ..:? "shrinkLimit" ..!= defaultShrinkLimit
                                  <*> cov
                                  <*> v ..:? "seed"
                                  <*> v ..:? "dictFreq"    ..!= 0.40
                                  <*> v ..:? "corpusDir"   ..!= Nothing
                                  <*> v ..:? "mutConsts"   ..!= defaultMutationConsts

                -- SolConf
                fnFilter = bool Whitelist Blacklist <$> v ..:? "filterBlacklist" ..!= True
                                                    <*> v ..:? "filterFunctions" ..!= []
                mode = v ..:? "testMode" >>= \case
                  Just s  -> pure $ validateTestMode s
                  Nothing -> pure "property"
                sc = SolConf <$> v ..:? "contractAddr"    ..!= defaultContractAddr
                             <*> v ..:? "deployer"        ..!= defaultDeployerAddr
                             <*> v ..:? "sender"          ..!= Set.fromList [0x10000, 0x20000, defaultDeployerAddr]
                             <*> v ..:? "balanceAddr"     ..!= 0xffffffff
                             <*> v ..:? "balanceContract" ..!= 0
                             <*> v ..:? "codeSize"        ..!= 0x6000      -- 24576 (EIP-170)
                             <*> v ..:? "prefix"          ..!= "echidna_"
                             <*> v ..:? "cryticArgs"      ..!= []
                             <*> v ..:? "solcArgs"        ..!= ""
                             <*> v ..:? "solcLibs"        ..!= []
                             <*> v ..:? "quiet"           ..!= False
                             <*> v ..:? "initialize"      ..!= Nothing
                             <*> v ..:? "deployContracts" ..!= []
                             <*> v ..:? "deployBytecodes" ..!= []
                             <*> v ..:? "allContracts"    ..!= False
                             <*> mode
                             <*> v ..:? "testDestruction" ..!= False
                             <*> v ..:? "allowFFI"        ..!= False
                             <*> fnFilter
                names :: Names
                names Sender = (" from: " ++) . show
                names _      = const ""
                format = fromMaybe Interactive <$> (v ..:? "format" >>= \case
                  Just ("text" :: String) -> pure . Just . NonInteractive $ Text
                  Just "json"             -> pure . Just . NonInteractive $ JSON
                  Just "none"             -> pure . Just . NonInteractive $ None
                  Nothing -> pure Nothing
                  _ -> M.fail "Unrecognized format type (should be text, json, or none)") in
            EConfig <$> cc <*> pure names <*> sc <*> tc <*> xc
                    <*> (UIConf <$> v ..:? "timeout" <*> format)

-- | The default config used by Echidna (see the 'FromJSON' instance for values used).
defaultConfig :: EConfig
defaultConfig = either (error "Config parser got messed up :(") id $ Y.decodeEither' ""

-- | Try to parse an Echidna config file, throw an error if we can't.
parseConfig :: FilePath -> IO EConfigWithUsage
parseConfig f = BS.readFile f >>= Y.decodeThrow

-- | Run some action with the default configuration, useful in the REPL.
withDefaultConfig :: ReaderT EConfig m a -> m a
withDefaultConfig = (`runReaderT` defaultConfig)

-- | 'withDefaultConfig' but not for transformers
withDefaultConfig' :: Reader EConfig a -> a
withDefaultConfig' = (`runReader` defaultConfig)
