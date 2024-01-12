module Echidna.Config where

import Control.Applicative ((<|>))
import Control.Monad.State (StateT(..), runStateT, modify')
import Control.Monad.Trans (lift)
import Data.Aeson
import Data.Aeson.KeyMap (keys)
import Data.Bool (bool)
import Data.ByteString qualified as BS
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (isPrefixOf)
import Data.Yaml qualified as Y

import EVM.Types (VM(..), W256)

import Echidna.Test
import Echidna.Types.Campaign
import Echidna.Mutator.Corpus (defaultMutationConsts)
import Echidna.Output.Source (CoverageFileType(..))
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
    (c, ks) <- runStateT (parser v') $ Set.fromList []
    let found = Set.fromList (keys v')
    pure $ EConfigWithUsage c (found `Set.difference` ks) (ks `Set.difference` found)
    -- this parser runs in StateT and comes equipped with the following
    -- equivalent unary operators:
    -- x .:? k (Parser) <==> x ..:? k (StateT)
    -- x .!= v (Parser) <==> x ..!= v (StateT)
    -- tl;dr use an extra initial . to lift into the StateT parser
    where
    parser v =
      EConfig <$> campaignConfParser
              <*> pure names
              <*> solConfParser
              <*> testConfParser
              <*> txConfParser
              <*> (UIConf <$> v ..:? "timeout" <*> formatParser)
              <*> v ..:? "rpcUrl"
              <*> v ..:? "rpcBlock"
      where
      useKey k = modify' $ Set.insert k
      x ..:? k = useKey k >> lift (x .:? k)
      x ..!= y = fromMaybe y <$> x
      -- Parse as unbounded Integer and see if it fits into W256
      getWord256 k def = do
        value :: Integer <- fromMaybe (fromIntegral (def :: W256)) <$> v ..:? k
        if value > fromIntegral (maxBound :: W256) then
          fail $ show k <> ": value does not fit in 256 bits"
        else
          pure $ fromIntegral value

      txConfParser = TxConf
        <$> v ..:? "propMaxGas" ..!= maxGasPerBlock
        <*> v ..:? "testMaxGas" ..!= maxGasPerBlock
        <*> getWord256 "maxGasprice" 0
        <*> getWord256 "maxTimeDelay" defaultTimeDelay
        <*> getWord256 "maxBlockDelay" defaultBlockDelay
        <*> getWord256 "maxValue" 100000000000000000000 -- 100 eth

      testConfParser = do
        psender <- v ..:? "psender" ..!= 0x10000
        fprefix <- v ..:? "prefix"  ..!= "echidna_"
        let goal fname = if (fprefix <> "revert_") `isPrefixOf` fname then ResRevert else ResTrue
            classify fname vm = maybe ResOther classifyRes vm.result == goal fname
        pure $ TestConf classify (const psender)

      campaignConfParser = CampaignConf
        <$> v ..:? "testLimit" ..!= defaultTestLimit
        <*> v ..:? "stopOnFail" ..!= False
        <*> v ..:? "estimateGas" ..!= False
        <*> v ..:? "seqLen" ..!= defaultSequenceLength
        <*> v ..:? "shrinkLimit" ..!= defaultShrinkLimit
        <*> (v ..:? "coverage" <&> \case Just False -> Nothing;  _ -> Just mempty)
        <*> v ..:? "seed"
        <*> v ..:? "dictFreq" ..!= 0.40
        <*> v ..:? "corpusDir" ..!= Nothing
        <*> v ..:? "mutConsts" ..!= defaultMutationConsts
        <*> v ..:? "coverageFormats" ..!= [Txt,Html,Lcov]
        <*> v ..:? "workers"
        <*> v ..:? "server"

      solConfParser = SolConf
        <$> v ..:? "contractAddr"    ..!= defaultContractAddr
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
        <*> ((<|>) <$> v ..:? "allContracts"
                   -- TODO: keep compatible with the old name for a while
                   <*> lift (v .:? "multi-abi")) ..!= False
        <*> mode
        <*> v ..:? "testDestruction" ..!= False
        <*> v ..:? "allowFFI"        ..!= False
        <*> fnFilter
        where
        mode = v ..:? "testMode" >>= \case
          Just s  -> pure $ validateTestMode s
          Nothing -> pure "property"
        fnFilter = bool Whitelist Blacklist <$> v ..:? "filterBlacklist" ..!= True
                                            <*> v ..:? "filterFunctions" ..!= []

      names :: Names
      names Sender = (" from: " ++) . show
      names _      = const ""

      formatParser = fromMaybe Interactive <$> (v ..:? "format" >>= \case
        Just ("text" :: String) -> pure . Just . NonInteractive $ Text
        Just "json"             -> pure . Just . NonInteractive $ JSON
        Just "none"             -> pure . Just . NonInteractive $ None
        Nothing -> pure Nothing
        _ -> fail "Unrecognized format type (should be text, json, or none)")

-- | The default config used by Echidna (see the 'FromJSON' instance for values used).
defaultConfig :: EConfig
defaultConfig = either (error "Config parser got messed up :(") id $ Y.decodeEither' ""

-- | Try to parse an Echidna config file, throw an error if we can't.
parseConfig :: FilePath -> IO EConfigWithUsage
parseConfig f = BS.readFile f >>= Y.decodeThrow
