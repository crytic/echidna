{-# LANGUAGE DeriveAnyClass #-}

module Echidna.Onchain where

import Control.Exception (catch)
import Data.Aeson (ToJSON, FromJSON, ToJSONKey(toJSONKey))
import Data.Aeson qualified as JSON
import Data.Aeson.Types (toJSONKeyText)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.UTF8 qualified as UTF8
import Data.Functor ((<&>))
import Data.IORef (readIORef)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (isJust, fromJust, fromMaybe)
import Data.Text qualified as Text
import Data.Text (Text)
import Data.Vector qualified as Vector
import Data.Word (Word64)
import Etherscan qualified
import GHC.Generics (Generic)
import Network.HTTP.Simple (HttpException)
import Optics (view)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Text.Read (readMaybe)

import EVM (initialContract, bytecode)
import EVM.Fetch qualified
import EVM.Solidity (SourceCache(..), SolcContract (..))
import EVM.Types hiding (Env)

import Echidna.Orphans.JSON ()
import Echidna.Symbolic (forceWord, forceBuf)
import Echidna.Types (emptyAccount)
import Echidna.Types.Campaign (CampaignConf(..))
import Echidna.Types.Config (Env(..), EConfig(..))
import Echidna.Output.Source (saveCoverages)
import Control.Monad (when, forM_)

rpcUrlEnv :: IO (Maybe Text)
rpcUrlEnv = do
  val <- lookupEnv "ECHIDNA_RPC_URL"
  pure (Text.pack <$> val)

rpcBlockEnv :: IO (Maybe Word64)
rpcBlockEnv = do
  val <- lookupEnv "ECHIDNA_RPC_BLOCK"
  pure (val >>= readMaybe)

etherscanApiKey :: IO (Maybe Text)
etherscanApiKey = do
  val <- lookupEnv "ETHERSCAN_API_KEY"
  pure (Text.pack <$> val)

-- TODO: temporary solution, handle errors gracefully
safeFetchContractFrom :: EVM.Fetch.BlockNumber -> Text -> Addr -> IO (Maybe Contract)
safeFetchContractFrom rpcBlock rpcUrl addr =
  catch
    (EVM.Fetch.fetchContractFrom rpcBlock rpcUrl addr)
    (\(_ :: HttpException) -> pure $ Just emptyAccount)

-- TODO: temporary solution, handle errors gracefully
safeFetchSlotFrom :: EVM.Fetch.BlockNumber -> Text -> Addr -> W256 -> IO (Maybe W256)
safeFetchSlotFrom rpcBlock rpcUrl addr slot =
  catch
    (EVM.Fetch.fetchSlotFrom rpcBlock rpcUrl addr slot)
    (\(_ :: HttpException) -> pure $ Just 0)

data FetchedContractData = FetchedContractData
  { runtimeCode :: ByteString
  , nonce :: Maybe W64
  , balance :: W256
  }
  deriving (Generic, ToJSON, FromJSON, Show)

instance ToJSONKey W256 where
  toJSONKey = toJSONKeyText (Text.pack . show)

fromFetchedContractData :: FetchedContractData -> Contract
fromFetchedContractData contractData =
  (initialContract (RuntimeCode (ConcreteRuntimeCode contractData.runtimeCode)))
    { nonce = contractData.nonce
    , balance = Lit contractData.balance
    , external = True
    }

toFetchedContractData :: Contract -> FetchedContractData
toFetchedContractData contract =
  let code = case contract.code of
               RuntimeCode (ConcreteRuntimeCode c) -> c
               _ -> error "unexpected code"
  in FetchedContractData
    { runtimeCode = code
    , nonce = contract.nonce
    , balance = forceWord contract.balance
    }

-- | Try to load the persisted RPC cache.
-- TODO: we use the corpus dir for now, think about where to place it
loadRpcCache
  :: EConfig
  -> IO ( Map Addr (Maybe Contract)
        , Map Addr (Map W256 (Maybe W256))
        )
loadRpcCache cfg =
  case cfg.campaignConf.corpusDir of
    Nothing -> pure (mempty, mempty)
    Just dir -> do
      let cache_dir = dir </> "cache"
      createDirectoryIfMissing True cache_dir
      case cfg.rpcBlock of
        Just block -> do
          parsedContracts :: Maybe (Map Addr FetchedContractData) <-
            readFileIfExists (cache_dir </> "block_" <> show block <> "_fetch_cache_contracts.json")
            <&> (>>= JSON.decodeStrict)
          parsedSlots :: Maybe (Map Addr (Map W256 (Maybe W256))) <-
            readFileIfExists (cache_dir </> "block_" <> show block <> "_fetch_cache_slots.json")
            <&> (>>= JSON.decodeStrict)
          pure
            ( maybe mempty (Map.map (Just . fromFetchedContractData)) parsedContracts
            , fromMaybe mempty parsedSlots
            )
        Nothing ->
          pure (mempty, mempty)

readFileIfExists :: FilePath -> IO (Maybe BS.ByteString)
readFileIfExists path = do
  exists <- doesFileExist path
  if exists then Just <$> BS.readFile path else pure Nothing

-- | "Reverse engineer" the SolcContract and SourceCache structures for the
-- code fetched from the outside
externalSolcContract :: Env -> Addr -> Contract -> IO (Maybe (SourceCache, SolcContract))
externalSolcContract env addr c = do
  let runtimeCode = forceBuf $ fromJust $ view bytecode c
  putStr $ "Fetching Solidity source for contract at address " <> show addr <> "... "
  srcRet <- Etherscan.fetchContractSource env.cfg.etherscanApiKey addr
  putStrLn $ if isJust srcRet then "Success!" else "Error!"
  putStr $ "Fetching Solidity source map for contract at address " <> show addr <> "... "
  srcmapRet <- Etherscan.fetchContractSourceMap addr
  putStrLn $ if isJust srcmapRet then "Success!" else "Error!"
  pure $ do
    src <- srcRet
    (_, srcmap) <- srcmapRet
    let
      files = Map.singleton 0 (show addr, UTF8.fromString src.code)
      sourceCache = SourceCache
        { files
        , lines = Vector.fromList . BS.split 0xa . snd <$> files
        , asts = mempty
        }
      solcContract = SolcContract
        { runtimeCode = runtimeCode
        , creationCode = mempty
        , runtimeCodehash = keccak' runtimeCode
        , creationCodehash = keccak' mempty
        , runtimeSrcmap = mempty
        , creationSrcmap = srcmap
        , contractName = src.name
        , constructorInputs = [] -- error "TODO: mkConstructor abis TODO"
        , abiMap = mempty -- error "TODO: mkAbiMap abis"
        , eventMap = mempty -- error "TODO: mkEventMap abis"
        , errorMap = mempty -- error "TODO: mkErrorMap abis"
        , storageLayout = Nothing
        , immutableReferences = mempty
        }
    pure (sourceCache, solcContract)

-- TODO: This should happen continuously event-based
saveRpcCache :: Env -> IO ()
saveRpcCache env = do
  contractsCache <- readIORef env.fetchContractCache
  slotsCache <- readIORef env.fetchSlotCache
  case env.cfg.campaignConf.corpusDir of
    Nothing -> pure ()
    Just dir -> do
      let cacheDir = dir </> "cache"
      case env.cfg.rpcBlock of
        Just block -> do
          -- Save fetched data, it's okay to override as the cache only grows
          JSON.encodeFile (cacheDir </> "block_" <> show block <> "_fetch_cache_contracts.json")
                          (toFetchedContractData <$> Map.mapMaybe id contractsCache)
          JSON.encodeFile (cacheDir </> "block_" <> show block <> "_fetch_cache_slots.json")
                          slotsCache
        Nothing ->
          pure ()

saveCoverageReport :: Env -> Int -> IO ()
saveCoverageReport env runId = do
  case env.cfg.campaignConf.corpusDir of
    Nothing -> pure ()
    Just dir -> do
      -- coverage reports for external contracts, we only support
      -- Ethereum Mainnet for now
      when (env.chainId == Just 1) $ do
        contractsCache <- readIORef env.fetchContractCache
        forM_ (Map.toList contractsCache) $ \(addr, mc) ->
          case mc of
            Just contract -> do
              r <- externalSolcContract env addr contract
              case r of
                Just (externalSourceCache, solcContract) -> do
                  let dir' = dir </> show addr
                  saveCoverages env
                                runId
                                dir'
                                externalSourceCache
                                [solcContract]
                Nothing -> pure ()
            Nothing -> pure ()
