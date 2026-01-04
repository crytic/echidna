module Echidna.Onchain
  ( etherscanApiKey
  , fetchChainIdFrom
  , rpcBlockEnv
  , rpcUrlEnv
  , safeFetchContractFrom
  , safeFetchSlotFrom
  , saveCoverageReport
  , saveRpcCache
  )
where

import Control.Concurrent.MVar (readMVar)
import Control.Exception (catch, SomeException)
import Control.Monad (when, forM_)
import Data.ByteString qualified as BS
import Data.ByteString.UTF8 qualified as UTF8
import Data.Map qualified as Map
import Data.Maybe (isJust, isNothing, fromJust, fromMaybe)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Data.Word (Word64)
import Network.HTTP.Simple (HttpException)
import Network.Wreq.Session qualified as Session
import Optics (view)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Text.Read (readMaybe)

import EVM (bytecode)
import EVM.Effects (defaultConfig)
import EVM.Fetch qualified
import EVM.Solidity (SourceCache(..), SolcContract(..), SrcMap, makeSrcMaps)
import EVM.Types hiding (Env)

import Echidna.Onchain.Etherscan qualified as Etherscan
import Echidna.Onchain.Sourcify qualified as Sourcify
import Echidna.Onchain.Types (SourceData(..))
import Echidna.Output.Source (saveCoverages)
import Echidna.SymExec.Symbolic (forceBuf)
import Echidna.Types.Campaign (CampaignConf(..))
import Echidna.Types.Config (Env(..), EConfig(..))

saveRpcCache :: Env -> IO ()
saveRpcCache env = do
  case (env.fetchSession.cacheDir, env.cfg.rpcBlock) of
    (Just dir, Just n) -> do
      cache <- readMVar (env.fetchSession.sharedCache)
      EVM.Fetch.saveCache dir (fromIntegral n) cache
    (_, Nothing) -> when (isJust (env.cfg.rpcUrl))
      $ putStrLn "Warning: cannot save RPC cache without a specified block number."
    (Nothing, _) -> pure ()

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

safeFetchContractFrom :: EVM.Fetch.Session -> EVM.Fetch.BlockNumber -> Text -> Addr -> IO (EVM.Fetch.FetchResult Contract)
safeFetchContractFrom session rpcBlock rpcUrl addr = do
  catch
    (do
      res <- EVM.Fetch.fetchContractWithSession defaultConfig session rpcBlock rpcUrl addr
      pure $ case res of
        EVM.Fetch.FetchSuccess c status -> EVM.Fetch.FetchSuccess (EVM.Fetch.makeContractFromRPC c) status
        EVM.Fetch.FetchFailure status -> EVM.Fetch.FetchFailure status
        EVM.Fetch.FetchError e -> EVM.Fetch.FetchError e
    )
    (\(e :: HttpException) -> pure $ EVM.Fetch.FetchError (Text.pack $ show e))

safeFetchSlotFrom :: EVM.Fetch.Session -> EVM.Fetch.BlockNumber -> Text -> Addr -> W256 -> IO (EVM.Fetch.FetchResult W256)
safeFetchSlotFrom session rpcBlock rpcUrl addr slot =
  catch
    (EVM.Fetch.fetchSlotWithCache defaultConfig session rpcBlock rpcUrl addr slot)
    (\(e :: HttpException) -> pure $ EVM.Fetch.FetchError (Text.pack $ show e))

-- | "Reverse engineer" the SolcContract and SourceCache structures for the
-- code fetched from the outside
externalSolcContract :: Env -> String -> Addr -> Contract -> IO (Maybe (SourceCache, SolcContract))
externalSolcContract env explorerUrl addr c = do
  let runtimeCode = forceBuf $ fromJust $ view bytecode c

  -- Try Sourcify first (if chainId available)
  sourcifyResult <- case env.chainId of
    Just chainId -> do
      putStr $ "Fetching source for " <> show addr <> " from Sourcify... "
      Sourcify.fetchContractSource chainId addr
    Nothing -> pure Nothing

  -- If Sourcify fails, try Etherscan (only if API key exists)
  sourceData <- case sourcifyResult of
    Just sd -> do
      putStrLn "Success!"
      pure (Just sd)
    Nothing -> case env.cfg.etherscanApiKey of
      Nothing -> do
        when (isNothing env.chainId) $
          putStrLn "No source available (no chain ID or Etherscan API key)"
        when (isJust env.chainId) $
          putStrLn "Skipping Etherscan (no API key configured)"
        pure Nothing
      Just _ -> do
        putStr $ "Fetching source for " <> show addr <> " from Etherscan... "
        result <- Etherscan.fetchContractSourceData
          env.chainId
          env.cfg.etherscanApiKey
          explorerUrl
          addr
        when (isJust result) $ putStrLn "Success!"
        when (isNothing result) $ putStrLn "Failed!"
        pure result

  -- Convert to SolcContract
  case sourceData of
    Just sd -> buildSolcContract runtimeCode sd
    Nothing -> pure Nothing

-- | Build SolcContract and SourceCache from SourceData
buildSolcContract :: BS.ByteString -> SourceData -> IO (Maybe (SourceCache, SolcContract))
buildSolcContract runtimeCode sd = do
  -- Build SourceCache from multiple source files
  let sourcesList = Map.toList sd.sourceFiles
      filesMap = Map.fromList $ zip [0..]
        (fmap (\(path, content) -> (Text.unpack path, UTF8.fromString $ Text.unpack content)) sourcesList)
      sourceCache = SourceCache
        { files = filesMap
        , lines = Vector.fromList . BS.split 0xa . snd <$> filesMap
        , asts = mempty
        }

  -- Parse source maps safely
  runtimeSrcmap <- case sd.runtimeSrcMap of
    Just sm -> makeSrcMapsSafe sm
    Nothing -> pure mempty

  creationSrcmap <- case sd.creationSrcMap of
    Just sm -> makeSrcMapsSafe sm
    Nothing -> pure mempty

  -- Build ABI maps
  -- TODO: Need mkAbiMap, mkEventMap, mkErrorMap to be exported from hevm
  -- For now, we keep them as mempty but at least we have the ABI data available
  let (abiMap', eventMap', errorMap') = (mempty, mempty, mempty)

  let solcContract = SolcContract
        { runtimeCode = runtimeCode
        , creationCode = mempty
        , runtimeCodehash = keccak' runtimeCode
        , creationCodehash = keccak' mempty
        , runtimeSrcmap = runtimeSrcmap
        , creationSrcmap = creationSrcmap
        , contractName = sd.contractName
        , constructorInputs = []
        , abiMap = abiMap'
        , eventMap = eventMap'
        , errorMap = errorMap'
        , storageLayout = Nothing
        , immutableReferences = fromMaybe mempty sd.immutableRefs
        }

  pure $ Just (sourceCache, solcContract)

-- | Safe wrapper for makeSrcMaps to prevent crashes
makeSrcMapsSafe :: Text.Text -> IO (Seq SrcMap)
makeSrcMapsSafe txt =
  catch (pure $ fromMaybe mempty $! makeSrcMaps txt)
        (\(_ :: SomeException) -> pure mempty)


saveCoverageReport :: Env -> Int -> IO ()
saveCoverageReport env runId = do
  case env.cfg.campaignConf.corpusDir of
    Nothing -> pure ()
    Just dir -> do
      -- coverage reports for external contracts
      -- Get contracts from hevm session cache
      sessionCache <- readMVar env.fetchSession.sharedCache
      explorerUrl <- Etherscan.getBlockExplorerUrl env.chainId
      let contractsCache = EVM.Fetch.makeContractFromRPC <$> sessionCache.contractCache
      forM_ (Map.toList contractsCache) $ \(addr, contract) -> do
        r <- externalSolcContract env explorerUrl addr contract
        case r of
          Just (externalSourceCache, solcContract) -> do
            let dir' = dir </> show addr
            saveCoverages env
                          runId
                          dir'
                          externalSourceCache
                          [solcContract]
          Nothing -> pure ()

fetchChainIdFrom :: Maybe Text -> IO (Maybe W256)
fetchChainIdFrom (Just url) = do
  sess <- Session.newAPISession
  res <- EVM.Fetch.fetchQuery
    EVM.Fetch.Latest -- this shouldn't matter
    (EVM.Fetch.fetchWithSession url sess)
    EVM.Fetch.QueryChainId
  pure $ either (const Nothing) Just res
fetchChainIdFrom Nothing = pure Nothing
