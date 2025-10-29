{-# LANGUAGE DeriveAnyClass #-}

module Echidna.Onchain where

import Control.Exception (catch)
import Data.Aeson (ToJSON, FromJSON)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.UTF8 qualified as UTF8
import Data.Map qualified as Map
import Data.Maybe (isJust, fromJust)
import Data.Text qualified as Text
import Data.Text (Text)
import Data.Vector qualified as Vector
import Data.Word (Word64)
import Etherscan qualified
import GHC.Generics (Generic)
import Network.HTTP.Simple (HttpException)
import Optics (view)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Network.Wreq.Session qualified as Session
import Text.Read (readMaybe)

import EVM (initialContract, bytecode)
import EVM.Effects (defaultConfig)
import EVM.Fetch qualified
import EVM.Solidity (SourceCache(..), SolcContract (..))
import EVM.Types hiding (Env)

import Echidna.SymExec.Symbolic (forceWord, forceBuf)
import Echidna.Types (emptyAccount)
import Echidna.Types.Campaign (CampaignConf(..))
import Echidna.Types.Config (Env(..), EConfig(..))
import Echidna.Output.Source (saveCoverages)
import Control.Monad (when, forM_)
import Control.Concurrent.MVar (readMVar)

saveRpcCache :: Env -> IO ()
saveRpcCache env = do
  case (env.fetchSession.cacheDir, env.cfg.rpcBlock) of
    (Just dir, Just n) -> do
      cache <- readMVar (env.fetchSession.sharedCache)
      EVM.Fetch.saveCache dir (fromIntegral n) cache
    (_, Nothing) -> if (env.cfg.rpcUrl /= Nothing)
      then putStrLn "Warning: cannot save RPC cache without a specified block number."
      else pure ()
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

-- TODO: temporary solution, handle errors gracefully
safeFetchContractFrom :: EVM.Fetch.Session -> EVM.Fetch.BlockNumber -> Text -> Addr -> IO (Maybe Contract)
safeFetchContractFrom session rpcBlock rpcUrl addr = do
  catch
    (EVM.Fetch.fetchContractWithSession defaultConfig session rpcBlock rpcUrl addr)
    (\(_ :: HttpException) -> pure $ Just emptyAccount)

-- TODO: temporary solution, handle errors gracefully
safeFetchSlotFrom :: EVM.Fetch.Session -> EVM.Fetch.BlockNumber -> Text -> Addr -> W256 -> IO (Maybe W256)
safeFetchSlotFrom session rpcBlock rpcUrl addr slot =
  catch
    (EVM.Fetch.fetchSlotWithCache defaultConfig session rpcBlock rpcUrl addr slot)
    (\(_ :: HttpException) -> pure $ Just 0)

data FetchedContractData = FetchedContractData
  { runtimeCode :: ByteString
  , nonce :: Maybe W64
  , balance :: W256
  }
  deriving (Generic, ToJSON, FromJSON, Show)

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


saveCoverageReport :: Env -> Int -> IO ()
saveCoverageReport env runId = do
  case env.cfg.campaignConf.corpusDir of
    Nothing -> pure ()
    Just dir -> do
      -- coverage reports for external contracts, we only support
      -- Ethereum Mainnet for now
      when (env.chainId == Just 1) $ do
        -- Get contracts from hevm session cache
        sessionCache <- readMVar env.fetchSession.sharedCache
        let contractsCache = sessionCache.contractCache
        forM_ (Map.toList contractsCache) $ \(addr, contract) -> do
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

fetchChainIdFrom :: Maybe Text -> IO (Maybe W256)
fetchChainIdFrom (Just url) = do
  sess <- Session.newAPISession
  EVM.Fetch.fetchQuery
    EVM.Fetch.Latest -- this shouldn't matter
    (EVM.Fetch.fetchWithSession url sess)
    EVM.Fetch.QueryChainId
fetchChainIdFrom Nothing = pure Nothing
