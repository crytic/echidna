module Echidna.Libraries where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.Reader (liftIO, MonadReader, MonadIO)
import Control.Monad.ST (RealWorld)
import Data.Aeson (FromJSON(..), withObject, (.:), eitherDecode)
import Data.ByteString.Lazy qualified as LBS
import Data.List (find, isSuffixOf)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory (listDirectory)
import System.FilePath ((</>))

import EVM.Solidity
import EVM.Types hiding (Env)

import Echidna.Deploy (deployContracts)
import Echidna.Types.Config (Env(..))

-- | Library linking information from crytic-compile --compile-autolink
data LibraryLinking = LibraryLinking
  { deploymentOrder :: [Text]
  , libraryAddresses :: Map Text Addr
  } deriving (Show)

instance FromJSON LibraryLinking where
  parseJSON = withObject "LibraryLinking" $ \o -> do
    deploymentOrder <- o .: "deployment_order"
    libraryAddresses <- o .: "library_addresses"
    pure LibraryLinking{deploymentOrder, libraryAddresses}

-- | Try to read library linking information
readLibraryLinking :: FilePath -> IO (Maybe LibraryLinking)
readLibraryLinking d = do
  fs <- filter (".link" `Data.List.isSuffixOf`) <$> listDirectory d
  case fs of
    [] -> pure Nothing
    [linkingFile] -> do
      content <- LBS.readFile $ d </> linkingFile
      case eitherDecode content of
        Left err -> do
          putStrLn $ "Warning: Failed to parse " <> linkingFile <> ": " <> err
          pure Nothing
        Right linking -> pure (Just linking)
    _ -> error $ "Multiple link files found: " <> show fs <> "\n"

-- | Deploy libraries using autolink information if available
deployAutolinkLibraries
  :: (MonadIO m, MonadReader Env m, MonadThrow m)
  => [SolcContract]
  -> Addr
  -> VM Concrete RealWorld
  -> m (VM Concrete RealWorld)
deployAutolinkLibraries cs deployer vm = do
  linking <- liftIO $ readLibraryLinking "crytic-export"
  case linking of
    Nothing -> pure vm
    Just (LibraryLinking deploymentOrder libraryAddresses) -> do
      -- Deploy libraries in the specified order at the specified addresses
      let orderedLibs = mapMaybe (\libName -> do
            addr <- Map.lookup libName libraryAddresses
            contract <- find (\c -> T.isSuffixOf (":" <> libName) c.contractName) cs
            pure (addr, contract)) deploymentOrder
      deployContracts orderedLibs deployer vm
