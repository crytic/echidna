{-# LANGUAGE LambdaCase, FlexibleContexts, TemplateHaskell, TupleSections #-}

module Main where

import System.IO               (hPrint, stderr)
import Control.Concurrent.MVar (MVar, newMVar, readMVar, swapMVar)
import Control.Lens
import Control.Monad           (forM, forM_, replicateM_)
import Control.Monad.Catch     (MonadThrow(..))
import Control.Monad.Identity  (Identity(..))
import Control.Monad.Reader    (runReaderT)
import Control.Monad.IO.Class  (liftIO)
import Data.List               ((\\), foldl')
import Data.Semigroup          ((<>))
import Data.Set                (unions)
import Data.Text               (Text, unpack, pack)
import Data.Yaml
import EVM                     (VM)
import EVM.Types               (Addr)

import qualified Data.ByteString as BS
import qualified Data.Text as T

--import Hedgehog hiding (checkParallel, Property)
--import Hedgehog.Internal.Property (GroupName(..), PropertyName(..))

import Echidna.ABI
import Echidna.Config
--import Echidna.Coverage
import Echidna.Exec
import Echidna.Property
import Echidna.Solidity

import Options.Applicative hiding (Parser, argument)
import Options.Applicative as O

warn :: Show a => a -> IO ()
warn = hPrint stderr

-- Cmd line parser
-- {{{

data Options = Options
  { filePath         :: FilePath
  , configFilepath   :: FilePath
  , selectedContract :: Maybe String
  }
  

options :: O.Parser Options
options = Options
      <$> O.argument str
          ( metavar "FILE"
         <> help "Solidity file to analyze" )
      <*> O.argument str
          ( metavar "CONFIG"
         <> help "Config file" )
      <*> optional ( O.argument str
          ( metavar "CONTRACT"
         <> help "Contract inside of file to analyze" ))


opts :: ParserInfo Options
opts = info (options <**> helper)
  ( fullDesc
  <> progDesc "Fuzzing/property based testing of EVM code"
  <> header "Echidna - Ethereum fuzz testing framework" )

-- }}}
-- Types & instances
-- {{{

data Sender = Sender {
    _address   :: Addr
  , _name      :: String
  } deriving Show

data Property = Property {
    _function :: Text
  , _return   :: PropertyType
  } deriving Show

data PerPropConf = PerPropConf {
    _testLimit' :: Int
  , _sender     :: [Sender]
  , _properties :: [Property]
  } deriving Show

makeLenses ''Sender
makeLenses ''Property
makeLenses ''PerPropConf

instance FromJSON Sender where
  parseJSON (Object v) = Sender <$> v .: "address" <*> v.: "name"
  parseJSON _ = mempty

instance FromJSON Property where
  parseJSON (Object v) = Property <$> fmap (T.takeWhile (/= '(')) (v .: "name")
                                  <*> v .: "return"
  parseJSON _ = mempty

instance FromJSON PerPropConf where
  parseJSON (Object v) = PerPropConf
    <$> v .: "testLimit"
    <*> v .: "sender"
    <*> v .: "properties"
  parseJSON _ = mempty

-- }}}
-- Parsing a config
-- {{{

readConf :: FilePath -> IO (Maybe (Config, [Property]))
readConf f = decodeEither <$> BS.readFile f >>= \case
  Left e -> putStrLn ("couldn't parse config, " ++ e) >> pure Nothing
  Right (PerPropConf t s p) -> pure . Just . (,p) $
    defaultConfig & addrList ?~ (view address <$> s) & range .~ t & epochs .~ 1 & outputJson .~ True

makeProperty :: Property -> (Text, VM -> Bool)
makeProperty (Property f r) = (f, flip (checkTest r) f)

-- }}}
-- Main
-- {{{

main :: IO ()
main = do
  (Options file configFile contract) <- execParser opts
  readConf configFile >>= \case
    Nothing        -> pure ()
    (Just (c, ps)) -> do
      if null ps then throwM NoTests else pure ()
      (v,a,t) <- runReaderT (loadSolidity file (pack <$> contract)) c
      let abi = t ++ map fst a
      forM_ ((view function <$> ps) \\ abi) $ \p ->
        warn $ "Warning: property " ++ unpack p ++ " not found in ABI"
      let ts =  [ p | p <- ps, p ^. function `elem` abi ]
      liftIO $ ePropertySeq (map makeProperty ts) a v c
