{-# LANGUAGE LambdaCase, FlexibleContexts, TemplateHaskell, TupleSections #-}

module Main where

import Control.Lens
import Control.Monad.Identity (Identity(..))
import Control.Monad.Reader   (runReaderT)
import Data.Text              (Text)
import Data.Yaml
import EVM                    (VM)
import EVM.Types              (Addr)
import System.Environment     (getArgs)

import qualified Data.ByteString as BS

import Hedgehog hiding (checkParallel, Property)
import Hedgehog.Internal.Property (GroupName(..), PropertyName(..))

import Echidna.ABI
import Echidna.Config
import Echidna.Exec
import Echidna.Property
import Echidna.Solidity

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
    _testLimit' :: TestLimit
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
  parseJSON (Object v) = Property <$> v .: "name" <*> v .: "return"
  parseJSON _ = mempty

instance FromJSON PerPropConf where
  parseJSON (Object v) = PerPropConf
    <$> ((v .: "testLimit" :: Parser Int) <&> fromIntegral)
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
    defaultConfig & addrList .~ Just (view address <$> s) & testLimit .~ t

group :: String -> Config -> [SolSignature] -> VM -> [Property] -> Group
group n c a v p = Group (GroupName n) $ map prop p where
  prop (Property f r) = ( PropertyName $ show f
                        , useConfig (ePropertySeq (flip (checkTest r) f) a v))
  -- typechecker gets mad if we don't explicitly destack here idk why
  useConfig = runIdentity . (`runReaderT` c)

-- }}}
-- Main
-- {{{
   
main :: IO ()
main = getArgs >>= \case
  [cf,sf] -> readConf cf >>= \case
    Nothing       -> pure ()
    (Just (c, p)) -> do (v,a,_) <- runReaderT (loadSolidity sf Nothing) c
                        _ <- checkParallel $ group sf c a v p
                        pure ()
  _ -> putStrLn "USAGE: ./perprop-exe config.yaml contract.sol"

-- }}}
