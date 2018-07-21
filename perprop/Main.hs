{-# LANGUAGE LambdaCase, FlexibleContexts, TemplateHaskell, TupleSections #-}

module Main where

import Control.Concurrent.MVar (MVar, newMVar, readMVar, swapMVar)
import Control.Lens
import Control.Monad           (forM, forM_ , replicateM_)
import Control.Monad.Identity  (Identity(..))
import Control.Monad.Reader    (runReaderT)
import Data.List               (foldl')
import Data.Set                (unions)
import Data.Text               (Text, unpack)
import Data.Yaml
import EVM                     (VM)
import EVM.Types               (Addr)
import System.Environment      (getArgs)

import qualified Data.ByteString as BS

import Hedgehog hiding (checkParallel, Property)
import Hedgehog.Internal.Property (GroupName(..), PropertyName(..))

import Echidna.ABI
import Echidna.Config
import Echidna.Exec
import Echidna.Property
import Echidna.Solidity


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
    <$> ((v .: "testLimit" :: Data.Yaml.Parser Int) <&> fromIntegral)
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
    defaultConfig & addrList .~ Just (view address <$> s) & testLimit .~ t & epochs .~ 1 & outputJson .~ True

group :: String
      -> Config
      -> [SolSignature]
      -> VM
      -> [(Property, [SolCall], MVar [CoverageInfo])]
      -> Group
group n c a v ps = Group (GroupName n) $ map prop ps where
  prop (Property f r, cov, mvar) = ( PropertyName $ show f
                                   , useConfig (ePropertySeqCoverage cov mvar (flip (checkTest r) f) a v))

  useConfig = runIdentity . (`runReaderT` c)

-- }}}
-- Main
-- {{{

main :: IO ()
main = getArgs >>= \case
  [cf,sf] -> readConf cf >>= \case
    Nothing       -> pure ()
    (Just (c, ps)) -> do
      (v,a,t) <- runReaderT (loadSolidity sf Nothing) c
      forM_ (map (view function) ps) $ \p -> if p `elem` (t ++ map fst a)
        then pure ()
        else error $ "Property " ++ unpack p ++ " not found in ABI"
      tests <- mapM (\p -> fmap (p,) (newMVar [])) ps
      replicateM_ (c ^. epochs) $ do
        xs <- forM tests $ \(p,mvar) -> do
          cov     <- readMVar mvar
          lastGen <- getCover cov
          _       <- swapMVar mvar []
          pure (p,lastGen,mvar)

        checkParallelJson $ group sf c a v xs

      ls <- mapM (readMVar . snd) tests
      let ci = foldl' (\acc xs -> unions (acc : map snd xs)) mempty ls
      putStrLn $ ppHashes (byHashes ci)

  _ -> putStrLn "USAGE: ./perprop-exe config.yaml contract.sol"
  
-- }}}
