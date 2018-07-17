{-# LANGUAGE LambdaCase, FlexibleContexts, TemplateHaskell, TupleSections #-}

module Main where

import Control.Concurrent.MVar (MVar, newMVar, readMVar, swapMVar)
import Control.Lens hiding     (argument)
import Control.Monad           (forM, replicateM_)
import Control.Monad.Identity  (Identity(..))
import Control.Monad.Reader    (runReaderT)
import Data.Semigroup          ((<>))
import Data.Text               (Text)
import Data.Yaml
import EVM                     (VM)
import EVM.Types               (Addr)

import qualified Data.ByteString as BS

import Hedgehog hiding (checkParallel, Property)
import Hedgehog.Internal.Property (GroupName(..), PropertyName(..))

import Options.Applicative hiding (Parser,argument)
import Options.Applicative as O

import Echidna.ABI
import Echidna.Config
import Echidna.Exec
import Echidna.Property
import Echidna.Solidity


-- Command line arguments parser
-- {{{

data Options = Options
  { filePath         :: FilePath
  , configFilepath   :: FilePath
  , coverageSelector :: Bool
  }

options :: O.Parser Options
options = Options
      <$> O.argument str
          ( metavar "FILE"
         <> help "Solidity file to analyze" )
      <*> O.argument str
          ( metavar "CONFIG"
         <> help "Echidna config file" )
      <*> switch
          ( long "coverage"
         <> help "Turn on coverage")

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
    defaultConfig & addrList .~ Just (view address <$> s) & testLimit .~ t

group :: String -> Config -> [SolSignature] -> VM -> [Property] -> Group
group n c a v p = Group (GroupName n) $ map prop p where
  prop (Property f r) = ( PropertyName $ show f
                        , useConfig (ePropertySeq (flip (checkTest r) f) a v))
  -- typechecker gets mad if we don't explicitly destack here idk why
  useConfig = runIdentity . (`runReaderT` c)

groupWithCoverage :: String
                  -> Config
                  -> [SolSignature]
                  -> VM
                  -> [(Property,[SolCall],MVar [CoverageInfo])]
                  -> Group
groupWithCoverage n c a v ps = Group (GroupName n) $ map prop ps where
  prop ((Property f r),cov,mvar) = ( PropertyName $ show f
                                   , useConfig (ePropertySeqCoverage cov mvar (flip (checkTest r) f) a v))

  useConfig = runIdentity . (`runReaderT` c)

-- }}}
-- Main
-- {{{
   
main :: IO ()
main = do
  (Options file config useCov) <- execParser opts
  readConf config >>= \case
    Nothing -> pure ()
    (Just (c,ps)) -> do
      (v,a,_) <- runReaderT (loadSolidity file Nothing) c
      if not useCov
        then do
        _ <- checkParallel $ group file c a v ps
        pure ()
        else do
        tests <- mapM (\p -> fmap (p,) (newMVar [])) ps
        replicateM_ (c ^. epochs) $ do
          xs <- forM tests $ \(p,mvar) -> do
            cov     <- readMVar mvar
            lastGen <- getCover cov
            _       <- swapMVar mvar []
            Prelude.return (p,lastGen,mvar)

          checkParallel $ groupWithCoverage file c a v xs

-- }}}
