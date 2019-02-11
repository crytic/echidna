{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Echidna.Solidity where

import Control.Lens
import Control.Exception          (Exception)
import Control.Monad              (liftM2, mapM_)
import Control.Monad.Catch        (MonadThrow(..))
import Control.Monad.IO.Class     (MonadIO(..))
import Control.Monad.Reader       (MonadReader)
import Control.Monad.State.Strict (execStateT)
import Data.Foldable              (toList)
import Data.Has                   (Has(..))
import Data.List                  (find, findIndices, partition)
import Data.Maybe                 (isNothing)
import Data.Monoid                ((<>))
import Data.Text                  (Text, isPrefixOf, pack, unpack)
import System.Process             (readProcess)
import System.IO.Temp             (writeSystemTempFile)

import Echidna.ABI (SolSignature)
import Echidna.Exec (execTx)
import Echidna.Transaction (Tx(..))

import EVM hiding (contracts)
import EVM.Exec     (vmForEthrunCreation)
import EVM.Solidity
import EVM.Types    (Addr)

import qualified Data.ByteString as BS

-- | Things that can go wrong trying to load a Solidity file for Echidna testing. Read the 'Show'
-- instance for more detailed explanations.
data SolException = BadAddr Addr
                  | CompileFailure
                  | NoContracts
                  | TestArgsFound Text
                  | ContractNotFound Text
                  | NoBytecode Text
                  | NoFuncs
                  | NoTests
                  | OnlyTests

instance Show SolException where
  show = \case
    BadAddr a            -> "No contract at " ++ show a ++ " exists"
    CompileFailure       -> "Couldn't compile given file"
    NoContracts          -> "No contracts found in given file"
    (ContractNotFound c) -> "Given contract " ++ show c ++ " not found in given file"
    (TestArgsFound t)    -> "Test " ++ show t ++ " has arguments, aborting"
    (NoBytecode t)       -> "No bytecode found for contract " ++ show t
    NoFuncs              -> "ABI is empty, are you sure your constructor is right?"
    NoTests              -> "No tests found in ABI"
    OnlyTests            -> "Only tests and no public functions found in ABI"

instance Exception SolException

-- | Configuration for loading Solidity for Echidna testing.
data SolConf = SolConf { _contractAddr :: Addr   -- ^ Contract address to use
                       , _deployer     :: Addr   -- ^ Contract deployer address to use
                       , _sender       :: [Addr] -- ^ Sender addresses to use
                       , _prefix       :: Text   -- ^ Function name prefix used to denote tests
                       , _solcArgs     :: String -- ^ Args to pass to @solc@
                       }
makeLenses ''SolConf

-- | Given a file, try to compile it and get a list of its contracts, throwing exceptions if necessary.
contracts :: (MonadIO m, MonadThrow m, MonadReader x m, Has SolConf x) => FilePath -> m [SolcContract]
contracts fp = view (hasLens . solcArgs) >>= liftIO . solc >>= (\case
  Nothing -> throwM CompileFailure
  Just m  -> pure . toList $ fst m) where
    solc a = readSolc =<< writeSystemTempFile "" =<< readProcess "solc" (usual <> words a) ""
    usual = ["--combined-json=bin-runtime,bin,srcmap,srcmap-runtime,abi,ast", fp]
    
-- | Given a file and a possible contract name, compile the file as solidity, then, if a name is
-- given, try to return the specified contract, otherwise, return the first contract in the file,
-- throwing errors if necessary.
selected :: (MonadIO m, MonadThrow m, MonadReader x m, Has SolConf x) => FilePath -> Maybe Text -> m SolcContract
selected fp name = do cs <- contracts fp
                      c <- choose cs $ ((pack fp <> ":") <>) <$> name
                      liftIO $ if isNothing name && length cs > 1
                        then putStrLn "Multiple contracts found in file, only analyzing the first"
                        else pure ()
                      liftIO . putStrLn $ "Analyzing contract: " <> unpack (c ^. contractName)
                      return c
  where choose []    _        = throwM NoContracts
        choose (c:_) Nothing  = return c
        choose cs    (Just n) = maybe (throwM $ ContractNotFound n) pure $
                                  find ((n ==) . view contractName) cs

-- | Given a file and a possible contract name, compile the file as solidity, then, if a name is
-- given, try to fine the specified contract, otherwise, find the first contract in the file. Take
-- said contract and return an initial VM state with it loaded, its ABI (as 'SolSignature's), and the
-- names of its Echidna tests.
loadSolidity :: (MonadIO m, MonadThrow m, MonadReader x m, Has SolConf x)
             => FilePath -> Maybe Text -> m (VM, [SolSignature], [Text])
loadSolidity fp name = let ensure (l, e) = if null l then throwM e else pure () in do
    c <- selected fp name
    (SolConf ca d _ pref _) <- view hasLens
    let bc = c ^. creationCode
        abi = map (liftM2 (,) (view methodName) (fmap snd . view methodInputs)) . toList $ c ^. abiMap
        (tests, funs) = partition (isPrefixOf pref . fst) abi
    loaded <- execStateT (execTx $ Tx (Right bc) d ca 0) $ vmForEthrunCreation bc
    mapM_ ensure [(abi, NoFuncs), (tests, NoTests), (funs, OnlyTests)]
    liftIO $ print $ findConstants bc 
    case find (not . null . snd) tests of
      (Just (t,_)) -> throwM $ TestArgsFound t
      Nothing      -> return (loaded, funs, fst <$> tests)

takeConstant :: Int -> ([Integer],[Integer]) -> [Integer]
takeConstant n (_,xs) = take n $ drop 1 xs 

findPush :: Int -> [Integer] -> [Int]
findPush n =  findIndices (\x -> x >= 0x60 && x <= 0x60 + (toInteger (n-1)) )   

findConstants_ :: Int -> [Integer] -> [[Integer]]
findConstants_ n bs = map ((takeConstant n) . (flip splitAt bs)) (findPush n bs)
 
findConstants :: BS.ByteString -> [[Integer]]
findConstants bc = let bs = map toInteger $ BS.unpack bc 
                  in (findConstants_ 1 bs) ++ (findConstants_ 2 bs) ++ (findConstants_ 3 bs)  
