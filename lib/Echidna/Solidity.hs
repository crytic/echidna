{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Echidna.Solidity where

import Control.Lens hiding        (Indexed)
import Control.Exception          (Exception)
import Control.Monad              (liftM2, mapM_, when, unless, void)
import Control.Monad.Catch        (MonadThrow(..))
import Control.Monad.IO.Class     (MonadIO(..))
import Control.Monad.Reader       (MonadReader)
import Control.Monad.State.Strict (execStateT)
import Data.Aeson                 (Value(..))
import Data.Aeson.Lens
import Data.ByteString.Lens       (packedChars)
import Data.DoubleWord            (Int256, Word256)
import Data.Foldable              (toList, fold)
import Data.Has                   (Has(..))
import Data.List                  (find, nub, partition)
import Data.List.Lens             (prefixed, suffixed)
import Data.Maybe                 (isNothing, catMaybes, fromMaybe)
import Data.Monoid                ((<>))
import Data.Sequence              (Seq)
import Data.Text                  (Text, isPrefixOf, isSuffixOf)
import Data.Text.Encoding         (encodeUtf8)
import Data.Text.Lens             (unpacked)
import Data.Text.Read             (decimal)
import Data.Word
import System.Process             (StdStream(..), readCreateProcess, proc, std_err)
import System.IO                  (openFile, IOMode(..))

import Echidna.ABI         (SolSignature, AbiType2(..), AbiValue2(..), SolSignature2)
import Echidna.Exec        (execTx)
import Echidna.Transaction (Tx(..), World(..), World2(..))

import EVM hiding (contracts)
import qualified EVM (contracts)
import EVM.ABI      (AbiType, AbiValue(..), Indexed(..), Event(..), Anonymity(..), parseTypeName)
import EVM.Exec     (vmForEthrunCreation)
import EVM.Keccak   (keccak, abiKeccak)
import EVM.Solidity
import EVM.Types    (Addr, W256)
import EVM.Concrete (w256)

import qualified Data.ByteString     as BS
import qualified Data.ByteString.Base16 as BS16
import qualified Data.HashMap.Strict as M
import qualified Data.Map.Strict     as Map
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import qualified Data.Vector         as V
import qualified Text.Megaparsec      as P
import qualified Text.Megaparsec.Char as P

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
data SolConf = SolConf { _contractAddr    :: Addr     -- ^ Contract address to use
                       , _deployer        :: Addr     -- ^ Contract deployer address to use
                       , _sender          :: [Addr]   -- ^ Sender addresses to use
                       , _balanceAddr     :: Integer  -- ^ Initial balance of deployer and senders
                       , _balanceContract :: Integer  -- ^ Initial balance of contract to test
                       , _prefix          :: Text     -- ^ Function name prefix used to denote tests
                       , _solcArgs        :: String   -- ^ Args to pass to @solc@
                       , _solcLibs        :: [String] -- ^ List of libraries to load, in order.
                       , _quiet           :: Bool     -- ^ Suppress @solc@ output, errors, and warnings
                       , _checkAsserts    :: Bool     -- ^ Test if we can cause assertions to fail
                       }
makeLenses ''SolConf

data SolcContract2 = SolcContract2
  { _runtimeCodehash2  :: W256
  , _creationCodehash2 :: W256
  , _runtimeCode2      :: BS.ByteString
  , _creationCode2     :: BS.ByteString
  , _contractName2     :: Text
  , _constructorInputs2 :: [(Text, AbiType2)]
  , _abiMap2           :: Map.Map Word32 Method2
  , _eventMap2         :: Map.Map W256 Event2
  , _runtimeSrcmap2    :: Seq SrcMap
  , _creationSrcmap2   :: Seq SrcMap
  , _contractAst2      :: Value
  } deriving (Show, Eq)

data Method2 = Method2
  { _methodOutput2 :: Maybe (Text, AbiType2)
  , _methodInputs2 :: [(Text, AbiType2)]
  , _methodName2 :: Text
  , _methodSignature2 :: Text
  } deriving (Show, Eq, Ord)

data Event2 = Event2 Text Anonymity [(AbiType2, Indexed)]
  deriving (Show, Ord, Eq)

makeLenses ''SolcContract2
makeLenses ''Method2

-- | An Echidna test is either the name of the function to call and the address where its contract is,
-- or a function that could experience an exception
type SolTest = Either (Text, Addr) SolSignature2

toCode :: Text -> BS.ByteString
toCode = fst . BS16.decode . encodeUtf8

force :: String -> Maybe a -> a
force s = fromMaybe (error s)

parseMethodInput2 :: (Show s, AsValue s) => s -> (Text, AbiType2)
parseMethodInput2 x =
  ( x ^?! key "name" . _String
  , force "internal error: method type" (parseTypeName2 (x ^?! key "type" . _String))
  )

readSolc2 :: FilePath -> IO (Maybe (Map.Map Text SolcContract2, SourceCache))
readSolc2 fp =
  (readJSON2 <$> TIO.readFile fp) >>=
    \case
      Nothing -> return Nothing
      Just (contracts, asts, sources) -> do
        sourceCache <- makeSourceCache sources asts
        return $! Just (contracts, sourceCache)

readJSON2 :: Text -> Maybe (Map.Map Text SolcContract2, Map.Map Text Value, [Text])
readJSON2 json = do
  contracts <-
    f <$> (json ^? key "contracts" . _Object)
      <*> (fmap (fmap (^. _String)) $ json ^? key "sourceList" . _Array)
  sources <- toList . fmap (view _String) <$> json ^? key "sourceList" . _Array
  return (contracts, Map.fromList (M.toList asts), sources)
  where
    asts = fromMaybe (error "JSON lacks abstract syntax trees.") (json ^? key "sources" . _Object)
    f x y = Map.fromList . map (g y) . M.toList $ x
    g _ (s, x) =
      let
        theRuntimeCode = toCode (x ^?! key "bin-runtime" . _String)
        theCreationCode = toCode (x ^?! key "bin" . _String)
        abis =
          toList ((x ^?! key "abi" . _String) ^?! _Array)
      in (s, SolcContract2 {
        _runtimeCode2      = theRuntimeCode,
        _creationCode2     = theCreationCode,
        _runtimeCodehash2  = keccak (stripBytecodeMetadata theRuntimeCode),
        _creationCodehash2 = keccak (stripBytecodeMetadata theCreationCode),
        _runtimeSrcmap2    = force "internal error: srcmap-runtime" (makeSrcMaps (x ^?! key "srcmap-runtime" . _String)),
        _creationSrcmap2   = force "internal error: srcmap" (makeSrcMaps (x ^?! key "srcmap" . _String)),
        _contractName2 = s,
        _contractAst2 =
          fromMaybe
            (error "JSON lacks abstract syntax trees.")
            (preview (ix (T.split (== ':') s !! 0) . key "AST") asts),

        _constructorInputs2 =
          let
            isConstructor y =
              "constructor" == y ^?! key "type" . _String
          in
            case filter isConstructor abis of
              [abi] -> map parseMethodInput2 (toList (abi ^?! key "inputs" . _Array))
              [] -> [] -- default constructor has zero inputs
              _  -> error "strange: contract has multiple constructors",

        _abiMap2       = Map.fromList $
          let
            relevant =
              filter (\y -> "function" == y ^?! key "type" . _String) abis
          in flip map relevant $
            \abi -> (
              abiKeccak (encodeUtf8 (signature abi)),
              Method2
                { _methodName2 = abi ^?! key "name" . _String
                , _methodSignature2 = signature abi
                , _methodInputs2 =
                    map parseMethodInput2
                      (toList (abi ^?! key "inputs" . _Array))
                , _methodOutput2 =
                    fmap parseMethodInput2
                      (abi ^? key "outputs" . _Array . ix 0)
                }
            ),
        _eventMap2     = Map.fromList $
          flip map (filter (\y -> "event" == y ^?! key "type" . _String)
                     . toList $ (x ^?! key "abi" . _String) ^?! _Array) $
            \abi ->
              ( keccak (encodeUtf8 (signature abi))
              , Event2
                  (abi ^?! key "name" . _String)
                  (case abi ^?! key "anonymous" . _Bool of
                     True -> Anonymous
                     False -> NotAnonymous)
                  (map (\y -> ( force "internal error: type" (parseTypeName2 (y ^?! key "type" . _String))
                              , if y ^?! key "indexed" . _Bool
                                then Indexed
                                else NotIndexed ))
                    (toList $ abi ^?! key "inputs" . _Array))
              )
      })

signature :: AsValue s => s -> Text
signature abi =
  case abi ^?! key "type" of
    "fallback" -> "<fallback>"
    _ ->
      fold [
        fromMaybe "<constructor>" (abi ^? key "name" . _String), "(",
        T.intercalate ","
          (map (\x -> x ^?! key "type" . _String)
            (toList $ abi ^?! key "inputs" . _Array)),
        ")"
      ]

makeSourceCache :: [Text] -> Map.Map Text Value -> IO SourceCache
makeSourceCache paths asts = do
  xs <- mapM (BS.readFile . T.unpack) paths
  return $! SourceCache
    { _snippetCache = mempty
    , _sourceFiles =
        Map.fromList (zip [0..] (zip paths xs))
    , _sourceLines =
        Map.fromList (zip [0 .. length paths - 1]
                       (map (V.fromList . BS.split 0xa) xs))
    , _sourceAsts =
        asts
    }

parseTypeName2 :: Text -> Maybe AbiType2
parseTypeName2 = P.parseMaybe typeWithArraySuffix

typeWithArraySuffix :: P.Parsec () Text AbiType2
typeWithArraySuffix = do
  base <- basicType2
  sizes <-
    P.many $
      P.between
        (P.char '[') (P.char ']')
        (P.many P.digitChar)

  let
    parseSize :: AbiType2 -> [Char] -> AbiType2
    parseSize t "" = AbiArrayDynamicType2 t
    parseSize t s  = AbiArrayType2 (read s) t

  pure (foldl parseSize base sizes)

basicType2 :: P.Parsec () Text AbiType2
basicType2 =
  P.choice
    [ P.string "address" *> pure AbiAddressType2
    , P.string "bool"    *> pure AbiBoolType2
    , P.string "string"  *> pure AbiStringType2

    , sizedType "uint" AbiUIntType2
    , sizedType "int"  AbiIntType2
    , sizedType "bytes" AbiBytesType2

    , P.string "bytes" *> pure AbiBytesDynamicType2
    ]

  where
    sizedType :: Text -> (Int -> AbiType2) -> P.Parsec () Text AbiType2
    sizedType s f = P.try $ do
      void (P.string s)
      fmap (f . read) (P.some P.digitChar)

-- | Given a file, use its extenstion to check if it is a precompiled contract or try to compile it and
-- get a list of its contracts, throwing exceptions if necessary.
contracts :: (MonadIO m, MonadThrow m, MonadReader x m, Has SolConf x) => FilePath -> m [SolcContract2]
contracts fp = let usual = ["--solc-disable-warnings", "--export-format", "solc"] in do
  a  <- view (hasLens . solcArgs)
  q  <- view (hasLens . quiet)
  ls <- view (hasLens . solcLibs)
  let solargs = a ++ linkLibraries ls & (usual ++) . 
                  (\sa -> if null sa then [] else ["--solc-args", sa])
  maybe (throwM CompileFailure) (pure . toList . fst) =<< liftIO (do
    stderr <- if q then UseHandle <$> openFile "/dev/null" WriteMode else pure Inherit
    _ <- readCreateProcess (proc "crytic-compile" $ solargs |> fp) {std_err = stderr} ""
    readSolc2 "crytic-export/combined_solc.json")


addresses :: (MonadReader x m, Has SolConf x) => m [AbiValue2]
addresses = view hasLens <&> \(SolConf ca d ads _ _ _ _ _ _ _) ->
  AbiAddress2 . fromIntegral <$> nub (ads ++ [ca, d, 0x0])

populateAddresses :: [Addr] -> Integer -> VM -> VM
populateAddresses []     _ vm = vm
populateAddresses (a:as) b vm = populateAddresses as b (vm & set (env . EVM.contracts . at a) (Just account))
  where account = initialContract (RuntimeCode mempty) & set nonce 1 & set balance (w256 $ fromInteger b)

-- | Address to load the first library
addrLibrary :: Addr
addrLibrary = 0xff

 -- | Load a list of solidity contracts as libraries
loadLibraries :: (MonadIO m, MonadThrow m, MonadReader x m, Has SolConf x)
              => [SolcContract2] -> Addr -> Addr -> VM -> m VM
loadLibraries []     _  _ vm = return vm
loadLibraries (l:ls) la d vm = loadLibraries ls (la + 1) d =<< loadRest
  where loadRest = execStateT (execTx $ Tx (Right $ l ^. creationCode2) d la 0xffffffff 0) vm

-- | Generate a string to use as argument in solc to link libraries starting from addrLibrary
linkLibraries :: [String] -> String
linkLibraries [] = ""
linkLibraries ls = "--libraries " ++
  iconcatMap (\i x -> concat [x, ":", show $ addrLibrary + toEnum i, ","]) ls

-- | Given an optional contract name and a list of 'SolcContract's, try to load the specified
-- contract, or, if not provided, the first contract in the list, into a 'VM' usable for Echidna
-- testing and extract an ABI and list of tests. Throws exceptions if anything returned doesn't look
-- usable for Echidna. NOTE: Contract names passed to this function should be prefixed by the
-- filename their code is in, plus a colon.
loadSpecified :: (MonadIO m, MonadThrow m, MonadReader x m, Has SolConf x)
              => Maybe Text -> [SolcContract2] -> m (VM, [SolSignature2], [Text])
loadSpecified name cs = let ensure l e = if l == mempty then throwM e else pure () in do
  -- Pick contract to load
  c <- choose cs name
  q <- view (hasLens . quiet)
  liftIO $ do
    when (isNothing name && length cs > 1 && not q) $
      putStrLn "Multiple contracts found in file, only analyzing the first"
    unless q . putStrLn $ "Analyzing contract: " <> c ^. contractName2 . unpacked

  -- Local variables
  (SolConf ca d ads bala balc pref _ libs _ ch) <- view hasLens
  let bc = c ^. creationCode2
      blank = populateAddresses (ads |> d) bala (vmForEthrunCreation bc)
      abi = liftM2 (,) (view methodName2) (fmap snd . view methodInputs2) <$> toList (c ^. abiMap2)
      (tests, funs) = partition (isPrefixOf pref . fst) abi

  -- Select libraries
  ls <- mapM (choose cs . Just . T.pack) libs

  -- Make sure everything is ready to use, then ship it
  mapM_ (uncurry ensure) $ [(abi, NoFuncs), (funs, OnlyTests)]
                        ++ if ch then [] else [(tests, NoTests)] -- ABI checks
  ensure bc (NoBytecode $ c ^. contractName2)                    -- Bytecode check
  case find (not . null . snd) tests of
    Just (t,_) -> throwM $ TestArgsFound t                       -- Test args check
    Nothing    -> loadLibraries ls addrLibrary d blank >>= fmap (, fallback : funs, fst <$> tests) .
      execStateT (execTx $ Tx (Right bc) d ca 0xffffffff (w256 $ fromInteger balc))


  where choose []    _        = throwM NoContracts
        choose (c:_) Nothing  = return c
        choose _     (Just n) = maybe (throwM $ ContractNotFound n) pure $
                                      find (isSuffixOf n . view contractName2) cs
        fallback = ("",[])

-- | Given a file and an optional contract name, compile the file as solidity, then, if a name is
-- given, try to fine the specified contract (assuming it is in the file provided), otherwise, find
-- the first contract in the file. Take said contract and return an initial VM state with it loaded,
-- its ABI (as 'SolSignature's), and the names of its Echidna tests. NOTE: unlike 'loadSpecified',
-- contract names passed here don't need the file they occur in specified.
--loadSolidity :: (MonadIO m, MonadThrow m, MonadReader x m, Has SolConf x)
--             => FilePath -> Maybe Text -> m (VM, [SolSignature], [Text])
--loadSolidity fp name = contracts fp >>= loadSpecified name
loadWithCryticCompile :: (MonadIO m, MonadThrow m, MonadReader x m, Has SolConf x)
             => FilePath -> Maybe Text -> m (VM, [SolSignature2], [Text])
loadWithCryticCompile fp name = contracts fp >>= loadSpecified name

-- | Given the results of 'loadSolidity', assuming a single-contract test, get everything ready
-- for running a 'Campaign' against the tests found.
prepareForTest :: (MonadReader x m, Has SolConf x)
               => (VM, [SolSignature2], [Text]) -> m (VM, World2, [SolTest])
prepareForTest (v, a, ts) = view hasLens <&> \(SolConf _ _ s _ _ _ _ _ _ ch) ->
  (v, World2 s [(r, a)], fmap Left (zip ts $ repeat r) ++ if ch then Right <$> drop 1 a else []) where
    r = v ^. state . contract

-- | Basically loadSolidity, but prepares the results to be passed directly into
-- a testing function.
loadSolTests :: (MonadIO m, MonadThrow m, MonadReader x m, Has SolConf x)
             => FilePath -> Maybe Text -> m (VM, World2, [SolTest])
loadSolTests fp name = loadWithCryticCompile fp name >>= prepareForTest

mkValidAbiInt :: Int -> Int256 -> Maybe AbiValue2
mkValidAbiInt i x = if abs x <= 2 ^ (i - 1) - 1 then Just $ AbiInt2 i x else Nothing

mkValidAbiUInt :: Int -> Word256 -> Maybe AbiValue2
mkValidAbiUInt i x = if x <= 2 ^ i - 1 then Just $ AbiUInt2 i x else Nothing

-- | Given a list of 'SolcContract's, try to parse out string and integer literals
extractConstants :: [SolcContract2] -> [AbiValue2]
extractConstants = nub . concatMap (constants "" . view contractAst2) where
  -- Tools for parsing numbers and quoted strings from 'Text'
  asDecimal = preview $ to decimal . _Right . _1
  asQuoted  = preview $ unpacked . prefixed "\"" . suffixed "\"" . packedChars
  -- We need this because sometimes @solc@ emits a json string with a type, then a string
  -- representation of some value of that type. Why is this? Unclear. Anyway, this lets us match
  -- those cases like regular strings
  literal t f (String (T.words -> ((^? only t) -> m) : y : _)) = m *> f y
  literal _ _ _                                                = Nothing
  -- When we get a number, it could be an address, uint, or int. We'll try everything.
  dec i = let l f = f <$> [8,16..256] <*> fmap fromIntegral [i-1..i+1] in
    AbiAddress2 i : catMaybes (l mkValidAbiInt ++ l mkValidAbiUInt)
  -- 'constants' takes a property name and its 'Value', then tries to find solidity literals
  -- CASE ONE: we're looking at a big object with a bunch of little objects, recurse
  constants _ (Object o) = concatMap (uncurry constants) $ M.toList o
  constants _ (Array  a) = concatMap (constants "")        a
  -- CASE TWO: we're looking at a @type@, try to parse it
  -- 2.1: We're looking at a @int_const@ with a decimal number inside, could be an address, int, or uint
  --      @type: "int_const 0x12"@ ==> @[AbiAddress 18, AbiUInt 8 18,..., AbiUInt 256 18, AbiInt 8 18,...]@
  constants "typeString" (literal "int_const" asDecimal -> Just i) = dec i
  -- 2.2: We're looking at something of the form @type: literal_string "[...]"@, a string literal
  --      @type: "literal_string \"123\""@ ==> @[AbiString "123", AbiBytes 3 "123"...]@
  constants "typeString" (literal "literal_string" asQuoted -> Just b) =
    let size = BS.length b in [AbiString2 b, AbiBytesDynamic2 b] ++
      fmap (\n -> AbiBytes2 n . BS.append b $ BS.replicate (n - size) 0) [size..32]
  -- CASE THREE: we're at a leaf node with no constants
  constants _  _ = []

returnTypes :: [SolcContract2] -> Text -> Maybe AbiType2
returnTypes cs t = preview (_Just . methodOutput2 . _Just . _2) .
  find ((== t) . view methodName2) $ concatMap (toList . view abiMap2) cs
