-- | The textual syntax an MCP client uses to name a sequence of calls.
--
-- A sequence is calls separated by @;@, each written the way it would be in
-- Solidity, with @?@ standing for an argument left for the fuzzer to fill in:
--
-- > transfer(0x10, 100); approve(?, ?)
--
-- Parsing deliberately stops at an argument's shape rather than its type: an
-- integer becomes a @uint256@ and a @0x@-prefixed literal an @address@,
-- whatever the function's signature says. Calls are checked against the ABI in
-- 'Echidna.MCP', and only by name and arity — the same way
-- 'Echidna.Transaction.matchingContracts' resolves a prototype.
module Echidna.MCP.Parse
  ( parseArg
  , parseArray
  , parseFuzzArg
  , parseFuzzCall
  , parseFuzzSequence
  , parsePrimitive
  , splitArgs
  ) where

import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd, isPrefixOf, isSuffixOf)
import Data.List.Split (splitOn)
import Data.Text (pack)
import Data.Vector qualified as V
import Text.Read (readMaybe)

import EVM.ABI (AbiType(..), AbiValue(..), abiValueType)

import Echidna.Types.Signature (SolCallPrototype)

-- | Parse a whole sequence: calls separated by @;@.
parseFuzzSequence :: String -> Maybe [SolCallPrototype]
parseFuzzSequence s = mapM (parseFuzzCall . trim) (splitOn ";" s)

-- | Parse one call, leaving @?@ arguments open.
parseFuzzCall :: String -> Maybe SolCallPrototype
parseFuzzCall s = do
  let (fname, rest) = break (== '(') s
  args <- mapM parseFuzzArg . argList =<< delimited '(' ')' rest
  pure (pack fname, args)

-- | Parse one argument of a call, @?@ meaning "left for the fuzzer".
parseFuzzArg :: String -> Maybe (Maybe AbiValue)
parseFuzzArg s
  | trim s == "?" = Just Nothing
  | otherwise = Just <$> parseArg s

-- | Parse a concrete argument, either an array or a primitive.
parseArg :: String -> Maybe AbiValue
parseArg s
  | "[" `isPrefixOf` s' = parseArray s'
  | otherwise = parsePrimitive s'
  where s' = trim s

-- | Parse a bracketed list of primitives into a dynamic array. Every element
-- has to come out the same type, since the array needs one.
parseArray :: String -> Maybe AbiValue
parseArray s = do
  vals <- mapM parsePrimitive . argList =<< delimited '[' ']' (trim s)
  case vals of
    -- Nothing in an empty array says what it holds, so it gets the type the
    -- rest of this parser defaults to.
    [] -> Just $ AbiArrayDynamic (AbiUIntType 256) V.empty
    (v:_) | all ((== abiValueType v) . abiValueType) vals ->
      Just $ AbiArrayDynamic (abiValueType v) (V.fromList vals)
    _ -> Nothing

-- | Parse a single value: a boolean, a @0x@-prefixed address, or a @uint256@.
parsePrimitive :: String -> Maybe AbiValue
parsePrimitive s = case map toLower s' of
  "true" -> Just (AbiBool True)
  "false" -> Just (AbiBool False)
  _ | "0x" `isPrefixOf` s' -> AbiAddress . fromIntegral <$> integer
    | otherwise -> AbiUInt 256 . fromIntegral <$> integer
  where
    s' = trim s
    integer = readMaybe s' :: Maybe Integer

-- | Split a comma-separated argument list, keeping bracketed groups whole.
splitArgs :: String -> [String]
splitArgs = go (0 :: Int) ""
  where
    go _ current [] = [reverse current]
    go depth current (c:cs) = case c of
      '[' -> go (depth + 1) (c:current) cs
      ']' -> go (depth - 1) (c:current) cs
      ',' | depth == 0 -> reverse current : go depth "" cs
      _ -> go depth (c:current) cs

-- | The contents of a delimited group, or 'Nothing' if that is not what this
-- is. Checking the closing delimiter is what keeps @foo(1@ from parsing as a
-- call with no arguments.
delimited :: Char -> Char -> String -> Maybe String
delimited open close s
  | [open] `isPrefixOf` s && [close] `isSuffixOf` s = Just $ drop 1 (init s)
  | otherwise = Nothing

-- | The arguments in a delimited list. An empty one has no arguments rather
-- than one empty argument, which is what 'splitArgs' would make of it.
argList :: String -> [String]
argList s
  | all isSpace s = []
  | otherwise = splitArgs s

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
