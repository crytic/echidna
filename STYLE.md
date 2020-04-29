# Code Styling

Please try to adhere to the following guidelines when contributing code.

## Whitespace

The tabstop is set to two spaces. Please remove any trailing spaces from your
code. Multiple spaces are disallowed unless it is for indentation purposes.
Please put spaces in between operators, except for the preceding space before a
comma. For example,

```haskell
(1 +)
(2,)
x ^. lens
case x of
  Just x  -> x
  Nothing -> error "asdf"
```

Choosing what to align and what not to align is up to you. Generally short
oneliners look better aligned but more complex code can be unaligned.

```haskell
f (Cons1 a)       = a
f (LolWhat1337 b) = b

g (Cons1 a) =
  -- 5 lines or something
g (LolWhat1337 b) =
  -- 5 lines or something
```

## case

Pattern matches of a case should be indented two spaces.

```haskell
-- CORRECT
case x of
  Just x -> x
  Nothing -> 0
-- INCORRECT
case x of
     Just x -> x
     Nothing -> 0
```

Cases should begin on a new line

```haskell
f x =
  case x of
    -- ...
  where y =
          case x of
            -- ...
```

## if

```haskell
-- pure
if cond then
  result
else
  other_result

-- monadic
if cond then do
  x <- getX
  pure $ x + 1
else do
  y <- getY
  pure $ y - 1
```

## let/where

Prefer `where` over `let` in pure functions. Do so as well in monadic functions
when applicable.

```haskell
-- CORRECT
f x = y
  where y = x + 1
-- INCORRECT
f x = let y = x + 1 in y
```

For definitions in a `where` clause, indent up to the first definition (6
spaces) or begin a new line and indent 2 more spaces.

```haskell
f x = y
  where y = x + 1
f x = y
  where
    y = x + 1
```

## Types

ADTs with records should be written in a lens-compatible fashion and lenses
should be genereated and prisms for sum types are more or less free.

```haskell
data MyData = MyData { _record1 :: Type1
                     , _record2 :: Type2 }
  deriving (...)
makeLenses ''MyData

data MyData2 = MyData2
  { _record1 :: Type1
  , _record2 :: Type2 }
  deriving (...)
makeLenses ''MyData2

data MySum = Sum1
           | Sum2
  deriving (...)
makePrisms ''MySum

data MySum2 =
    Sum3
  | Sum4
  deriving (...)
makePrisms ''MySum2
```
