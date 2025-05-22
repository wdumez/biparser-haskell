{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Utils where

import Control.Monad.Except (MonadError)
import Data.List (uncons)
import PErr (PErr, err)

-- | @guardError b e@ succeeds if @b@ is @True@, and throws an error with message @msg@ otherwise.
-- guardError :: (MonadError e m) => Bool -> e -> m ()
-- guardError b e = if b then pure () else throwError e
-- guardError :: (MonadFail f) => Bool -> String -> f ()
-- guardError b msg = if b then pure () else fail msg
guardError :: (MonadError PErr m) => Bool -> String -> m ()
guardError b msg = if b then pure () else err msg

safeHead :: [a] -> Maybe a
safeHead = fmap fst . uncons

isNewline :: Char -> Bool
isNewline = (== '\n')

isSpace :: Char -> Bool
isSpace = (== ' ')

isWhitespace :: Char -> Bool
isWhitespace c = isNewline c || isSpace c

leftToMaybe :: Either a b -> Maybe a
leftToMaybe = either Just (const Nothing)

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

swapEither :: Either a b -> Either b a
swapEither = either Right Left

maybeToLeft :: Maybe a -> Either a ()
maybeToLeft = maybe (Right ()) Left

maybeToRight :: Maybe a -> Either () a
maybeToRight = maybe (Left ()) Right

unitL :: a -> ((), a)
unitL = ((),)

unitL' :: ((), a) -> a
unitL' ((), x) = x

unitR :: a -> (a, ())
unitR = (,())

unitR' :: (a, ()) -> a
unitR' (x, ()) = x

swap :: (x, y) -> (y, x)
swap (x, y) = (y, x)

removePrefix :: (Eq a) => [a] -> [a] -> [a]
removePrefix [] _ = []
removePrefix xs [] = xs
removePrefix s@(x : xs) (y : ys)
  | x == y = removePrefix xs ys
  | otherwise = s

-- >>> removePrefix "hello" "he"
-- "llo"

removeSuffix :: (Eq a) => [a] -> [a] -> [a]
removeSuffix xs ys =
  let sx = reverse xs
      sy = reverse ys
      sx' = removePrefix sx sy
   in reverse sx'

-- >>> removeSuffix "" ""
-- >>> removeSuffix "hello" "llo"  -- "he"
-- >>> removeSuffix "" "llo"       -- ""
-- >>> removeSuffix "hello" "hey"  -- "hello"
-- >>> removeSuffix "hello" "hello"
-- ""
-- "he"
-- ""
-- "hello"
-- ""

punctuate :: a -> [a] -> [a]
punctuate _ [] = []
punctuate _ [x] = [x]
punctuate p (x : xs) = x : p : punctuate p xs

any' :: (Foldable t) => t (a -> Bool) -> a -> Bool
any' fs c = foldr (\f -> (||) (f c)) False fs

splitList :: (a -> Bool) -> [a] -> [[a]]
splitList _ [] = []
splitList p xs =
  let (pre, suf) = span p xs
   in case suf of
        [] -> [pre]
        (_ : suf') -> pre : splitList p suf'

-- >>> splitList (/= '.') "F.G.H"
-- ["F","G","H"]

startsWith :: String -> String -> Bool
startsWith [] _ = True
startsWith _ [] = False
startsWith (p : ps) (x : xs)
  | p == x = startsWith ps xs
  | otherwise = False

digitToInt :: Char -> Maybe Int
digitToInt c = case c of
  '0' -> Just 0
  '1' -> Just 1
  '2' -> Just 2
  '3' -> Just 3
  '4' -> Just 4
  '5' -> Just 5
  '6' -> Just 6
  '7' -> Just 7
  '8' -> Just 8
  '9' -> Just 9
  _ -> Nothing

intToDigit :: Int -> Maybe Char
intToDigit i = case i of
  0 -> Just '0'
  1 -> Just '1'
  2 -> Just '2'
  3 -> Just '3'
  4 -> Just '4'
  5 -> Just '5'
  6 -> Just '6'
  7 -> Just '7'
  8 -> Just '8'
  9 -> Just '9'
  _ -> Nothing

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x : xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = filter (<= x) xs
    larger = filter (> x) xs
