{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Biparser.Internal where

import Control.Applicative ((<|>))
import Control.Lens hiding (cons, from, iso, to, uncons)
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import qualified Data.List as List
import PErr
import Parser (P (P), defaultPState, runParser)
import Pos
import Printer
import Stack (IndentStack)
import Utils
import Prelude hiding (print, read)

data Biparser c x y = Biparser
  { parse :: x -> P (y, c),
    print :: (y, Maybe c) -> CP x
  }

parseBiparser :: Biparser c x y -> x -> (String, IndentStack, Pos) -> Either PErr ((y, c), (String, IndentStack, Pos))
parseBiparser p x = runParser (parse p x)

printBiparser :: Biparser c x y -> (y, Maybe c) -> (String, IndentStack, Pos) -> Either PErr (x, (String, IndentStack, Pos))
printBiparser p ymc = runPrinter (print p ymc)

-- For quick testing.
testParse :: Biparser c x y -> x -> String -> Either PErr ((y, c), (String, IndentStack, Pos))
testParse p x str = parseBiparser p x (defaultPState & _1 .~ str)

-- For quick testing.
testPrint :: Biparser c x y -> (y, Maybe c) -> Either PErr (x, (String, IndentStack, Pos))
testPrint p ymc = printBiparser p ymc defaultPState

-- Primitive combinators

infixl 1 <?>

-- | Attach a name to the error produced by the biparser.
(<?>) :: Biparser c x y -> String -> Biparser c x y
-- (Biparser f g) <?> s = Biparser f' g'
--   where
--     f' x = do
--       pos <- use _3
--       let msg = show pos ++ ": " ++ s
--       withError (PErr.Context msg) (f x)
--     g' = withError (PErr.Context s) . g
p <?> _ = p

-- | @identity@ does nothing to its input.
identity :: Biparser () x x
identity =
  Biparser
    { parse = \x -> pure (x, ()),
      print = \(x, _) -> pure x
    }

-- | Consume an item from the input.
item :: Biparser () () Char
item =
  Biparser
    { parse = \_ -> P $ StateT $ \(str, st, pos) ->
        case str of
          [] -> Left "unexpected eof"
          (c : cs) -> Right ((c, ()), (cs, st, updatePosChar pos c)),
      print = \(c, _) -> printChar c
    }

-- | Consume characters while the predicate holds.
munch :: (Char -> Bool) -> Biparser () () String
munch f =
  Biparser
    { parse = \_ -> do
        str <- use _1
        let (munched, rest) = span f str
        _1 .= rest
        _3 %= (`updatePosString` munched)
        pure (munched, ()),
      print = \(s, _) -> do
        -- Check that the string is the result of a munch.
        unless (all f s) (fail $ "consistency check failed (string: " ++ show s ++ ")")
        -- Check that a munch would not have consumed more.
        str <- use _1
        maybe
          (pure ())
          (\c -> when (f c) (fail $ "consistency check failed (stream containted " ++ show c ++ ")"))
          (safeHead str)
        -- All checks passed, so print the string.
        printString s
    }

-- | Transform the input/output using a partial isomorphism.
partialIso :: (x -> Either PErr y) -> (y -> Either PErr x) -> Biparser () x y
partialIso to from =
  Biparser
    { parse = \x -> case to x of
        Left e -> throwError e
        Right y -> pure (y, ()),
      print = \(y, _) -> case from y of
        Left e -> throwError e
        Right x -> pure x
    }

-- | @lookahead p@ applies @p@ to the parse.
-- If it is successful nothing happens, otherwise an error is raised.
-- @p@ is also applied when printing, to maintain consistency.
lookahead :: (x -> P a) -> Biparser () x x
lookahead p =
  Biparser
    { parse = \x -> P $ StateT $ \s ->
        case runParser (p x) s of
          Left _ -> Left "lookahead.parse: did not succeed"
          Right _ -> Right ((x, ()), s),
      print = \(x, _) -> do
        Printer.printLookahead (p x)
        pure x
    }
    <?> "lookahead"

-- | Like @lookahead@, but flips the error.
notFollowedBy :: (x -> P a) -> Biparser () x x
notFollowedBy p =
  Biparser
    { parse = \x -> P $ StateT $ \s ->
        case runParser (p x) s of
          Left _ -> Right ((x, ()), s)
          Right _ -> Left "notFollowedBy.parse: unexpected success",
      print = \(x, _) -> do
        Printer.printNotFollowedBy (p x)
        pure x
    }
    <?> "notFollowedBy"

-- | Alternative to @lookahead@ which is more efficient in the print but cannot use the position there.
lookahead_ :: (x -> P a) -> Biparser () x x
lookahead_ p =
  Biparser
    { parse = parse (lookahead p),
      print = \(x, _) -> do
        Printer.printLookahead_ (p x)
        pure x
    }

-- | Alternative to @notFollowedBy@ which is more efficient in the print but cannot use the position there.
notFollowedBy_ :: (x -> P a) -> Biparser () x x
notFollowedBy_ p =
  Biparser
    { parse = parse (notFollowedBy p),
      print = \(x, _) -> do
        Printer.printNotFollowedBy_ (p x)
        pure x
    }

infixl 6 >>>

-- | @p >>> q@ applies @p@, then @q@, passing the output of @p@ as input to @q@, and collecting both complements.
(>>>) ::
  Biparser c1 x y ->
  Biparser c2 y z ->
  Biparser (c1, c2) x z
p >>> q =
  Biparser
    { parse = \x -> do
        (y, c1) <- parse p x
        (z, c2) <- parse q y
        pure (z, (c1, c2)),
      print = \(z, mc) -> do
        y <- print q (z, snd <$> mc)
        print p (y, fst <$> mc)
    }

-- | Apply an isomorphism to the complement.
-- Requires from . to == id
mapC :: (c1 -> c2) -> (c2 -> c1) -> Biparser c1 x y -> Biparser c2 x y
mapC to from p =
  Biparser
    { parse = \x -> do
        (y, c) <- parse p x
        pure (y, to c),
      print = \(y, c2) -> print p (y, from <$> c2)
    }

-- An align strategy takes a modified view and original complement as arguments, and produces an aligned complement.
-- To satisfy AlignConsistent, make sure that any complement output by a successful parse remains unchanged by the strategy.
type AlignStrategy y c = y -> c -> c

-- | @s2 after s1@ aligns first with strategy @s1@, followed by strategy @s2@, which uses the result of @s1@ as its complement.
after :: AlignStrategy y c -> AlignStrategy y c -> AlignStrategy y c
s2 `after` s1 = \y c -> s2 y (s1 y c)

-- | Positional alignment. Leaves the complement unchanged (default).
positional :: AlignStrategy y c
positional _ c = c

-- | @replace f c0@ replaces the complement @c@ with @c0@ if @f c@ is satisfied, and keeps @c@ otherwise.
replace :: (c -> Bool) -> c -> AlignStrategy y c
replace f c0 = \_ c -> if f c then c0 else c

-- | @keep f c0@ keeps the complement @c@ if @f c@ is satisfied, and replaces it with @c0@ otherwise.
keep :: (c -> Bool) -> c -> AlignStrategy y c
-- keep f c0 = \_ c -> if f c then c else c0
keep f = replace (not . f)

-- | Align the complement such that it contains only spaces.
allSpaces :: AlignStrategy y String
allSpaces = keep (all (== ' ')) ""

-- | Align the complement such that it contains only whitespace.
allWhitespace :: AlignStrategy y String
allWhitespace = keep (all isWhitespace) ""

-- | Align the complement such that it contains a newline.
containsNewLine :: AlignStrategy y String
containsNewLine = keep (any (== '\n')) "\n"

-- | Align the complement such that it contains a positive number.
positive :: Int -> AlignStrategy y Int
positive = replace (< 0)

-- | Use an alignment strategy to align the view and complement.
-- - ParsePrint requires AlignConsistent.
align :: AlignStrategy y c -> Biparser c x y -> Biparser c x y
align f p =
  Biparser
    { parse = parse p,
      print = \(y, c) -> print p (y, f y <$> c)
    }

-- | Store information about the input in the complement.
store :: (x -> c) -> Biparser c x x
store f =
  Biparser
    { parse = \x -> pure (x, f x),
      print = \(x, _) -> pure x
    }

-- | Make the complement optional by mapping it into @Left@.
relaxCl :: Biparser c1 x y -> Biparser (Either c1 c2) x y
relaxCl p =
  Biparser
    { parse = \x -> (\(y, c) -> (y, Left c)) <$> parse p x,
      print = \(y, c) -> print p (y, c >>= leftToMaybe)
    }

-- | @first p@ executes @p@ in the first component of the input/output tuples.
first :: Biparser c x y -> Biparser c (x, z) (y, z)
first p =
  Biparser
    { parse = \(x, z) -> (\(y, c) -> ((y, z), c)) <$> parse p x,
      print = \((y, z), mc) -> (,z) <$> print p (y, mc)
    }

-- | @discard x0@ discards the input into the complement.
-- When printing without complement, it uses @x0@ as output instead.
discard :: x -> Biparser x x ()
discard x0 =
  Biparser
    { parse = \x -> pure ((), x),
      print = \((), mc) -> case mc of
        Nothing -> pure x0
        Just c -> pure c
    }

infixl 5 +++

-- | Deterministic choice. The choice is part of the input/output.
(+++) :: Biparser c x1 y1 -> Biparser c x2 y2 -> Biparser c (Either x1 x2) (Either y1 y2)
p +++ q =
  Biparser
    { parse = \ex -> case ex of
        Left x1 -> (\(y1, c) -> (Left y1, c)) <$> parse p x1
        Right x2 -> (\(y2, c) -> (Right y2, c)) <$> parse q x2,
      print = \(ey, c) -> case ey of
        Left y1 -> Left <$> print p (y1, c)
        Right y2 -> Right <$> print q (y2, c)
    }

infixl 5 <+>

-- | Biased choice. The parser and printer both prefer the left branch.
-- - ParsePrint requires ParsePrintConsistent.
-- - PrintParse requires PrintParseConsistent.
(<+>) :: Biparser c x y -> Biparser c x y -> Biparser c x y
p <+> q =
  Biparser
    { parse = \x -> parse p x <|> parse q x,
      print = \ymc -> print p ymc <|> print q ymc
    }

infixl 5 </>

-- | Remembered choice. The choice is left-biased, but the complement is used to remember the parser's choice.
-- That choice is then preferred by the printer if the complement is available.
-- - PrintParse requires ParseConsistent.
(</>) :: Biparser c1 x y -> Biparser c2 x y -> Biparser (Either c1 c2) x y
p </> q =
  Biparser
    { parse = \x ->
        ((\(y, c1) -> (y, Left c1)) <$> parse p x) <|> ((\(y, c2) -> (y, Right c2)) <$> parse q x),
      print = \(y, mc) -> case mc of
        Nothing -> print p (y, Nothing) <|> print q (y, Nothing)
        Just (Left c1) -> print p (y, Just c1) <|> print q (y, Nothing)
        Just (Right c2) -> print q (y, Just c2) <|> print p (y, Nothing)
    }

-- | Push an indentation to the stack.
pushIndent :: Biparser () Int ()
pushIndent =
  Biparser
    { parse = \i -> do
        _2 %= (i :)
        pure ((), ()),
      print = \_ -> do
        s <- use _2
        case List.uncons s of
          Nothing -> fail "pushIndent.print: cannot pop empty stack"
          Just (i, xs) -> do
            _2 .= xs
            pure i
    }

-- | Pop an indentation from the stack.
popIndent :: Biparser () () Int
popIndent =
  Biparser
    { parse = \_ -> do
        s <- use _2
        case List.uncons s of
          Nothing -> fail "popIndent.parse: cannot pop empty stack"
          Just (i, xs) -> do
            _2 .= xs
            pure (i, ()),
      print = \(i, _) -> _2 %= (i :)
    }

-- | Read an indentation from the stack.
readIndent :: Biparser () () Int
readIndent =
  Biparser
    { parse = \_ -> do
        s <- use _2
        case List.uncons s of
          Nothing -> fail "readIndent.parse: cannot read empty stack"
          Just (i, _) -> pure (i, ()),
      print = \(j, _) -> do
        s <- use _2
        case List.uncons s of
          Nothing -> fail "readIndent.print: cannot read empty stack"
          Just (i, _) -> guardError (i == j) "readIndent.print: indent did not match top of stack"
    }

-- | Un-read an indentation to the stack.
unreadIndent :: Biparser () Int ()
unreadIndent =
  Biparser
    { parse = \j -> do
        s <- use _2
        case List.uncons s of
          Nothing -> fail "unreadIndent.parse: cannot read empty stack"
          Just (i, _) -> guardError (i == j) "unreadIndent.parse: indent did not match top of stack"
        pure ((), ()),
      print = \_ -> do
        s <- use _2
        case List.uncons s of
          Nothing -> fail "unreadIndent.print: cannot read empty stack"
          Just (i, _) -> pure i
    }

-- | @column@ reads the current column @c@ during the parse and outputs it.
-- During the print, it consumes the parsed column @c@ only if it equals the current column @c'@. Otherwise, it fails (consistency check).
column :: Biparser () () Int
column =
  Biparser
    { parse = \_ -> do
        c <- view colNr <$> use _3
        pure (c, ()),
      print = \(c, _) -> do
        Printer.printLookahead $ do
          c' <- view colNr <$> use _3
          guardError (c == c') $ "input column (" ++ show c ++ ") /= actual column (" ++ show c' ++ ")"
    }
    <?> "column"

-- >>> testParse column () "aa"
-- >>> testPrint column (1, Nothing)
-- >>> testPrint column (2, Nothing)
-- Right ((1,()),("aa",[],Ln 1, Col 1))
-- Right ((),("",[],Ln 1, Col 1))
-- Left input column (2) /= actual column (1)

-- | Like @column@ but easier to use in proofs.
column' :: Biparser () () Int
column' =
  Biparser
    { parse = \_ -> P $ StateT $ \(str, st, pos) -> Right ((view colNr pos, ()), (str, st, pos)),
      print = \(col, _) -> Printer.printLookahead $
        P $
          StateT $ \(str, st, pos) ->
            let col' = view colNr pos
             in if col == col'
                  then Right (col, (str, st, pos))
                  else Left (Error $ "input column (" ++ show col ++ ") /= actual column (" ++ show col' ++ ")")
    }
