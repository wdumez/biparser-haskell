{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# HLINT ignore "Use $>" #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Use void" #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Parser where

import Control.Applicative
import Control.Lens hiding (op)
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import PErr
import Pos
import Stack
import Utils

mkPState :: String -> IndentStack -> Pos -> (String, IndentStack, Pos)
mkPState a b c = (a, b, c)

defaultPState :: (String, IndentStack, Pos)
defaultPState = mkPState "" [] defaultPos

lString :: Lens' (String, IndentStack, Pos) String
lString = _1

lStack :: Lens' (String, IndentStack, Pos) IndentStack
lStack = _2

lPos :: Lens' (String, IndentStack, Pos) Pos
lPos = _3

newtype P a = P {runP :: StateT (String, IndentStack, Pos) (Either PErr) a}
  deriving (Functor, Applicative, Monad, MonadState (String, IndentStack, Pos), MonadError PErr)

instance MonadFail P where
  fail = err

-- Manual definition so as not to collect errors, which is awful for performance.
instance Alternative P where
  empty = fail "empty"
  p <|> q = catchError p (const q)

runParser :: P a -> (String, IndentStack, Pos) -> Either PErr (a, (String, IndentStack, Pos))
runParser = runStateT . runP

-- For quick testing of the parser on an input stream.
runParser' :: P a -> String -> Either PErr (a, (String, IndentStack, Pos))
runParser' p str = runParser p (str, [], mkPos (1, 1))

-- Primitive combinators

infixl 1 <?>

(<?>) :: P a -> String -> P a
-- p <?> s = do
--   pos <- readPos
--   let msg = show pos ++ ": " ++ s
--   withError (PErr.Context msg) p
p <?> _ = p

readPos :: P Pos
readPos = use lPos

readIndent :: P Int
readIndent = do
  s <- use lStack
  case s of
    [] -> fail "cannot read empty stack"
    (c : _) -> pure c

pushIndent :: Int -> P ()
pushIndent x = lStack %= (x :)

popIndent :: P Int
popIndent = do
  s <- use lStack
  case s of
    [] -> fail "cannot pop empty stack"
    (c : cs) -> do
      lStack .= cs
      pure c

satisfy :: (Char -> Bool) -> P Char
satisfy f =
  do
    s <- use lString
    case s of
      [] -> fail "empty stream"
      (c : cs)
        | f c -> do
            lString .= cs
            lPos %= (`updatePosChar` c)
            pure c
        | otherwise -> fail $ "not satisfied on character: " ++ show c

-- | Consume the input stream while the predicate @f@ holds, and return all consumed characters.
munch :: (Char -> Bool) -> P String
munch f =
  do
    str <- use lString
    let (munched, rest) = span f str
    lString .= rest
    lPos %= (`updatePosString` munched)
    pure munched

-- | @lookahead p@ executes @p@, and if *successful* it pretends like nothing happened.
-- Otherwise, it throws a lookahead error.
lookahead :: P a -> P ()
lookahead p = do
  s <- get
  case runParser p s of
    Left _ -> fail "lookahead failed"
    Right _ -> pure ()

-- | @notFollowedBy p@ executes @p@, and if *unsuccessful* it pretends like nothing happened.
-- Otherwise, it throws a notFollowedBy error.
notFollowedBy :: P a -> P ()
notFollowedBy p = lookahead (expectError p)

expectError :: P a -> P ()
expectError p = do
  s <- get
  case runParser p s of
    Left _ -> pure ()
    Right _ -> fail "unexpected success"

-- Non-primitive combinators

-- | Unconditionally consume the next character from the input, or fail if the stream is empty.
item :: P Char
item = satisfy (const True)

-- | Munch while satisfying @munchF@, and subsequently succeed only if @validF@ holds on the parsed result.
munch' :: (Char -> Bool) -> (String -> Bool) -> P String
munch' munchF validF =
  do
    s <- munch munchF
    guardError (validF s) "invalid result"
    pure s

-- @validate f@ executes predicate @f@ on the next character in the stream.
-- If True, nothing happens. Otherwise, fail.
validate :: (Char -> Bool) -> P ()
validate f = lookahead (satisfy f)

-- | Ensure that the end-of-file is reached, and fail otherwise.
eof :: P ()
eof = notFollowedBy item

char :: Char -> P Char
char c = satisfy (== c)

string :: String -> P String
string = foldr (liftA2 (:) . char) (pure [])

option :: a -> P a -> P a
option x p = p <|> pure x

opt :: (Monoid a) => P a -> P a
opt = option mempty

choice :: [P a] -> P a
choice = foldr (<|>) (fail "no successes")

oneOf :: [Char] -> P Char
oneOf = choice . map char

oneOf' :: [String] -> P String
oneOf' = choice . map string

between :: (Applicative f) => f a -> f b -> f c -> f c
between open close p = open *> p <* close

infixr 5 <<>>

-- | Lifted @(<>)@.
(<<>>) :: (Applicative f, Semigroup a) => f a -> f a -> f a
(<<>>) = liftA2 (<>)

sepBy0, sepBy1, sepBy2 :: P a -> P b -> P [a]
sepBy0 p sep = sepBy1 p sep <|> pure []
sepBy1 p sep = liftA2 (:) p ((sep *> sepBy1 p sep) <|> pure [])
sepBy2 p sep = liftA2 (:) (p <* sep) (sepBy1 p sep)

manyWhitespace, someWhitespace, manySpaces, someSpaces :: P String
manyWhitespace = many (satisfy isWhitespace)
someWhitespace = some (satisfy isWhitespace)
manySpaces = many (satisfy isSpace)
someSpaces = some (satisfy isSpace)

type Indent = Int

-- | Output the current column number in the file.
column :: P Indent
column = view colNr <$> readPos <?> "column"

-- | Parse whitespace which enters a new line, and output the indentation level.
parseNewLine :: P Indent
parseNewLine =
  do
    ws <- someWhitespace
    guardError ('\n' `elem` ws) "parseNewLine: no '\\n' found"
    let (_, req) = splitWhitespace ws
    pure (length req + 1)
    <?> "parseNewLine"

-- >>> runParser' parseNewLine "  \n  \n   "
-- Right (4,("",[],Ln 3, Col 4))

-- | Split whitespace into optional and important whitespace. The important part contains all trailing spaces.
splitWhitespace :: String -> (String, String)
splitWhitespace ws =
  let (xs, ys) = span (== ' ') (reverse ws)
   in (reverse ys, xs)

-- >>> splitWhitespace "  \n  "
-- >>> splitWhitespace "  "
-- >>> splitWhitespace " \n  \n "
-- ("  \n","  ")
-- ("","  ")
-- (" \n  \n"," ")

-- >>> let p = parseNewLine
-- >>> runParser' p ""
-- >>> runParser' p " "
-- >>> runParser' p "\n"
-- >>> runParser' p " \n  \n "
-- >>> runParser' p " \n a \n  "
-- >>> runParser' p "("
-- >>> runParser' p "-"
-- Left empty stream
-- Left parseNewLine: no '\n' found
-- Right (1,("",[],Ln 2, Col 1))
-- Right (2,("",[],Ln 3, Col 2))
-- Right (2,("a \n  ",[],Ln 2, Col 2))
-- Left not satisfied on character: '('
-- Left not satisfied on character: '-'

-- | Parse whitespace which stays on the same line, and output the indentation level.
parseSameLine :: P Indent
parseSameLine = manySpaces *> notFollowedBy (satisfy (== '\n')) *> column <?> "parseSameLine"

-- >>> let p = parseSameLine
-- >>> runParser' p ""
-- >>> runParser' p " "
-- >>> runParser' p "\n"
-- >>> runParser' p " \n  \n "
-- >>> runParser' p " \n a \n  "
-- >>> runParser' p "("
-- >>> runParser' p "-"
-- Right (1,("",[],Ln 1, Col 1))
-- Right (2,("",[],Ln 1, Col 2))
-- Left lookahead failed
-- Left lookahead failed
-- Left lookahead failed
-- Right (1,("(",[],Ln 1, Col 1))
-- Right (1,("-",[],Ln 1, Col 1))

-- | Parse whitespace and output the indentation level.
parseIndent :: P Indent
parseIndent = parseSameLine <|> parseNewLine <?> "parseIndent"

-- >>> let p = parseIndent
-- >>> runParser' p ""
-- >>> runParser' p " "
-- >>> runParser' p "\n"
-- >>> runParser' p " \n  \n "
-- >>> runParser' p " \n a \n  "
-- >>> runParser' p "("
-- >>> runParser' p "-"
-- Right (1,("",[],Ln 1, Col 1))
-- Right (2,("",[],Ln 1, Col 2))
-- Right (1,("",[],Ln 2, Col 1))
-- Right (2,("",[],Ln 3, Col 2))
-- Right (2,("a \n  ",[],Ln 2, Col 2))
-- Right (1,("(",[],Ln 1, Col 1))
-- Right (1,("-",[],Ln 1, Col 1))

-- | Parse whitespace which separates items in a block.
itemIndent :: P ()
itemIndent =
  do
    j <- parseNewLine
    i <- readIndent
    guardError (i == j) $ "itemIndent: parsed level (" ++ show j ++ ") /= block level (" ++ show i ++ ")"
    <?> "itemIndent"

-- | Parse a block of one or more items.
block1 :: P a -> P [a]
block1 p =
  do
    i <- parseIndent
    pushIndent i
    xs <- sepBy1 p itemIndent
    void popIndent
    pure xs
    <?> "block1"

-- | Parse a block of zero or more items.
block0 :: P a -> P [a]
block0 p = block1 p <|> pure [] <?> "block0"

-- >>> let p = block1 (string "hey")
-- >>> runParser' p ""
-- >>> runParser' p "hoi"
-- >>> runParser' p "hey"
-- >>> runParser' p "hey  \nhey"
-- >>> runParser' p "hey\n hey"
-- >>> runParser' p " hey\nhey"
-- >>> runParser' p " hey\n hey"
-- >>> runParser' p "hey\nhoi"
-- >>> runParser' p "hey hey hey"
-- Left empty stream
-- Left not satisfied on character: 'o'
-- Right (["hey"],("",[],Ln 1, Col 4))
-- Right (["hey","hey"],("",[],Ln 2, Col 4))
-- Right (["hey"],("\n hey",[],Ln 1, Col 4))
-- Right (["hey"],("\nhey",[],Ln 1, Col 5))
-- Right (["hey","hey"],("",[],Ln 2, Col 5))
-- Right (["hey"],("\nhoi",[],Ln 1, Col 4))
-- Right (["hey"],(" hey hey",[],Ln 1, Col 4))

-- >>> let p = block1 (string "hello world")
-- >>> runParser' p "hello world"
-- >>> runParser' p "hello    world"
-- >>> runParser' p " hello world \n hello world"
-- Right (["hello world"],("",[],Ln 1, Col 12))
-- Left not satisfied on character: ' '
-- Right (["hello world","hello world"],("",[],Ln 2, Col 13))

-- | Parse whitespace which enters a new line during a line-fold.
foldNewLine :: P ()
foldNewLine =
  do
    j <- parseNewLine
    i <- readIndent
    guardError (j > i) $
      "foldNewLine: parsed level (" ++ show j ++ ") <= block level (" ++ show i ++ ")"
    <?> "foldNewLine"

-- >>> let p = pushIndent 2 *> foldNewLine
-- >>> runParser' p ""
-- >>> runParser' p " "
-- >>> runParser' p "\n"
-- >>> runParser' p "\n  "
-- >>> runParser' p " \n  \n "
-- >>> runParser' p " \n  \n  "
-- >>> runParser' p " \n a \n  "
-- Left empty stream
-- Left parseNewLine: no '\n' found
-- Left foldNewLine: parsed level (1) <= block level (2)
-- Right ((),("",[2],Ln 2, Col 3))
-- Left foldNewLine: parsed level (2) <= block level (2)
-- Right ((),("",[2],Ln 3, Col 3))
-- Left foldNewLine: parsed level (2) <= block level (2)

-- | Parse whitespace during a line-fold.
lineFold :: P ()
lineFold = foldNewLine <|> void manySpaces <?> "lineFold"

-- >>> let p = pushIndent 2 *> lineFold
-- >>> runParser' p ""
-- >>> runParser' p "\n"
-- >>> runParser' p "  \n"
-- >>> runParser' p "\n("
-- >>> runParser' p "\n ("
-- >>> runParser' p "\n  ("
-- >>> runParser' p "  \n  "
-- >>> runParser' p "  \n   "
-- >>> runParser' p "  \n  \n   \n   "
-- >>> runParser' p "     "
-- >>> runParser' p "("
-- >>> runParser' p " ("
-- Right ((),("",[2],Ln 1, Col 1))
-- Right ((),("\n",[2],Ln 1, Col 1))
-- Right ((),("\n",[2],Ln 1, Col 3))
-- Right ((),("\n(",[2],Ln 1, Col 1))
-- Right ((),("\n (",[2],Ln 1, Col 1))
-- Right ((),("(",[2],Ln 2, Col 3))
-- Right ((),("",[2],Ln 2, Col 3))
-- Right ((),("",[2],Ln 2, Col 4))
-- Right ((),("",[2],Ln 4, Col 4))
-- Right ((),("",[2],Ln 1, Col 6))
-- Right ((),("(",[2],Ln 1, Col 1))
-- Right ((),("(",[2],Ln 1, Col 2))
