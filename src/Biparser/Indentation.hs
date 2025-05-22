{-# OPTIONS_GHC -Wno-unused-imports #-}

module Biparser.Indentation where

import Biparser.Combinators
import Biparser.Internal
import Parser (splitWhitespace)
import Pos
import Utils
import Prelude hiding (print)

type Indent = Int

type NrSpaces = Int

type Whitespace = String

type CIndent = Either NrSpaces Whitespace

type CBlock c = ((CIndent, [(Maybe c, Maybe Whitespace)]), NrSpaces)

type CLineFold = Either (Either NrSpaces (Whitespace, NrSpaces)) Whitespace

-- | @withIndent p@ runs @p@ such that @p@'s input and output contains the indentation level of the current block in the second component.
-- This means @p@ can read the current indentation level during its computation.
withIndent :: Biparser c (x, Indent) (y, Indent) -> Biparser c x y
withIndent p =
  iso unitR unitR'
    >#>> second readIndent
    >#>> p
    >>#> second unreadIndent
    >>#> iso unitR' unitR

-- | @withNonEmptyStack p@ is @p@, but only if the indentation stack is not empty. Otherwise, it fails.
ifNonEmptyStack :: Biparser c x y -> Biparser c x y
ifNonEmptyStack p = (readIndent >#>> unreadIndent) #*>> p

-- >>> testParse (withIndent undefined) () ""
-- >>> testPrint (withIndent undefined) ((), Nothing)
-- Left readIndent.parse: cannot read empty stack
-- Left unreadIndent.print: cannot read empty stack

-- | Convert between absolute and relative indentation.
-- - Absolute: the column number (starts at 1).
-- - Relative: the number of extra spaces compared to the level before.
--
-- If there is no previous indentation level, then the value remains unchanged.
abs2rel :: Biparser () Indent NrSpaces
abs2rel = withIndent (iso f g) <+> (iso (\x -> x - 1) (+ 1))
  where
    f (c, i) = (c - i, i)
    g (c, i) = (c + i, i)

-- >>> let p = abs2rel
-- >>> parseBiparser p 7 ("", [3], mkPos (1,1))
-- >>> testParse p 7 ""
-- >>> testParse p 1 ""
-- >>> printBiparser p (4, Nothing) ("", [3], mkPos (1,1))
-- >>> printBiparser p (4, Nothing) ("", [], mkPos (1,1))
-- >>> printBiparser p (2, Nothing) ("", [], mkPos (1,1))
-- Right ((4,()),("",[3],Ln 1, Col 1))
-- Right ((6,()),("",[],Ln 1, Col 1))
-- Right ((0,()),("",[],Ln 1, Col 1))
-- Right (7,("",[3],Ln 1, Col 1))
-- Right (5,("",[],Ln 1, Col 1))
-- Right (3,("",[],Ln 1, Col 1))

-- | Remove the top level of the indentation stack and discard it into the complement as a relative value.
-- E.g., if the stack is [7, 4], then the new stack will be [4], and the complement will contain 3.
-- When printing, the indentation stack will receive a new value which is the old value increased by the relative amount.
-- If no amount is given, then a default value will be used:
-- - if the stack is not empty, then default to @defaultIndent@ spaces
-- - if the stack is empty, then default to 0 spaces (because this is the top-level indent)
decreaseIndent :: Biparser NrSpaces () ()
decreaseIndent =
  popIndent
    >#>> abs2rel
    >#>> discardRelative

-- | Discard a relative indentation into the complement. When printing, a default of @defaultIndent@ is used, except when the stack is empty; in that case, @0@ is used as default instead because it is a top-level indent.
-- The implementation seems strange at first because the round-trip properties depend on the stack.
-- The essence is this: a branch mismatch is impossible because the left biparser always succeeds in both parse and print when the stack is not empty, and always fails in both parse and print when the stack is empty. Thus:
-- - nonempty stack: left biparser in both parse and print
-- - empty stack: right biparser in both parse and print
discardRelative :: Biparser NrSpaces NrSpaces ()
discardRelative =
  ifNonEmptyStack (align (positive defaultIndent) $ discard defaultIndent)
    <+> (align (positive 0) $ discard 0)

-- >>> parseBiparser decreaseIndent () ("", [7, 4], defaultPos)
-- Right (((),3),("",[4],Ln 1, Col 1))

-- >>> printBiparser decreaseIndent ((), Just 3) ("", [4], defaultPos)
-- Right ((),("",[7,4],Ln 1, Col 1))

-- >>> printBiparser decreaseIndent ((), Nothing) ("", [4], defaultPos)
-- Right ((),("",[6,4],Ln 1, Col 1))

-- >>> parseBiparser decreaseIndent () ("", [], defaultPos)
-- >>> printBiparser decreaseIndent ((), Just 3) ("", [], defaultPos)
-- >>> printBiparser decreaseIndent ((), Nothing) ("", [], defaultPos)
-- Left popIndent.parse: cannot pop empty stack
-- Right ((),("",[4],Ln 1, Col 1))
-- Right ((),("",[1],Ln 1, Col 1))

-- | Parse a single whitespace character.
oneWhitespace :: Biparser () () Char
oneWhitespace = satisfy (isWhitespace)

-- Various combinators for parsing whitespace or spaces.
manyWhitespace, someWhitespace, manySpaces, someSpaces :: Biparser () () Whitespace
manyWhitespace = many'_ oneWhitespace
someWhitespace = some'_ oneWhitespace
manySpaces = many'_ (satisfy isSpace)
someSpaces = some'_ (satisfy isSpace)

-- | Parse whitespace which starts a new-line indented block, and output the indentation.
-- AlignConsistent is satsified because the optional whitespace never contains anything other than whitespace.
parseNewLine :: Biparser Whitespace () Indent
parseNewLine =
  align
    (containsNewLine `after` allWhitespace)
    $ someWhitespace
      >#>> validate ('\n' `elem`)
      >#>> iso splitWhitespace (uncurry (++))
      >#>> first (discard "\n")
      >>#> iso unitL' unitL
      >>#> iso spaces2indent indent2spaces
      <?> "parseNewLine"

-- >>> let p = parseNewLine
-- >>> testParse p () ""
-- >>> testParse p () " "
-- >>> testParse p () "\n"
-- >>> testParse p () " \n  \n "
-- >>> testParse p () " \n a \n  "
-- >>> testParse p () "("
-- >>> testParse p () "-"
-- Left unexpected eof
-- Left validate: invalid value
-- Right ((1,"\n"),("",[],Ln 2, Col 1))
-- Right ((2," \n  \n"),("",[],Ln 3, Col 2))
-- Right ((2," \n"),("a \n  ",[],Ln 2, Col 2))
-- Left validate: invalid value
-- Left validate: invalid value

-- >>> let p = parseNewLine
-- >>> testPrint p (2, Nothing)
-- >>> testPrint p (2, Just "hello")
-- >>> testPrint p (2, Just "")
-- >>> testPrint p (2, Just "  ")
-- >>> testPrint p (2, Just "\n")
-- >>> testPrint p (2, Just " \n")
-- >>> testPrint p (2, Just "\n   \n \n")
-- >>> printBiparser p (2, Nothing) ("(", [], (mkPos (1,1)))
-- >>> printBiparser p (2, Nothing) ("-", [], (mkPos (1,1)))
-- Right ((),("\n ",[],Ln 2, Col 2))
-- Right ((),("\n ",[],Ln 2, Col 2))
-- Right ((),("\n ",[],Ln 2, Col 2))
-- Right ((),("\n ",[],Ln 2, Col 2))
-- Right ((),("\n ",[],Ln 2, Col 2))
-- Right ((),(" \n ",[],Ln 2, Col 2))
-- Right ((),("\n   \n \n ",[],Ln 4, Col 2))
-- Right ((),("\n (",[],Ln 2, Col 2))
-- Right ((),("\n -",[],Ln 2, Col 2))

-- | Parse whitespace which stays on the same line during line-folding. The number of spaces is stored in the complement, and defaults to 1 during printing if the given number is less than zero.
-- This means zero spaces is allowed, e.g. @if(x)then(y)else(z)@ is valid, with line-folding consuming no spaces anywhere.
-- But the default (given @Nothing@) is set to one space because that is always a good fallback (whereas zero spaces may or may not be valid).
-- AlignConsistent is satisfied because a successful parse can never result in a negative number of spaces.
foldSameLine :: Biparser NrSpaces () ()
foldSameLine =
  align (positive 1) $
    manySpaces
      <<*# notFollowedBy_ (parse (char '\n'))
      >#>> iso length (`replicate` ' ')
      >#>> discard 1
      <?> "foldSameLine"

-- >>> let p = foldSameLine
-- >>> testParse p () ""
-- >>> testParse p () " "
-- >>> testParse p () "\n"
-- >>> testParse p () " \n  \n "
-- >>> testParse p () " \n a \n  "
-- >>> testParse p () "("
-- >>> testParse p () "-"
-- Right (((),0),("",[],Ln 1, Col 1))
-- Right (((),1),("",[],Ln 1, Col 2))
-- Left notFollowedBy.parse: unexpected success
-- Left notFollowedBy.parse: unexpected success
-- Left notFollowedBy.parse: unexpected success
-- Right (((),0),("(",[],Ln 1, Col 1))
-- Right (((),0),("-",[],Ln 1, Col 1))

-- >>> let p = foldSameLine
-- >>> testPrint p ((), Nothing)
-- >>> testPrint p ((), Just (-5))
-- >>> testPrint p ((), Just 0)
-- >>> testPrint p ((), Just 1)
-- >>> testPrint p ((), Just 2)
-- >>> printBiparser p ((), Nothing) ("(", [], mkPos (1,1))
-- >>> printBiparser p ((), Nothing) ("-", [], mkPos (1,1))
-- >>> printBiparser p ((), Just 0) ("(", [], mkPos (1,1))
-- >>> printBiparser p ((), Just 0) ("-", [], mkPos (1,1))
-- Right ((),(" ",[],Ln 1, Col 2))
-- Right ((),(" ",[],Ln 1, Col 2))
-- Right ((),("",[],Ln 1, Col 1))
-- Right ((),(" ",[],Ln 1, Col 2))
-- Right ((),("  ",[],Ln 1, Col 3))
-- Right ((),(" (",[],Ln 1, Col 2))
-- Right ((),(" -",[],Ln 1, Col 2))
-- Right ((),("(",[],Ln 1, Col 1))
-- Right ((),("-",[],Ln 1, Col 1))

-- | Parse whitespace which starts a same-line block, and output the indentation level. Defaults to 1 space during printing if the complement is missing.
-- One space is a good fallback option, e.g.: @do(m)@ works with 0 spaces, but @do m@ does not, while @do (m)@ and @do m@ both work with one space.
-- But sometimes, there is no value for which it would work. This occurs when the future print prints more characters on this line than were originally parsed.
-- That means the number of spaces should be negative in order to align with the other items of the block, but that is of course impossible.
-- In that case, @parseSameLine@ will fail.
parseSameLine :: Biparser NrSpaces () Indent
parseSameLine =
  align (positive 1) $
    manySpaces
      <<*# notFollowedBy_ (parse (satisfy (== '\n')))
      >#>> iso length (`replicate` ' ')
      >#>> discard 1
      >>#> column
      <?> "parseSameLine"

-- >>> let p = parseSameLine
-- >>> testParse p () ""
-- >>> testParse p () " "
-- >>> testParse p () "\n"
-- >>> testParse p () " \n  \n "
-- >>> testParse p () " \n a \n  "
-- >>> testParse p () "("
-- >>> testParse p () "-"
-- Right ((1,0),("",[],Ln 1, Col 1))
-- Right ((2,1),("",[],Ln 1, Col 2))
-- Left notFollowedBy.parse: unexpected success
-- Left notFollowedBy.parse: unexpected success
-- Left notFollowedBy.parse: unexpected success
-- Right ((1,0),("(",[],Ln 1, Col 1))
-- Right ((1,0),("-",[],Ln 1, Col 1))

-- >>> let p = parseSameLine
-- >>> testPrint p (2, Nothing)
-- >>> testPrint p (2, Just (-5))
-- >>> testPrint p (2, Just 0)
-- >>> testPrint p (2, Just 1)
-- >>> testPrint p (2, Just 2)
-- >>> printBiparser p (2, Nothing) ("(", [], mkPos (1,1))
-- >>> printBiparser p (2, Nothing) ("-", [], mkPos (1,1))
-- >>> printBiparser p (1, Nothing) ("(", [], mkPos (1,1))
-- >>> printBiparser p (1, Nothing) ("-", [], mkPos (1,1))
-- >>> printBiparser p (1, Just 0) ("(", [], mkPos (1,1))
-- >>> printBiparser p (1, Just 0) ("-", [], mkPos (1,1))
-- Right ((),(" ",[],Ln 1, Col 2))
-- Right ((),(" ",[],Ln 1, Col 2))
-- Left input column (2) /= actual column (1)
-- Right ((),(" ",[],Ln 1, Col 2))
-- Left input column (2) /= actual column (3)
-- Right ((),(" (",[],Ln 1, Col 2))
-- Right ((),(" -",[],Ln 1, Col 2))
-- Left input column (1) /= actual column (2)
-- Left input column (1) /= actual column (2)
-- Right ((),("(",[],Ln 1, Col 1))
-- Right ((),("-",[],Ln 1, Col 1))

-- | Parse whitespace which starts an indented block and output the indentation level.
parseIndent :: Biparser CIndent () Indent
parseIndent = parseSameLine </> parseNewLine <?> "parseIndent"

-- >>> let p = parseIndent
-- >>> testParse p () ""
-- >>> testParse p () " "
-- >>> testParse p () "\n"
-- >>> testParse p () " \n  \n "
-- >>> testParse p () " \n a \n  "
-- >>> testParse p () "("
-- >>> testParse p () "-"
-- Right ((1,Left 0),("",[],Ln 1, Col 1))
-- Right ((2,Left 1),("",[],Ln 1, Col 2))
-- Right ((1,Right "\n"),("",[],Ln 2, Col 1))
-- Right ((2,Right " \n  \n"),("",[],Ln 3, Col 2))
-- Right ((2,Right " \n"),("a \n  ",[],Ln 2, Col 2))
-- Right ((1,Left 0),("(",[],Ln 1, Col 1))
-- Right ((1,Left 0),("-",[],Ln 1, Col 1))

-- >>> let p = parseIndent
-- >>> testPrint p (3, Nothing)
-- >>> testPrint p (3, Just $ Left 1)
-- >>> testPrint p (3, Just $ Left 2)
-- >>> testPrint p (3, Just $ Left 3)
-- >>> testPrint p (3, Just $ Right "\n")
-- >>> testPrint p (3, Just $ Right " \n")
-- >>> testPrint p (3, Just $ Right "\n   \n \n")
-- >>> printBiparser p (2, Nothing) ("(", [], mkPos (1,1))
-- >>> printBiparser p (3, Nothing) ("-", [], mkPos (1,1))
-- Right ((),("\n  ",[],Ln 2, Col 3))
-- Right ((),("\n  ",[],Ln 2, Col 3))
-- Right ((),("  ",[],Ln 1, Col 3))
-- Right ((),("\n  ",[],Ln 2, Col 3))
-- Right ((),("\n  ",[],Ln 2, Col 3))
-- Right ((),(" \n  ",[],Ln 2, Col 3))
-- Right ((),("\n   \n \n  ",[],Ln 4, Col 3))
-- Right ((),(" (",[],Ln 1, Col 2))
-- Right ((),("\n  -",[],Ln 2, Col 3))

-- | Parse whitespace which separates items in a block.
itemIndent :: Biparser Whitespace ((), Indent) ((), Indent)
itemIndent =
  first parseNewLine
    >>#> (validate (uncurry (==)) <?> "indentation mismatch")
    >>#> iso (\(_, i) -> ((), i)) (\((), i) -> (i, i))
    <?> "itemIndent"

-- >>> let p = itemIndent
-- >>> parseBiparser p ((), 2) ("", [2], mkPos (1,1))
-- >>> parseBiparser p ((), 2) (" ", [2], mkPos (1,1))
-- >>> parseBiparser p ((), 2) ("\n", [2], mkPos (1,1))
-- >>> parseBiparser p ((), 2) (" \n  \n ", [2], mkPos (1,1))
-- >>> parseBiparser p ((), 2) (" \n a \n  ", [2], mkPos (1,1))
-- >>> parseBiparser p ((), 2) ("(", [2], mkPos (1,1))
-- Left unexpected eof
-- Left validate: invalid value
-- Left validate: invalid value
-- Right ((((),2)," \n  \n"),("",[2],Ln 3, Col 2))
-- Right ((((),2)," \n"),("a \n  ",[2],Ln 2, Col 2))
-- Left validate: invalid value

-- >>> let p = itemIndent
-- >>> testPrint p (((), 1), Nothing)
-- Right (((),1),("\n",[],Ln 2, Col 1))

-- | Parse an indented block of one or more items.
block1 :: Biparser c () x -> Biparser (CBlock c) () [x]
block1 p =
  parseIndent
    >>#> pushIndent
    >>#> iso unitL unitL'
    >>> first (sepBy1 p (withIndent itemIndent))
    >>> second decreaseIndent
    >>#> iso unitR' unitR
    <?> "block1"

-- >>> parseBiparser (block1 (char 'a')) () ("  a\n  a", [2], mkPos (1, 1))
-- Right (([(),()],((Left 2,[(Just (),Just "\n"),(Just (),Nothing)]),1)),("",[2],Ln 2, Col 4))

-- >>> printBiparser (block1 (char 'a')) ([(),()], Just ((Left 3, [(Just (),Just "\n"),(Just (),Nothing)]), 3)) ("\n a",[],mkPos (1,1))
-- Right ((),("   a\n   a\n a",[],Ln 2, Col 5))

-- | Parse an indented block of zero or more items.
block0 :: Biparser c () x -> Biparser (CBlock c) () [x]
block0 p = block1 p <+> mapC to from (constant [] null)
  where
    to :: () -> CBlock c
    to = const ((Left 0, []), 0)
    from :: CBlock c -> ()
    from = const ()

-- >>> let p = block1 (string "hey")
-- >>> testParse p () ""
-- >>> parseBiparser p () ("", [1], mkPos (1,1))
-- >>> testParse p () "hey"
-- >>> testParse p () "hey\n"
-- >>> parseBiparser p () ("hey", [1], mkPos (1,1))
-- >>> parseBiparser p () (" hey", [1], mkPos (1,1))
-- >>> parseBiparser p () ("  hey", [1], mkPos (1,1))
-- >>> testParse p () "hey\nhey"
-- >>> parseBiparser p () ("hey\nhey", [1], mkPos (1,1))
-- >>> parseBiparser p () (" hey\n hey", [1], mkPos (1,1))
-- >>> parseBiparser p () (" hey\n  hey", [1], mkPos (1,1))
-- Left unexpected eof
-- Left unexpected eof
-- Right (([()],((Left 0,[(Just (),Nothing)]),0)),("",[],Ln 1, Col 4))
-- Right (([()],((Left 0,[(Just (),Nothing)]),0)),("\n",[],Ln 1, Col 4))
-- Right (([()],((Left 0,[(Just (),Nothing)]),0)),("",[1],Ln 1, Col 4))
-- Right (([()],((Left 1,[(Just (),Nothing)]),1)),("",[1],Ln 1, Col 5))
-- Right (([()],((Left 2,[(Just (),Nothing)]),2)),("",[1],Ln 1, Col 6))
-- Right (([(),()],((Left 0,[(Just (),Just "\n"),(Just (),Nothing)]),0)),("",[],Ln 2, Col 4))
-- Right (([(),()],((Left 0,[(Just (),Just "\n"),(Just (),Nothing)]),0)),("",[1],Ln 2, Col 4))
-- Right (([(),()],((Left 1,[(Just (),Just "\n"),(Just (),Nothing)]),1)),("",[1],Ln 2, Col 5))
-- Right (([()],((Left 1,[(Just (),Nothing)]),1)),("\n  hey",[1],Ln 1, Col 5))

-- >>> let p = block1 (string "a" *>> block1 (string "b"))
-- >>> testParse p () ""
-- >>> testParse p () "c"
-- Left unexpected eof
-- Left known.parse: unexpected value

-- | Parse whitespace which enters a new line during a line-fold.
foldNewLine :: Biparser (Whitespace, NrSpaces) ((), Indent) ((), Indent)
foldNewLine =
  first parseNewLine
    >>#> validate (uncurry (>))
    -- We compare the new indentation with the current indentation level to find the number of optional spaces.
    -- e.g., the new column is 7; but the block's indent is 4. Then there are 7 - 4 - 1 == 2 optional spaces.
    -- The other way around: we have 2 optional spaces, then the new column is 2 + 4 + 1 == 7.
    >>#> iso (\(j, i) -> (j - i - 1, i)) (\(n, i) -> (n + i + 1, i))
    >>> first (align (positive 0) $ discard 0)
    <?> "foldNewLine"

-- | Parse whitespace during a line-fold.
lineFold :: Biparser CLineFold () ()
lineFold =
  foldSameLine
    </> withIndent foldNewLine
    </> (align allSpaces $ manySpaces >#>> discard "")

-- >>> testParse lineFold () ""
-- >>> testParse lineFold () "\n  where"
-- >>> testParse lineFold () ""
-- >>> testParse lineFold () ""
-- Right (((),Left (Left 0)),("",[],Ln 1, Col 1))
-- Right (((),Right ""),("\n  where",[],Ln 1, Col 1))
-- Right (((),Left (Left 0)),("",[],Ln 1, Col 1))
-- Right (((),Left (Left 0)),("",[],Ln 1, Col 1))

-- >>> testPrint lineFold ((), Just (Right "nonsense"))
-- Right ((),("",[],Ln 1, Col 1))

-- >>> let p = lineFold
-- >>> parseBiparser p () ("", [2], mkPos (1,1))
-- >>> parseBiparser p () ("\n", [2], mkPos (1,1))
-- >>> parseBiparser p () ("  \n", [2], mkPos (1,1))
-- >>> parseBiparser p () ("\n(", [2], mkPos (1,1))
-- >>> parseBiparser p () ("\n (", [2], mkPos (1,1))
-- >>> parseBiparser p () ("\n  (", [2], mkPos (1,1))
-- >>> parseBiparser p () ("  \n  ", [2], mkPos (1,1))
-- >>> parseBiparser p () ("  \n   ", [2], mkPos (1,1))
-- >>> parseBiparser p () ("  \n  \n   \n   ", [2], mkPos (1,1))
-- >>> parseBiparser p () ("     ", [2], mkPos (1,1))
-- >>> parseBiparser p () ("(", [2], mkPos (1,1))
-- >>> parseBiparser p () (" (", [2], mkPos (1,1))
-- Right (((),Left (Left 0)),("",[2],Ln 1, Col 1))
-- Right (((),Right ""),("\n",[2],Ln 1, Col 1))
-- Right (((),Right "  "),("\n",[2],Ln 1, Col 3))
-- Right (((),Right ""),("\n(",[2],Ln 1, Col 1))
-- Right (((),Right ""),("\n (",[2],Ln 1, Col 1))
-- Right (((),Left (Right ("\n",0))),("(",[2],Ln 2, Col 3))
-- Right (((),Left (Right ("  \n",0))),("",[2],Ln 2, Col 3))
-- Right (((),Left (Right ("  \n",1))),("",[2],Ln 2, Col 4))
-- Right (((),Left (Right ("  \n  \n   \n",1))),("",[2],Ln 4, Col 4))
-- Right (((),Left (Left 5)),("",[2],Ln 1, Col 6))
-- Right (((),Left (Left 0)),("(",[2],Ln 1, Col 1))
-- Right (((),Left (Left 1)),("(",[2],Ln 1, Col 2))

-- >>> let p = lineFold
-- >>> printBiparser p ((), Nothing) ("x", [2], mkPos (1,1))
-- >>> printBiparser p ((), Nothing) ("(", [2], mkPos (1,1))
-- >>> printBiparser p ((), Just $ Left $ Left 0) ("x", [2], mkPos (1,1))
-- >>> printBiparser p ((), Just $ Left $ Left 0) ("(", [2], mkPos (1,1))
-- >>> printBiparser p ((), Just $ Left $ Left 1) ("x", [2], mkPos (1,1))
-- >>> printBiparser p ((), Just $ Left $ Left 1) ("(", [2], mkPos (1,1))
-- >>> printBiparser p ((), Just $ Left $ Right ("hello", (-1))) ("x", [2], mkPos (1,1))
-- >>> printBiparser p ((), Just $ Left $ Right ("  \n ", 2)) ("(", [2], mkPos (1,1))
-- Right ((),(" x",[2],Ln 1, Col 2))
-- Right ((),(" (",[2],Ln 1, Col 2))
-- Right ((),("x",[2],Ln 1, Col 1))
-- Right ((),("(",[2],Ln 1, Col 1))
-- Right ((),(" x",[2],Ln 1, Col 2))
-- Right ((),(" (",[2],Ln 1, Col 2))
-- Right ((),("\n  x",[2],Ln 2, Col 3))
-- Right ((),("  \n     (",[2],Ln 2, Col 6))

-- | Turn an indentation level into leading spaces.
indent2spaces :: Indent -> String
indent2spaces i = replicate (i - 1) ' '

-- | Turn spaces leading spaces into an indentation level.
spaces2indent :: String -> Indent
spaces2indent s = length s + 1

-- | The default number of spaces for indentation.
defaultIndent :: NrSpaces
defaultIndent = 2
