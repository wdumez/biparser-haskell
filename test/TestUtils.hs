module TestUtils where

import Biparser
import qualified Biparser.Haskell as BH
import Biparser.Haskell.Basic
import Distribution.TestSuite
import Parser (P, runParser)
import qualified Parser.Haskell as PH
import Pos
import Printer
import Stack
import System.Directory
import Utils
import Prelude hiding (print)
import qualified Prelude

getTestFiles :: String -> IO [String]
getTestFiles dir = map (\f -> dir ++ "/" ++ f) . qsort . filter (startsWith "test") <$> getDirectoryContents dir

mkTest :: IO Progress -> String -> TestInstance
mkTest r n =
  let it =
        TestInstance
          { run = r,
            name = n,
            tags = [],
            options = [],
            setOption = \_ _ -> Right it
          }
   in it

-- >>> getTestFiles "examples"
-- ["examples/test1","examples/test2","examples/test3","examples/test4","examples/test5","examples/test6"]

failed :: String -> IO Progress
failed = pure . Finished . Fail

progress :: String -> IO Progress -> IO Progress
progress msg next = pure (Progress msg next)

passed :: IO Progress
passed = pure (Finished Pass)

mkEq :: (Eq a, Show a) => String -> a -> a -> IO Progress
mkEq n x _x =
  if x == _x
    then passed
    else failed (n ++ ": " ++ show x ++ " =/= " ++ show _x)

-- Stops at the first fail, or else completes all with a pass.
allGood :: [IO Progress] -> IO Progress
allGood [] = pure (Finished Pass)
allGood (t : ts) = do
  t' <- t
  case t' of
    Finished Pass -> allGood ts
    Progress msg next -> putStrLn msg *> allGood (next : ts)
    -- Any other case is a fail or error:
    _ -> pure t'

parsePrint :: (Eq x, Show x) => Biparser c x y -> x -> (String, IndentStack, Pos) -> IO Progress
parsePrint p x (str, st, pos) = case runParser (parse p x) (str, st, pos) of
  Left e -> failed "Failed in ParsePrint.parse" <* Prelude.print e
  Right ((y, c), (str', st', pos')) -> progress "ParsePrint: parse succeeded" $ do
    case runPrinter (print p (y, Just c)) (str', st', pos) of
      Left e -> failed "Failed in ParsePrint.print" <* Prelude.print e
      Right (_x, (_str, _st, _pos')) ->
        progress "ParsePrint: print succeeded" $
          do
            allGood
              [ mkEq "x" x _x,
                mkEq "str" str _str,
                mkEq "st" st _st,
                mkEq "pos'" pos' _pos'
              ]
            <* (putStrLn _str)

parsePrintNoC :: (Eq x, Show x) => Biparser c x y -> x -> (String, IndentStack, Pos) -> IO Progress
parsePrintNoC p x (str, st, pos) = case runParser (parse p x) (str, st, pos) of
  Left e -> failed "Failed in ParsePrintNoC.parse" <* Prelude.print e
  Right ((y, _), (str', st', _)) -> progress "ParsePrintNoC: parse succeeded" $ do
    case runPrinter (print p (y, Nothing)) (str', st', pos) of
      Left e -> failed "Failed in ParsePrintNoC.print" <* Prelude.print e
      Right (_x, (_str, _st, _pos')) -> progress "ParsePrintNoC: print succeeded" $ do
        -- putStrLn _str -- Showing output like this?
        allGood [mkEq "x" x _x]

parsePrintParse :: (Eq x, Show x, Eq y, Eq c, Show y, Show c) => Biparser c x y -> x -> (String, IndentStack, Pos) -> IO Progress
parsePrintParse p x (str, st, pos) = case runParser (parse p x) (str, st, pos) of
  Left e -> failed "Failed in ParsePrintParse.parse1" <* Prelude.print e
  Right ((y, c), (str', st', pos')) -> progress "Parse1 succeeded in ParsePrintParse" $ do
    case runPrinter (print p (y, Just c)) (str', st', pos) of
      Left e -> failed "Failed in ParsePrintParse.print" <* Prelude.print e
      Right (_x, (_str, _st, _pos')) -> progress "Print succeeded in ParsePrintParse" $ do
        case runParser (parse p _x) (_str, _st, pos) of
          Left e -> failed "Failed in ParsePrintParse.parse2" <* Prelude.print e
          Right ((_y, _c), (_str', _st', _pos')) -> progress "Parse2 succeeded in ParsePrintParse" $ do
            allGood
              [ mkEq "y" y _y,
                mkEq "c" c _c,
                mkEq "str'" str' _str',
                mkEq "st'" st' _st',
                mkEq "pos'" pos' _pos'
              ]

parsePrintParseNoC :: (Eq x, Show x, Eq y, Eq c, Show y, Show c) => Biparser c x y -> x -> (String, IndentStack, Pos) -> IO Progress
parsePrintParseNoC p x (str, st, pos) = case runParser (parse p x) (str, st, pos) of
  Left e -> failed "Failed in ParsePrintParseNoC.parse1" <* Prelude.print e
  Right ((y, c), (str', st', _)) -> progress "Parse1 succeeded in ParsePrintParseNoC" $ do
    case runPrinter (print p (y, Just c)) (str', st', pos) of
      Left e -> failed "Failed in ParsePrintParseNoC.print" <* Prelude.print e
      Right (_x, (_str, _st, _pos')) -> progress "Print succeeded in ParsePrintParseNoC" $ do
        case runParser (parse p _x) (_str, _st, pos) of
          Left e -> failed "Failed in ParsePrintParseNoC.parse2" <* Prelude.print e
          Right ((_y, _c), (_str', _st', _pos')) -> progress "Parse2 succeeded in ParsePrintParseNoC" $ do
            -- We only require the AST to be recovered now.
            allGood [mkEq "y" y _y]

-- Either they both fail, or they both succeed with exactly the same output (except for the complement)
sameParse :: (Eq y, Show y) => P y -> Biparser c x y -> x -> (String, IndentStack, Pos) -> IO Progress
sameParse p b x s@(str, st, pos) =
  let ep = runParser p s
      eb = runParser (parse b x) (str, st, pos)
   in case (ep, eb) of
        (Left _, Left _) -> pure (Finished Pass)
        (Left _, Right _) -> pure (Finished (Fail "Parser failed but biparser succeeded"))
        (Right _, Left _) -> pure (Finished (Fail "Parser succeeded but biparser failed"))
        (Right (y1, (str1, st1, pos1)), Right ((y2, _), (str2, st2, pos2))) ->
          allGood
            [ mkEq "y" y1 y2,
              mkEq "str" str1 str2,
              mkEq "st" st1 st2,
              mkEq "pos" pos1 pos2
            ]

mkParsePrint :: String -> IO Test
mkParsePrint filename = do
  str <- readFile filename
  let p = BH.fullModule
  let x = ()
  let st = []
  let pos = mkPos (1, 1)
  let r = parsePrint p x (str, st, pos)
  let theTest = mkTest r ("ParsePrint for " ++ filename)
  pure $ Test theTest

mkParsePrintNoC :: String -> IO Test
mkParsePrintNoC filename = do
  str <- readFile filename
  let p = BH.fullModule
  let x = ()
  let st = []
  let pos = mkPos (1, 1)
  let r = parsePrintNoC p x (str, st, pos)
  let theTest = mkTest r ("ParsePrint (no complement) for " ++ filename)
  pure $ Test theTest

mkParsePrintParse :: String -> IO Test
mkParsePrintParse filename = do
  str <- readFile filename
  let p = BH.fullModule
  let x = ()
  let st = []
  let pos = mkPos (1, 1)
  let r = parsePrintParse p x (str, st, pos)
  let theTest = mkTest r ("ParsePrintParse for " ++ filename)
  pure $ Test theTest

mkParsePrintParseNoC :: String -> IO Test
mkParsePrintParseNoC filename = do
  str <- readFile filename
  let p = BH.fullModule
  let x = ()
  let st = []
  let pos = mkPos (1, 1)
  let r = parsePrintParseNoC p x (str, st, pos)
  let theTest = mkTest r ("ParsePrintParse (no complement) for " ++ filename)
  pure $ Test theTest

mkSameParse :: String -> IO Test
mkSameParse filename = do
  str <- readFile filename
  let b = BH.fullModule
  let p = PH.fullModule
  let x = ()
  let st = []
  let pos = mkPos (1, 1)
  let r = sameParse p b x (str, st, pos)
  let theTest = mkTest r ("SameParse for " ++ filename)
  pure $ Test theTest

allEdgeCases :: [Test]
allEdgeCases = map Test [mkTest edgeCase1 "edgeCase1"]

-- | A varid followed by spaces followed by a number. It is impossible to parse no spaces; so what happens if you give the printer an input with no spaces?
-- - @runParser (parse p) ("x1", [], mkPos (1,1))@ will fail because varid consumes "too much"
-- - so @runPrinter (print p) (("x", ""), "1", _)@ should also fail to satisfy PrintParse!
edgeCase1 :: IO Progress
edgeCase1 =
  let p = varid <<#*>> manySpaces <<#*>> integer
   in case runPrinter (print p ((("x", ""), "1"), Nothing)) ("", [], mkPos (1, 1)) of
        Left _ -> passed
        Right ((_), (str, _, _)) -> failed ("Got output: " ++ str)
