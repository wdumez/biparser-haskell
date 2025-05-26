module Main where

import Biparser
import qualified Biparser.Haskell as BH
import Control.Monad
import Data.Time.Clock
import Language.Haskell.AST
import Parser
import qualified Parser.Haskell as PH
import Pos
import Printer
import System.Directory.Internal.Prelude (getArgs)
import Prelude hiding (print)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> badInput
    (arg : _) ->
      if arg == "long"
        then mapM_ longTest (take 5 (mulRange 1 10))
        else
          if arg == "nested"
            then mapM_ nestedTest (take 15 [1 ..])
            else
              if arg == "printLong"
                then mapM_ printLongTest (take 5 (mulRange 1 10))
                else
                  if arg == "printNested"
                    then mapM_ printNestedTest (take 5 (mulRange 1 10))
                    else badInput
  where
    badInput = error "stress test expected argument: \"long\" or \"nested\" or \"printLong\" or \"printNested\""

mulRange :: (Num a) => a -> a -> [a]
mulRange start mul = let x = start * mul in x : mulRange x mul

-- >>> take 10 (mulRange 1 10)
-- [10,100,1000,10000,100000,1000000,10000000,100000000,1000000000,10000000000]

bench :: a -> IO ()
bench r = do
  start <- getCurrentTime
  seq r $ do
    end <- getCurrentTime
    let diff = diffUTCTime end start
    putStrLn ("Finished in: " ++ show diff)

parseStressTest :: (Int -> String) -> Int -> IO ()
parseStressTest f n = do
  let str = f n
  let testCaseBiparser = runParser (parse BH.fullModule ()) (str, [], mkPos (1, 1))
  let testCaseParser = runParser (PH.fullModule) (str, [], mkPos (1, 1))
  putStrLn ("+++ Stress test: n = " ++ show n ++ " for parser...")
  bench testCaseParser
  putStrLn ("--- Stress test: n = " ++ show n ++ " for biparser...")
  bench testCaseBiparser

printStressTest :: (Int -> HsModule) -> Int -> IO ()
printStressTest f n = do
  let ast = f n
  let testCaseBiparser = runPrinter (print BH.fullModule (ast, Nothing)) ("", [], mkPos (1, 1))
  putStrLn ("--- Stress test: n = " ++ show n ++ " for biparser...")
  bench testCaseBiparser

longTest :: Int -> IO ()
longTest = parseStressTest longInput

nestedTest :: Int -> IO ()
nestedTest = parseStressTest nestedInput

printLongTest :: Int -> IO ()
printLongTest = printStressTest printLongInput

printNestedTest :: Int -> IO ()
printNestedTest = printStressTest printNestedInput

nestedInput :: Int -> String
nestedInput n = if n >= 0 then pre ++ join (take n (repeat mid)) ++ post else ""
  where
    pre = "f = "
    mid = "let f = 1 in "
    post = "1"

longInput :: Int -> String
longInput n
  | n <= 0 = ""
  | otherwise = "f 1 = 1\n" ++ (longInput (n - 1))

printNestedInput :: Int -> HsModule
printNestedInput n = HsModule (Module "") Nothing [] [HsPatBind f (HsUnguardedRhs (e n)) []]
  where
    e m
      | m <= 0 = HsLit (HsInteger "1")
      | otherwise = HsLet [d] (e (m - 1))
    d = HsPatBind f (HsUnguardedRhs (e 0)) []
    f = HsPVar (HsIdent "f")

-- >>> printNestedInput 1
-- HsModule (Module "") Nothing [] [HsPatBind (HsPVar (HsIdent "f")) (HsUnguardedRhs (HsLet [HsPatBind (HsPVar (HsIdent "f")) (HsUnguardedRhs (HsLit (HsInteger "1"))) []] (HsLit (HsInteger "1")))) []]

-- >>> runParser (parse BH.fullModule ()) (longInput 1, [], mkPos (1, 1))

-- HsModule (Module "") Nothing [] [HsTypeSig [HsIdent "f"] (HsQualType [] (HsTyFun (HsTyCon (Qual (Module "") (HsIdent "Int"))) (HsTyCon (Qual (Module "") (HsIdent "Int"))))),HsFunBind (HsIdent "f") [HsPLit (HsInteger "1")] (HsUnguardedRhs (HsLit (HsInteger "1"))) []]

-- HsModule (Module "") Nothing [] [HsPatBind (HsPVar (HsIdent "f")) (HsUnguardedRhs (HsLet [HsPatBind (HsPVar (HsIdent "f")) (HsUnguardedRhs (HsLit (HsInteger "1"))) []] (HsLit (HsInteger "1")))) []]

-- >>> printLongInput 1
-- HsModule (Module "") Nothing [] [HsTypeSig [HsIdent "f"] (HsQualType [] (HsTyFun (HsTyCon (Qual (Module "") (HsIdent "Int"))) (HsTyCon (Qual (Module "") (HsIdent "Int"))))),HsFunBind (HsIdent "f") [HsPLit (HsInteger "1")] (HsUnguardedRhs (HsLit (HsInteger "1"))) []]

printLongInput :: Int -> HsModule
printLongInput n = HsModule (Module "") Nothing [] (decls n)
  where
    decls m
      | m <= 0 = []
      | otherwise = fd : decls (m - 1)
    f = HsIdent "f"
    fd = HsFunBind f [HsPLit (HsInteger "1")] (HsUnguardedRhs (HsLit (HsInteger "1"))) []

-- >>> nestedInput 3
-- "f = let f = 1 in let f = 1 in let f = 1 in 1"

-- >>> longInput 3
-- "f :: Int -> Int\nf 1 = 1\nf :: Int -> Int\nf 1 = 1\nf :: Int -> Int\nf 1 = 1\n"

stressTest :: String -> Bool
stressTest str =
  let p = BH.fullModule
      x = ()
      st = []
      pos = mkPos (1, 1)
   in case runParser (parse p x) (str, st, pos) of
        Left _ -> False
        Right _ -> True
