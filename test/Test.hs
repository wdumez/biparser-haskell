module Test (tests) where

import Distribution.TestSuite
import TestUtils

tests :: IO [Test]
tests =
  sequence
    [ test_ParsePrint,
      test_ParsePrintParse,
      test_ParsePrintNoC,
      test_ParsePrintParseNoC,
      test_SameParse,
      test_EdgeCases
    ]

test_ParsePrint :: IO Test
test_ParsePrint = do
  files <- getTestFiles "round-trip-tests"
  ts <- sequence (map mkParsePrint files)
  pure (testGroup "ParsePrint" ts)

test_ParsePrintParse :: IO Test
test_ParsePrintParse = do
  files <- getTestFiles "round-trip-tests"
  ts <- sequence (map mkParsePrintParse files)
  pure (testGroup "ParsePrintParse" ts)

test_ParsePrintNoC :: IO Test
test_ParsePrintNoC = do
  files <- getTestFiles "round-trip-tests"
  ts <- sequence (map mkParsePrintNoC files)
  pure (testGroup "ParsePrint" ts)

test_ParsePrintParseNoC :: IO Test
test_ParsePrintParseNoC = do
  files <- getTestFiles "round-trip-tests"
  ts <- sequence (map mkParsePrintParseNoC files)
  pure (testGroup "ParsePrintParse" ts)

-- | Test whether the unidirectional parser and biparser (as parser) behave the same.
test_SameParse :: IO Test
test_SameParse = do
  files <- getTestFiles "round-trip-tests"
  ts <- sequence (map mkSameParse files)
  pure (testGroup "SameParse" ts)

test_EdgeCases :: IO Test
test_EdgeCases = pure (testGroup "EdgeCases" allEdgeCases)
