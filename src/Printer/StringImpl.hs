{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Printer.StringImpl where

import Control.Applicative
import Control.Lens
import Control.Monad.Codensity
import Control.Monad.Except
import Control.Monad.RWS
import Control.Monad.Reader
import Control.Monad.State
import PErr
import Parser
import Pos
import Stack
import Utils

{-
This implementation does not explicitly track the position updates with a Writer.
It instead compares the string with its new value to find the required position update.
-}

newtype CP a = CP {runCP :: Codensity (ReaderT Pos (StateT (String, IndentStack) (Either PErr))) a}
  deriving (Functor, Applicative, Monad)

instance MonadState (String, IndentStack) CP where
  state = CP . lift . lift . state

instance MonadReader Pos CP where
  ask = CP $ lift $ ask
  local f m = CP $ Codensity $ \cc -> ReaderT $ \pos -> StateT $ \s ->
    runStateT (runReaderT (runCodensity (runCP m) cc) (f pos)) s

instance MonadFail CP where
  fail = throwError . Error

instance Alternative CP where
  empty = fail "empty"
  p <|> q = catchError p (const q)

instance MonadError PErr CP where
  throwError = CP . lift . throwError
  catchError p k = CP $ Codensity $ \cc ->
    catchError
      (runCodensity (runCP p) cc)
      (\e -> runCodensity (runCP (k e)) cc)

-- Same as runPrinter' but uses pure as the final continuation.
runPrinter :: CP a -> (String, IndentStack, Pos) -> Either PErr (a, (String, IndentStack, Pos))
runPrinter p (str, st, pos) = runPrinter' p pure (str, st, pos)

-- Same as runPrinter'' but adds the final position at the end.
runPrinter' :: CP a -> (a -> ReaderT Pos (StateT (String, IndentStack) (Either PErr)) b) -> (String, IndentStack, Pos) -> Either PErr (b, (String, IndentStack, Pos))
runPrinter' p cc (str, st, pos) = evalPos str pos <$> runPrinter'' p cc (str, st, pos)

-- Runs the CP computation.
runPrinter'' :: CP a -> (a -> ReaderT Pos (StateT (String, IndentStack) (Either PErr)) b) -> (String, IndentStack, Pos) -> Either PErr (b, (String, IndentStack))
runPrinter'' p cc (str, st, pos) = runStateT (runReaderT (runCodensity (runCP p) cc) pos) (str, st)

-- Evalutate the final position based on the initial string, initial position and output of runPrinter''.
evalPos :: String -> Pos -> (b, (String, IndentStack)) -> (b, (String, IndentStack, Pos))
evalPos str pos (b, (str', st')) =
  let dstr = str' `removeSuffix` str
      pos' = updatePosString pos dstr
   in (b, (str', st', pos'))

-- | Add a character in front of the stream.
-- This is too complex to use in the proofs though.
printChar :: Char -> CP ()
printChar c = modifying _1 (c :)

printChar' :: Char -> CP ()
printChar' c = CP $ Codensity $ \cc -> ReaderT $ \pos -> StateT $ \(str, st) ->
  runStateT (runReaderT (runCodensity (runCP (pure ())) cc) pos) (c : str, st)

-- | Add a string in front of the stream.
printString :: String -> CP ()
printString = foldl (\acc c -> printChar c *> acc) (pure ())

-- | Perform a lookahead in the printer. This function is inefficient on failure because the check is only performed at the end of the print (with delayed backtracking). If your lookahead does not use the position, use @printLookahead'@ instead. If you want to use lookahead in the parser, use @parseLookahead@ instead.
printLookahead :: P a -> CP ()
printLookahead p = CP $ Codensity $ \cc -> ReaderT $ \pos -> StateT $ \(str, st) ->
  case runPrinter' (pure ()) cc (str, st, pos) of
    Left e -> Left e
    Right (b, (str', st', pos')) ->
      let pState = (str, st, pos')
       in case runParser p pState of
            Left e -> Left e
            Right _ -> Right (b, (str', st'))

testCol :: Int -> P ()
testCol x = do
  c <- view colNr <$> use _3
  guardError (x == c) "testCol failed"

-- | A more performant version of @printLookahead@ which performs the check right away, but cannot use the position because of it (with a runtime error if tried anyway).
printLookahead_ :: P a -> CP ()
printLookahead_ p = do
  (str, st) <- get
  let s = (str, st, error "printLookahead_ cannot use position!")
  case runParser p s of
    Left e -> throwError e
    Right _ -> pure ()

-- | Like @parseLookahead@, but flips the error.
printNotFollowedBy :: P a -> CP ()
printNotFollowedBy p = CP $ Codensity $ \cc -> ReaderT $ \pos -> StateT $ \(str, st) ->
  case runPrinter' (pure ()) cc (str, st, pos) of
    Left e -> Left e
    Right (b, (str', st', pos')) ->
      let pState = (str, st, pos')
       in case runParser p pState of
            Left _ -> Right (b, (str', st'))
            Right _ -> Left $ Error "notFollowedBy.print: unexpected success"

-- | Like @printLookahead_@, but flips the error.
printNotFollowedBy_ :: P a -> CP ()
printNotFollowedBy_ p = do
  (str, st) <- get
  let s = mkPState str st (error "printNotFollowedBy_ cannot use position!")
  case runParser p s of
    Left _ -> pure ()
    Right _ -> fail "notFollowedBy_.print: unexpected success"

{-
Reasoning with the printer's right-to-left semantics can be quite unintuitive, especially when adding lookahead.
Below are some examples of how the printer behaves.
-}

-- Here we see that a lookahead *before* a choice *cannot* be distributed into that choice!

-- >>> let p = printLookahead (testCol 1) *> (printChar 'a' <|> pure ())
-- >>> runPrinter p defaultPState
-- Left testCol failed

-- >>> let p = (printLookahead (testCol 1) *> printChar 'a') <|> (printLookahead (testCol 1) *> pure ())
-- >>> runPrinter p defaultPState
-- Right ((),("",[],Ln 1, Col 1))

-- But here we see that a lookahead *after* a choice *can* be distributed into that choice!

-- >>> let p = (printChar 'a' <|> pure ()) *> printLookahead (testCol 1)
-- >>> runPrinter p defaultPState
-- Right ((),("a",[],Ln 1, Col 2))

-- >>> let p = (printChar 'a' *> printLookahead (testCol 1)) <|> (pure () *> printLookahead (testCol 1))
-- >>> runPrinter p defaultPState
-- Right ((),("a",[],Ln 1, Col 2))

-- >>> let p = printLookahead (testCol 1) *> printLookahead (testCol 1)
-- >>> runPrinter p defaultPState
-- Right ((),("",[],Ln 1, Col 1))

-- >>> let p = printLookahead_ (testCol 1)
-- >>> runPrinter p defaultPState
-- printLookahead_ cannot use position!

-- >>> let p = pure ()
-- >>> runPrinter p defaultPState
-- Right ((),("",[],Ln 1, Col 1))

-- >>> let p = printChar 'a' *> printChar 'b'
-- >>> runPrinter p defaultPState
-- Right ((),("ba",[],Ln 1, Col 3))

-- >>> let p = printString "hello"
-- >>> runPrinter p defaultPState
-- Right ((),("hello",[],Ln 1, Col 6))

-- >>> let p = printString "\n "
-- >>> runPrinter p defaultPState
-- Right ((),("\n ",[],Ln 2, Col 2))

-- >>> let p = printString " \n"
-- >>> runPrinter p defaultPState
-- Right ((),(" \n",[],Ln 2, Col 1))

-- >>> let p = printChar '\n' *> printChar ' '
-- >>> runPrinter p defaultPState
-- Right ((),(" \n",[],Ln 2, Col 1))

-- >>> let p = printString " " *> printString "\n"
-- >>> runPrinter p defaultPState
-- Right ((),("\n ",[],Ln 2, Col 2))

-- >>> let p = printString "hey" <|> printString "hello"
-- >>> runPrinter p defaultPState
-- Right ((),("hey",[],Ln 1, Col 4))

-- >>> let p = empty <|> printString "hello"
-- >>> runPrinter p defaultPState
-- Right ((),("hello",[],Ln 1, Col 6))

-- >>> let p = printLookahead (testCol 3) *> printChar 'b' *> printChar 'a'
-- >>> runPrinter p defaultPState
-- Right ((),("ab",[],Ln 1, Col 3))

-- >>> let p = printChar 'b' *> printLookahead (testCol 3) *> printChar 'a'
-- >>> runPrinter p defaultPState
-- Left testCol failed

-- >>> let p =  printChar 'b' *> printChar 'a' *> printLookahead (testCol 3)
-- >>> runPrinter p defaultPState
-- Left testCol failed
