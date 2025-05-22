{-# HLINT ignore "Redundant lambda" #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# HLINT ignore "Use <&>" #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# HLINT ignore "Use const" #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Printer.RWSTImpl where

import Control.Applicative
import Control.Lens
import Control.Monad (ap)
import Control.Monad.Codensity
import Control.Monad.Except
import Control.Monad.RWS
import PErr
import Parser
import Pos
import Stack
import Utils

{-
This implementation explicitly tracks what updates are done to the position. For some reason this is faster than using the string to figure this out. Perhaps there is a time/memory tradeoff?
It is easier to use in the proofs than reverse state, so this is the main implementation.
-}

newtype CP a = CP {runCP :: Codensity (RWST Pos PosS (String, IndentStack) (Either PErr)) a}
  deriving (Functor, Monad)

instance Applicative CP where
  pure x = CP $ Codensity $ \cc -> RWST $ \pos (str, st) -> runRWST (cc x) pos (str, st)
  (<*>) = ap

instance MonadState (String, IndentStack) CP where
  state = CP . lift . state

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

instance MonadWriter PosS CP where
  tell = CP . lift . tell

  -- Note: we can only use limited functionality of writer. Namely, only tell works; listen cannot work because the continuation needs the written value, but that value is only produced as a result of running the continuation itself.
  listen = error "not implemented"
  pass = error "not implemented"

-- Same as runPrinter' but uses pure as the final continuation.
runPrinter :: CP a -> (String, IndentStack, Pos) -> Either PErr (a, (String, IndentStack, Pos))
runPrinter p (str, st, pos) = evalPos pos <$> runPrinter' p pure (str, st, pos)

-- Evaluate the position argument at the end of the computation.
evalPos :: Pos -> (a, (String, IndentStack, PosS)) -> (a, (String, IndentStack, Pos))
evalPos pos (a, (str, st, posS)) = (a, (str, st, runPosS posS pos))

-- Shuffle state and writer values around.
-- Note: shuffle . unshuffle == unshuffle . shuffle == id
shuffle :: (a, (b, c), d) -> (a, (b, c, d))
shuffle (a, (b, c), d) = (a, (b, c, d))

unshuffle :: (a, (b, c, d)) -> (a, (b, c), d)
unshuffle (a, (b, c, d)) = (a, (b, c), d)

-- Run the CP computation, but shuffle the values around to better match runParser.
runPrinter' :: CP a -> (a -> RWST Pos PosS (String, IndentStack) (Either PErr) b) -> (String, IndentStack, Pos) -> Either PErr (b, (String, IndentStack, PosS))
runPrinter' p cc (str, st, pos) = shuffle <$> runRWST (runCodensity (runCP p) cc) pos (str, st)

-- | Add a character in front of the stream.
-- This is too complex to use in the proofs though.
printChar :: Char -> CP ()
printChar c = do
  modifying _1 (c :)
  tell (mkPosS (`updatePosChar` c))

-- This version should be easier to prove with.
printChar' :: Char -> CP ()
printChar' c = CP $ Codensity $ \cc -> RWST $ \pos (str, st) ->
  case runRWST (cc ()) pos (c : str, st) of
    Left e -> Left e
    Right (b, (str', st'), posS) ->
      Right (b, (str', st'), mkPosS (`updatePosChar` c) <> posS)

-- | Add a string in front of the stream.
printString :: String -> CP ()
-- printString = foldl' (\acc c -> printChar c *> acc) (pure ())
printString = foldr (\c r -> r <* printChar c) (pure ())

-- Easier to prove with
printString' :: String -> CP ()
printString' s = CP $ Codensity $ \cc -> RWST $ \pos (str, st) ->
  case runRWST (cc ()) pos (s ++ str, st) of
    Left e -> Left e
    Right (b, (str', st'), posS) ->
      Right (b, (str', st'), mkPosS (`updatePosString` s) <> posS)

-- | Perform a lookahead in the printer. This function is inefficient on failure because the check is only performed at the end of the print (with delayed backtracking). If your lookahead does not use the position, use @printLookahead'@ instead. If you want to use lookahead in the parser, use @parseLookahead@ instead.
printLookahead :: P a -> CP ()
printLookahead p = CP $ Codensity $ \cc -> RWST $ \pos (str, st) ->
  case runRWST (runCodensity (runCP (pure ())) cc) pos (str, st) of
    Left e -> Left e
    Right r@(_, (_, _), posS) ->
      let pState = (str, st, runPosS posS pos)
       in case runParser p pState of
            Left e -> Left e
            Right _ -> Right r

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

-- | Like @printLookahead_@, but easier to use in proofs.
printLookahead_' :: P a -> CP ()
printLookahead_' p = CP $ Codensity $ \cc -> RWST $ \pos (str, st) ->
  let s = (str, st, undefined)
   in case runParser p s of
        Left e -> Left e
        Right _ -> runRWST (cc ()) pos (str, st)

-- | Like @parseLookahead@, but flips the error.
printNotFollowedBy :: P a -> CP ()
printNotFollowedBy p = CP $ Codensity $ \cc -> RWST $ \pos (str, st) ->
  case runRWST (runCodensity (runCP (pure ())) cc) pos (str, st) of
    Left e -> Left e
    Right r@(_, (_, _), posS) ->
      let pState = (str, st, runPosS posS pos)
       in case runParser p pState of
            Left _ -> Right r
            Right _ -> Left (Error "notFollowedBy.print: unexpected success")

-- | Like @printLookahead_@, but flips the error.
printNotFollowedBy_ :: P a -> CP ()
printNotFollowedBy_ p = do
  (str, st) <- get
  let s = mkPState str st (error "printNotFollowedBy_ cannot use position!")
  case runParser p s of
    Left _ -> pure ()
    Right _ -> fail "notFollowedBy'.print: unexpected success"

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
