{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Printer.RevStateImpl where

import Control.Applicative
import Control.Lens
import Control.Monad.Codensity
import Control.Monad.Except
import Control.Monad.Fix
import qualified Control.Monad.RevState as Rev
import Control.Monad.State
import PErr
import Parser
import Pos
import Stack
import Utils

{-
This implementation uses a reverse state for the position. There is no need to track what updates are necessary or calculate them manually; the reverse state does all this implicitly.
But this is hard to reason with in the proofs because it uses mfix.
-}

newtype CP a = CP {runCP :: Codensity (Rev.StateT Pos (StateT (String, IndentStack) (Either PErr))) a}
  deriving (Functor, Applicative, Monad)

runPrinter :: CP a -> (String, IndentStack, Pos) -> Either PErr (a, (String, IndentStack, Pos))
runPrinter p (str, st, pos) = runPrinter' p pure (str, st, pos)

runPrinter' :: CP a -> (a -> Rev.StateT Pos (StateT (String, IndentStack) (Either PErr)) b) -> (String, IndentStack, Pos) -> Either PErr (b, (String, IndentStack, Pos))
runPrinter' p cc (str, st, pos) = shuffle <$> runStateT (Rev.runStateT (runCodensity (runCP p) cc) pos) (str, st)
  where
    shuffle ((b, pos'), (str', st')) = (b, (str', st', pos'))

-- The following instances are shared between implementations (with perhaps slightly differing implementations):

instance MonadState (String, IndentStack) CP where
  state = CP . lift . Rev.liftStateT . state

instance MonadFail CP where
  fail = throwError . Error

instance Alternative CP where
  empty = fail "empty"
  p <|> q = catchError p (const q)

instance MonadError PErr CP where
  throwError = CP . lift . Rev.liftStateT . throwError
  catchError p k = CP $ Codensity $ \cc -> Rev.StateT $ \pos ->
    catchError
      (Rev.runStateT (runCodensity (runCP p) cc) pos)
      (\e -> Rev.runStateT (runCodensity (runCP (k e)) cc) pos)

-- The following instances are unique to this implementation:

instance Rev.MonadRevState Pos CP where
  state = CP . lift . Rev.state

instance MonadFix CP where
  mfix f = CP $ lift (mfix (\x -> runCodensity (runCP (f x)) pure))

printChar :: Char -> CP ()
printChar c = do
  modifying _1 (c :)
  Rev.modify (`updatePosChar` c)

-- | Add a string in front of the stream.
printString :: String -> CP ()
printString = foldl (\acc c -> printChar c *> acc) (pure ())

printLookahead :: P a -> CP ()
printLookahead p = CP $ Codensity $ \cc -> Rev.StateT $ \pos -> StateT $ \(str, st) ->
  case runPrinter' (pure ()) cc (str, st, pos) of
    Left e -> Left e
    Right (b, (str', st', pos')) ->
      let pState = mkPState str st pos'
       in case runParser p pState of
            Left e -> Left e
            Right _ -> Right ((b, pos'), (str', st'))

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

printNotFollowedBy :: P a -> CP ()
printNotFollowedBy p = CP $ Codensity $ \cc -> Rev.StateT $ \pos -> StateT $ \(str, st) ->
  case runPrinter' (pure ()) cc (str, st, pos) of
    Left e -> Left e
    Right (b, (str', st', pos')) ->
      let pState = mkPState str st pos'
       in case runParser p pState of
            Left _ -> Right ((b, pos'), (str', st'))
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
