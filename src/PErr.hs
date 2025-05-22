{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module PErr where

import Control.Monad.Except (MonadError, throwError)
import Data.String (IsString, fromString)
import Prettyprinter

data PErr
  = Error String
  | ErrorGroup [PErr]
  | Context String PErr
  deriving (Eq)

instance IsString PErr where
  fromString = Error

instance Semigroup PErr where
  ErrorGroup e1 <> ErrorGroup e2 = ErrorGroup (e1 <> e2)
  ErrorGroup [] <> e = e
  ErrorGroup e1 <> e2 = ErrorGroup (e1 <> [e2])
  e <> ErrorGroup [] = e
  e1 <> ErrorGroup e2 = ErrorGroup ([e1] <> e2)
  e1 <> e2 = ErrorGroup [e1, e2]

instance Monoid PErr where
  mempty = ErrorGroup []

instance Pretty PErr where
  pretty (Error s) = pretty s
  pretty (ErrorGroup es) = vsep (map ((pretty "- " <>) . nest 2 . pretty) es)
  pretty (Context ctxt e) = pretty ctxt <> pretty ":" <> line <> pretty e

instance Show PErr where
  show = show . pretty

contextGroup :: String -> [PErr] -> PErr
contextGroup ctxt children = Context ctxt (ErrorGroup children)

err :: (MonadError PErr m) => String -> m a
err msg = throwError (Error msg)

-- >>> let e1 = Error "e1"
-- >>> let e2 = Error "e2"
-- >>> let e3 = Error "e3"
-- >>> let e4 = Error "e4"
-- >>> pretty $ Context "all" $ Context "a" (Context "Of e1" e1 <> e2) <> (Context "b" (e3 <> e4))
-- >>> pretty $ (ErrorGroup [ErrorGroup [e1,e2], e3, e4])
-- all:
-- - a:
--   - Of e1:
--     e1
--   - e2
-- - b:
--   - e3
--   - e4
-- - - e1
--   - e2
-- - e3
-- - e4
