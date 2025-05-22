module Stack where

import Prelude hiding (abs)

type Stack a = [a]

mkStack :: [a] -> Stack a
mkStack = id

defaultStack :: Stack Int
defaultStack = mkStack []

type IndentStack = Stack Int
