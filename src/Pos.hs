{-# LANGUAGE TemplateHaskell #-}

module Pos
  ( Pos,
    lineNr,
    colNr,
    mkPos,
    defaultPos,
    onNewline,
    onNotNewline,
    updatePosChar,
    updatePosString,
    PosS,
    mkPosS,
    runPosS,
  )
where

import Control.Lens
import Data.Monoid

data Pos = Pos {_lineNr :: Int, _colNr :: Int} deriving (Eq, Ord)

makeLenses ''Pos

instance Show Pos where
  show p = "Ln " ++ (p ^. lineNr & show) ++ ", Col " ++ (p ^. colNr & show)

mkPos :: (Int, Int) -> Pos
mkPos (l, c) = Pos {_lineNr = l, _colNr = c}

defaultPos :: Pos
defaultPos = mkPos (1, 1)

onNewline :: Pos -> Pos
onNewline (Pos l _) = Pos (l + 1) 1

onNotNewline :: Pos -> Pos
onNotNewline (Pos l c) = Pos l (c + 1)

updatePosChar :: Pos -> Char -> Pos
updatePosChar p c
  | c == '\n' = onNewline p
  | otherwise = onNotNewline p

updatePosString :: Pos -> String -> Pos
updatePosString = foldl' updatePosChar

type PosS = Endo Pos

mkPosS :: (Pos -> Pos) -> PosS
mkPosS = Endo

runPosS :: PosS -> (Pos -> Pos)
runPosS = appEndo

-- >>> updatePosString (mkPos (1, 1)) "ab c\nd ef"
-- Ln 2, Col 5
