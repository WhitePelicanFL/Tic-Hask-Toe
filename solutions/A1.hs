module A1 where

import Data.Char (toUpper)
import Control.Applicative (Alternative(empty))

-- *** Assignment 1-1 *** --

-- Q#01
_SIZE_ = 3

-- Q#02
_DISPLAY_LOGO_ :: Bool
_DISPLAY_LOGO_ = True
d = _DISPLAY_LOGO_

-- Q#03
convertRowIndex :: Char -> Int
convertRowIndex char = fromEnum (toUpper (char)) - 65

-- Q#04
_INVALID_MOVE_ :: (Int, Int)
_INVALID_MOVE_ = (-1, -1)

-- Q#05
_SEP_ :: [Char]
_SEP_ = ['_','|','_']
_SEP2_ :: String
_SEP2_ = "_|_"


-- *** Assignment 1-2 *** --

-- Q#06
data Square = X | O | Neither deriving (Show, Eq)

-- not so sure about this one
type Move   = (Int, Int)

-- Q#07
data GameState = GameIsInProgress | GameIsTied | OWonTheGame | XWonTheGame deriving (Show, Eq)

-- Q#08
-- Some Synonyms
type Player = Square
type Row    = Square
type Line   = Square
type Board  = Row

-- Q#09
getFirstPlayerIf :: Bool -> Player
getFirstPlayerIf firstPlayerIf =
  if firstPlayerIf == True then X else O

getFirstPlayerGd :: Bool -> Player
getFirstPlayerGd firstPlayerGd
  | firstPlayerGd == True  = X
  | firstPlayerGd == False = O
  | otherwise              = Neither

-- Q#10
showGameState :: GameState -> String
showGameState gameStateString = case gameStateString of
  GameIsInProgress -> "Game is in progress..."
  GameIsTied       -> "Game is currently tied..."
  OWonTheGame      -> "Player O won the game."
  XWonTheGame      -> "Player X won the game."
  
-- Q#11
switchPlayer :: Player -> Player
switchPlayer player
  | player == X = O
  | player == O = X
  | otherwise = player

-- Q#12
showSquare = undefined