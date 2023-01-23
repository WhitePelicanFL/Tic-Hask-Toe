module A1 where

import Data.Char (toUpper)

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

-- *** Assignment 1-2 *** --

-- Q#06
data Square


-- Q#07
data GameState


-- Q#08






-- Q#09

getFirstPlayer = undefined


getFirstPlayer_ = undefined

-- Q#10

showGameState gs = undefined

-- Q#11

switchPlayer = undefined


-- Q#12

showSquare = undefined