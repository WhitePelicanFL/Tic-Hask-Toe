module A1 where

import Data.Char --(toUpper)
import Control.Applicative (Alternative(empty))
import Data.Bits (Bits(xor))
import Data.List
--import System.Console.ANSI (getTerminalSize)
import Data.Maybe


-- *** Assignment 1-1 *** --

-- Q#01
_SIZE_ :: Int
_SIZE_ = 3

-- Q#02
_DISPLAY_LOGO_ :: Bool
_DISPLAY_LOGO_ = True
d = _DISPLAY_LOGO_

-- Q#03
convertRowIndex :: Char -> Int
convertRowIndex char = fromEnum (toUpper char) - 65

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
data Square = X | O | E deriving (Show, Eq)

-- not so sure about this one
type Move   = (Int, Int)

-- Q#07
data GameState = GameIsInProgress | GameIsTied | OWonTheGame | XWonTheGame deriving (Show, Eq)

-- Q#08
-- Some Synonyms
type Player = Square
type Row    = [Square]
type Line   = [Square]
type Board  = [Row]

-- Q#09
getFirstPlayerIF :: Bool -> Player
getFirstPlayerIF firstPlayerIf = if firstPlayerIf then X else O

getFirstPlayerGD :: Bool -> Player
getFirstPlayerGD firstPlayerGd
  | firstPlayerGd = X
  | otherwise     = O

-- Q#10
showGameState :: GameState -> String
showGameState gameStateString = case gameStateString of
  GameIsInProgress -> "Game is in progress..."
  GameIsTied       -> "Game is tied..."
  OWonTheGame      -> "Player O won the game."
  XWonTheGame      -> "Player X won the game."
  
-- Q#11
isSingleton :: [a] -> Bool
isSingleton []       = False -- 0 element
isSingleton (x : []) = True  -- 1 element
isSingleton _        = False -- more than 1 element

switchPlayer :: Player -> Player
switchPlayer x
 | x == X    = O
 | x == O    = X
 | otherwise = E

isValidPlayer :: Player -> Bool
isValidPlayer x
 | x == X || x == O = True
 | otherwise        = False

-- Q#12
showSquare :: Square -> String
showSquare x = case x of
 X -> "X"
 O -> "O"
 _ -> "_"


third :: [a] -> a
third (a : (b : (c : ds))) = c

thrd :: [a] -> a
thrd xs = head (tail (tail (xs)))

isNull :: [a] -> Bool
isNull (x : xs) = False
isNull []       = True



_ODDS_ :: [Int]
_ODDS_ = [1, 3 .. 10]

-- _PRIMES_         = sieve [2 ..]
-- nPrimes n        = take n _PRIMES_
-- primesUpTo limit = takeWhile (<= limit) _PRIMES_

