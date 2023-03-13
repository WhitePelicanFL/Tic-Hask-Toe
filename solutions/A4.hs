module A4 where

import A1
import A2
import A3 hiding (
  _HEADER_,
  showSquares,
  dropFirstCol,
  dropLastCol,
  formatRows,
  isWinningLine,
  prependRowIndices
  )

-- *** Assignment 4-1 *** --

-- Q#01
_HEADER_ :: String
_HEADER_ = " " ++ formatLine (map show _RANGE_)

-- Q#02

showSquares :: Row -> [String]
showSquares xs = map showSquare xs


-- Q#03
dropFrstCol :: Board -> Board
dropFrstCol b = map tail b

-- Q#04
dropLastCol :: Board -> Board
dropLastCol b = map init b

--Q#05
formatRows :: [Row] -> [String]
formatRows xs = map (\x -> formatLine (showSquares x)) xs

map' :: (a -> b) -> [a] -> [b]
map' = undefined

-- Q#06
isWinningLine_ :: Player -> Line -> Bool
isWinningLine_ _ [] = False
isWinningLine_ E _  = False
isWinningLine_ p l  = null (filter (\s -> p /= s) l) -- the filter returns a list and null returns Bool

-- *** Assignment 4-2 *** --

-- Q#07

isWinningLine = undefined

-- Q#08

hasWon = undefined

-- Q#09

getGameState = undefined


playMove = undefined

-- Q#10

prependRowIndices = undefined

-- Q#11

formatBoard = undefined