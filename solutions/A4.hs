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
_X_WIN_ :: Board
_X_WIN_ = [ [X, O, O]
          , [O, X, O]
          , [O, O, X]
          ]

_O_WIN_ :: Board
_O_WIN_ = [ [O, X, O]
          , [X, X, O]
          , [X, O, O]
          ]
   
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
isWinningLine :: Player -> Line -> Bool
isWinningLine _ [] = False
isWinningLine E _  = False
isWinningLine p l  = foldr (\s b -> p == s && b) True l

-- Q#08
hasWon :: Player -> Board -> Bool
hasWon _ []               = False
hasWon E _                = False
hasWon player board  = foldr (\line bTorF -> isWinningLine player line || bTorF) False (getAllLines board)

-- Q#09
getGameState :: Board -> GameState
getGameState [] = GameIsInProgress
getGameState b
  | hasWon X b  = XWonTheGame
  | hasWon O b  = OWonTheGame
  | isTied b    = GameIsTied
  | otherwise   = GameIsInProgress

playMove :: Player -> Board -> Move -> (GameState, Board)
playMove _ [] _   = (GameIsInProgress, [])
playMove E b _    = (GameIsInProgress, b)
playMove p b m    = (gmSt, brd)
  where
    brd  = putSquare p b m
    gmSt = getGameState brd


-- Q#10
prependRowIndices :: [String] -> [String]
prependRowIndices []  = []
prependRowIndices s   = zipWith (:) _UPPER_ALPHA_ s


-- Q#11
formatBoard :: Board -> String
formatBoard b = unlines $ _HEADER_ : prependRowIndices (formatRows b)
