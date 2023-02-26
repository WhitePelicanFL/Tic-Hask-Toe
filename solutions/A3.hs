module A3 where

import A1
import A2

import Data.List (transpose)
import GHC.Base (VecElem(Int16ElemRep))


-- *** Assignment 3-1 ***


{-stringChecker :: IO ()
stringChecker = do
    putStrLn "Enter a string: "
    firstString <- getLine
    putStrLn "Enter next string: "
    secndString <- getLine
    putStrLn "Enter next string: "
    thirdString <- getLine
    let msg = concat [length firstString, " , ", lenth secndString, " , ", length thirdString]
-}
-- Q#01

showInts :: [Int] -> [String]
showInts [] = []
showInts (x : xs) = show x : showInts xs


_HEADER_ :: String
_HEADER_ = " " ++ formatLine (showInts _RANGE_)

-- Q#02
showSquares :: Row -> [String]
showSquares [] = []
showSquares xs = map show xs


-- Q#03
formatRows :: [Row] -> [String]
formatRows []      = []
formatRows [x, xs] = formatLine (showSquares x) : formatRows [xs]

-- Q#04

isColEmpty = undefined

-- Q#05

dropFirstCol = undefined


dropLastCol = undefined

-- Q#06

getDiag1 = undefined


getDiag2 = undefined


getAllLines = undefined

-- *** Assignment 3-2 ***

-- Q#07

putSquare = undefined

-- Q#08

prependRowIndices = undefined

-- Q#09

isWinningLine = undefined

-- Q#10

isValidMove = undefined