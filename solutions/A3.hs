module A3 where

import A1
import A2

import Data.List (transpose)


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
showSquares xs = map showSquare xs


-- Q#03
formatRows :: [Row] -> [String]
formatRows []       = []
formatRows (r : rs) = formatLine (showSquares r) : formatRows rs

-- Q#04
isColEmpty :: Row -> Int -> Bool
isColEmpty r c = torf where
  (xs, ys) = splitAt c r
  torf
    | c < 0   = False
    | null ys = False
    | head ys == X || head ys == O = False
    | otherwise = True

-- Q#05
dropFrstCol :: Board -> Board
dropFrstCol [] = []
dropFrstCol (x : xs) = tail x : dropFrstCol xs   
   

dropMidlCol :: Board -> Board
dropMidlCol = undefined

dropLastCol :: Board -> Board
dropLastCol [] = []
dropLastCol (x : xs) = init x : dropLastCol xs   


-- Q#06
getDiagTLBR :: Board -> Line
getDiagTLBR [] = []
getDiagTLBR (x : xs) = head x : getDiagTLBR (dropFrstCol xs)

getDiagTRBL :: Board -> Line
getDiagTRBL [] = []
getDiagTRBL (x : xs) = last x : getDiagTRBL (dropLastCol xs)


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


-- Polymorphic Type
data Pair a b = MkPair a b deriving Show

x :: Pair String Int
x = MkPair "p" 25

--y :: Pair (String, Int)
--y = MkPair ("p", 25)

z :: Pair Int Char
z = MkPair 7 'a'

p2t :: Pair a b -> (a, b)
p2t (MkPair a b) = (a, b)