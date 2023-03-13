module A3 where

import A1
import A2

import Data.List (transpose)
import GHC.Base (magicDict)
import Foreign (toBool)


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
showInts []       = []
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
isColEmpty r c  = torf where
  (xs, ys)      = splitAt c r
  torf
    | c < 0     = False
    | null ys   = False
    | head ys  == X || head ys == O = False
    | otherwise = True

-- Q#05
dropFrstCol :: Board -> Board
dropFrstCol []       = []
dropFrstCol (x : xs) = tail x : dropFrstCol xs   
   

dropMidlCol :: Board -> Board
dropMidlCol = undefined

dropLastCol :: Board -> Board
dropLastCol []       = []
dropLastCol (x : xs) = init x : dropLastCol xs   


-- Q#06
getDiagTLBR :: Board -> Line
getDiagTLBR []       = []
getDiagTLBR (x : xs) = head x : getDiagTLBR (dropFrstCol xs)

getDiagTRBL :: Board -> Line
getDiagTRBL []       = []
getDiagTRBL (x : xs) = last x : getDiagTRBL (dropLastCol xs)

getAllLines :: Board -> [Line]
getAllLines []  = []
getAllLines brd = brd ++ transpose brd ++ [getDiagTLBR brd] ++ [getDiagTRBL brd] 

-- *** Assignment 3-2 ***

-- Q#07
putSquare :: Player -> Board -> Move -> Board
putSquare _ [] _       = [] -- check that Board  is not empty
putSquare p b m
    | isMoveInBounds m = concat [x, [replaceSquareInRow p (snd m) y], ys]
    | otherwise        = b
    where
      (x, y : ys) = splitAt (fst m) b

-- Q#08
prependRowIndices :: [String] -> [String]
prependRowIndices []  = []
prependRowIndices s   = workerF (indexRowStrings s)
  where
    workerF :: [(Char, String)] -> [String]
    workerF []              = []
    workerF ((c, str) : sl) = (c : str) : workerF sl
  
-- Q#09
isWinningLine :: Player -> Line -> Bool
isWinningLine _ [] = False
isWinningLine p l  = workerF l
  where
    workerF :: Line -> Bool
    workerF []         = True
    workerF (lx : lxs) = (lx == p) && workerF lxs

-- Q#10
isValidMove :: Board -> Move -> Bool
isValidMove [] _  = False
isValidMove b m
  | isMoveInBounds m = isColEmpty y (snd m)
  | otherwise        = False
  where
    (x, y : ys)  = splitAt (fst m) b

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


root :: Float -> Float
root x = sqrt x

root' :: Float -> Maybe Float
root' x = if x < 0 then Nothing else Just (sqrt x)

divid :: Int -> Int -> Maybe Int
divid x 0 = Nothing
divid x y = Just (div x y)
