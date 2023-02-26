{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module A2 where

import A1
import Data.List (intercalate)
import Data.Bits (Bits(xor))
import GHC.Num (xorInteger)
import Data.Bool (bool)
import Control.Concurrent (yield)
import Control.Monad.ST.Lazy (strictToLazyST)
import Text.ParserCombinators.ReadP (string, between)
import Data.Char (isAlpha, toUpper)
import System.Random.Stateful (globalStdGen)

-- *** Assignment 2-1 *** --
isPrime :: Int -> Bool
isPrime z = loop 2 z where
  loop i 1 = False
  loop i 2 = True
  loop i x
    | i == x - 1 = True
    |otherwise   = if mod x i == 0 then False else loop (i + 1) x



-- Q#01
promptPlayer :: Player -> String
promptPlayer x = case x of
 E  -> "Invalid Player...try again."
 _  -> concat ["Player ", showSquare x, ", please enter a row and column position (ex. A2)"]

switchPlayer :: Player -> Player
switchPlayer x
 | x == X    = O
 | x == O    = X
 | otherwise = E

-- Q#02
_RANGE_  = [0 .. _SIZE_ - 1]

-- Q#03
_CHARS_  :: [Char]
_CHARS_   = ['0' .. '3']

isDigit :: Char -> Bool
isDigit c = c `elem` _CHARS_

readDigit :: Char -> Int
readDigit c
 | isDigit c = read [c]
 | otherwise = -1


--add x y = x + y
--add x = \y -> x + y
--add = \x -> (\y -> x + y)
sum' :: [Int] -> Int
sum' [] = 0
sum' (x : xs) = x + sum' xs

-- Q#04
_EMPTY_ROW_   = replicate _SIZE_ E
_EMPTY_BOARD_ = replicate _SIZE_ _EMPTY_ROW_

-- Q#05
--_TIED_BOARD_ :: Board
_TIED_BOARD_ =
  [
    [X, O, O]
  , [O, X, O]
  , [O, X, X]
  ]


isTied' :: Board -> Bool
isTied' []       = True
isTied' (x : xs)
 | E `elem` x    = False
 | otherwise = isTied' xs


elem' :: Eq a => a -> [a] -> Bool
elem' c []       = False
elem' c (x : xs) = c == x || elem' c xs

strUpper :: String ->String
strUpper xs = map toUpper xs

removeSymbol :: String -> String
removeSymbol [] = []
removeSymbol (x : xs)
  | isAlpha x = x : removeSymbol xs
  | otherwise = removeSymbol xs


reverse' :: [a] -> [a]
reverse' xs = loop [] xs where
  loop :: [a] -> [a] -> [a]
  loop result []       = result
  loop result (x : xs) = loop (x : result) xs

length' :: [a] -> Int
length' [] = 0
length' (x : xs) =  length' xs + 1

isPangram :: String -> Bool
isPangram s = foldl (\b a -> elem a t && b) True ['A' .. 'Z']
  where
    t = map toUpper (filter isAlpha s)

filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x : xs) = if p x then x : filter' p xs else filter' p xs

-- foldr pokemon that sums the pokemon numb from the list
pokemon :: [(String, Int)]
pokemon =  [("bul", 1), ("the", 5), ("abc", 4)]
pokemonSum :: [(String, Int)] -> Int
pokemonSum [] = 0
pokemonSum xs = foldr ((+) . snd) 0 xs

--pokemonSum (a : as) = (snd a) + pokemonSum as
--pokemonSum = foldr (\(x : xs) n -> snd x + n) 0 xs
($) :: (a -> b) -> a -> b
($) f x = f x

(%) :: (a -> a) -> a -> a
(%) f x = f (f x)
--(%) f x = f . (f x)



-- Q#06
_UPPER_ALPHA_ :: [Char]
_UPPER_ALPHA_  = ['A' ..]

indexRowStrings :: [String] -> [(Char, String)]
indexRowStrings str = case str of
  [] -> []
  _  -> zip _UPPER_ALPHA_ str
 
-- Q#07
formatLine :: [String] -> String
formatLine str = case str of
  [x, xs, xxs] -> _SEP_ ++ intercalate _SEP_ str ++ _SEP_
  _            -> "_|___|___|___|_"
  
-- *** Assignment 2-2 *** --

-- Q#08
-- THINKING I SHOULD BE USING _RANGE_ HERE AND
-- I DON'T UNDERSTAND WHY (0,0) AND (3,3) ARE BOTH VALID
isMoveInBounds :: Move -> Bool
isMoveInBounds (x, y) = (x >= 0 && y >= 0) && (x <= _SIZE_ && y <= _SIZE_)

-- Q#09
stringToMove :: String -> Move
stringToMove []      = _INVALID_MOVE_
stringToMove [_]     = _INVALID_MOVE_
stringToMove [x, y]  = if (elem x "abcdABCD" && elem y "01234") then 
                       (convertRowIndex x, readDigit y) else _INVALID_MOVE_
stringToMove _       = _INVALID_MOVE_
 
-- Q#10
{-
replaceSquareInRow :: Player -> Int -> Row -> Row
replaceSquareInRow player col row 
  | player /= X && player /= O  = row  -- invalid player
  | not (col `elem` _RANGE_)    = row  -- invalid col
  | null row                    = _EMPTY_ROW_ 
  | otherwise  go 
      (r, rs) = splitAt col row
-}
