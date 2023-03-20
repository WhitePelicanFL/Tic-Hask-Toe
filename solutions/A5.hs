module A5 where

import A1
import A2
import A3
import A4

import System.Random.Stateful (globalStdGen, uniformM)
import Control.Monad (when)
import Text.Printf (printf)
import Text.ParserCombinators.ReadP (between)

-- *** Assignment 5-1 *** --

-- Q#01
printBoard :: Board -> IO ()
printBoard b = putStrLn $ formatBoard b

-- Q#02
_LOGO_PATH_ :: FilePath
_LOGO_PATH_ = "./assets/logo.txt"

printLogo :: IO ()
printLogo = readFile _LOGO_PATH_ >>= (\a -> putStrLn a)

-- Q#03
_RANDOM_BOOL_ :: IO Bool
_RANDOM_BOOL_ = uniformM globalStdGen

firstPlayer :: IO Player
firstPlayer = _RANDOM_BOOL_ >>= (\b -> return $ getFirstPlayerGD b)

-- Q#04
getMove :: Board -> IO Move
getMove b =
  printf "Please enter a Move string (ex. a0): " >>
  getLine >>= \moveStr ->
  let
    move    = stringToMove moveStr
    valMove = isValidMove b move
  in
    if valMove then return move else printf "Invalid Move...try again\n" >> getMove b


-- Q#05
play :: Board -> Player -> IO ()
play b p =
  when _DISPLAY_LOGO_ printLogo >>
  printBoard b >>
  printf (promptPlayer p) >>
  getMove b >>= \move ->
    let (gamestate, newboard) = playMove p b move
    in
      if gamestate == GameIsInProgress
      then play newboard $ switchPlayer p
      else 
        printBoard newboard >>
        print (showGameState gamestate)


-- *** Assignment 5-2 *** --

-- Q#07
printLogoDo :: IO ()
printLogoDo = do
  a <- readFile _LOGO_PATH_ 
  putStrLn a

-- Q#08
firstPlayerDo :: IO Player
firstPlayerDo = do
  b <- _RANDOM_BOOL_
  return $ getFirstPlayerGD b

-- Q#09
getMoveDo :: Board -> IO Move
getMoveDo b = do
  printf "Please enter a Move string (ex. a0): "
  moveStr <- getLine
  let move    = stringToMove moveStr
      valMove = isValidMove b move
  if  valMove
  then do return move
  else do
    printf "Invalid Move...try again\n"
    getMove b


-- Q#10
playDo :: Board -> Player -> IO ()
playDo b p = do
  when _DISPLAY_LOGO_ printLogo
  printBoard b
  putStrLn (promptPlayer p)
  move <- getMoveDo b
  let (gamestate, newboard) = playMove p b move
  if gamestate == GameIsInProgress
  then do playDo newboard $ switchPlayer p
  else do
    printBoard newboard
    print (showGameState gamestate)
