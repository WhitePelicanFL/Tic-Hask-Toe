{-# Language FlexibleContexts #-}

import Control.Monad.State
import Data.Char
import Data.List
import System.IO
import Text.Printf

type Secret  = String
type Chances = Int
type Guess   = Char

data Hangman = Hangman { hWord :: [(Char, Char)], hChances :: Chances }
  deriving Show

getGuess :: IO Char
getGuess =
  printf "Enter the letter: " >>
  getLine >>= \s -> 
  let x = head s in
  case isAlpha x of
    True  -> do
      return (toUpper x)
    False -> do
      printf "Invalid character\n"
      getGuess

mkHangman :: Secret -> Hangman
mkHangman secret = Hangman (map (\c -> (toUpper c, '_')) secret) 7

procLetter :: Guess -> StateT Hangman IO ()
procLetter guess = do
  oldHangman <- get
  let word = hWord oldHangman
  case elem guess (map fst word) of
    True  -> changeWord
    False -> decChances
  where
  decChances = modify (\h -> h { hChances = hChances h - 1 })
  changeWord = modify $ \h ->
    let oldWord = hWord h
        newWord = map (\(secret, y) -> if secret == guess then (secret, guess) else (secret, y)) oldWord
    in  h { hWord = newWord }

renderGame :: StateT Hangman IO ()
renderGame = do
  hangman <- get
  let word    = hWord hangman
      chances = hChances hangman
  liftIO (putStrLn (intersperse ' ' (map snd word)))
  liftIO (printf "You have %d chances left.\n\n" chances)
  
playGame :: StateT Hangman IO ()
playGame = do
  h <- get
  case hChances h > 0 of
    False -> do
      liftIO (printf "Sorry, You lose!")
    True  -> do
      case all (/= '_') (map snd (hWord h)) of
        True  -> do
          liftIO (printf "The word was %s.\n" (map snd (hWord h)))
          liftIO (printf "Yay, You win!\n\n")
        False -> do
          renderGame
          guess <- liftIO getGuess
          procLetter guess
          playGame

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering 
  printf "Enter the secret word: "
  hSetEcho stdin False
  secret <- getLine
  hSetEcho stdin True
  printf "\n"
  runStateT playGame (mkHangman secret)
  return ()
