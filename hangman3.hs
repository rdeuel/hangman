module Hangman where
import System.IO
import Data.Char
import Control.Monad
import Control.Monad.State


data Hangman = Hangman {
	answer::String,
	guesses :: String,
	numBad :: Int
} deriving Show

data Result = GoodGuess | BadGuess | AlreadyGuessed | Won | Lost deriving (Show, Eq)

s0 = Hangman "elephant" "" 0
s1 = s0 {guesses = "lp", numBad = 3}

-- Evaluate a new guess
newGuessFn :: Char -> Hangman -> IO (Result, Hangman)
newGuessFn guess state@(Hangman ans gs bad)
   | guess `elem` gs = return (AlreadyGuessed, state)
   | wordComplete (guess : gs) ans = return (Won, state {guesses = guess : gs})
   | guess `elem` ans = return (GoodGuess, state {guesses = guess : gs})
   | bad < 6 = return (BadGuess, state {guesses = guess : gs, numBad = bad + 1})
   | otherwise = return (Lost, state {guesses = guess : gs, numBad = bad + 1})
   where 
      wordComplete guesses = foldr (\c r -> (c `elem` guesses) && r) True       

newGuess :: Char -> StateT Hangman IO Result
newGuess c = StateT (newGuessFn c)

-- Ask for a guess
askForGuess :: IO Char
askForGuess = do
   putStrLn ""
   putStrLn "Enter a guess:"
   liftM head getLine

-- Display the current situation
displayFn :: Hangman -> IO ((), Hangman)
displayFn state@(Hangman answer guessed numGuesses) = do
   let check c = if c `elem` guessed then c else '-' 
   putStrLn $ "progress:                          " ++ map check answer
   putStrLn $ "letters guessed so far :           " ++ guessed
   putStrLn $ "you lose when you spell 'HANGMAN': " 
      ++ take numGuesses "HANGMAN "
   return ((), state)

display :: StateT Hangman IO ()
display = StateT displayFn

-- Ask for a word - should hide this as it's typed
askForWord :: IO Hangman
askForWord = do
   putStrLn "\nEnter a word:"
   word <- getLine
   return $ Hangman word "" 0

-- Sequence all the state transitions
stateMain :: StateT Hangman IO ()
stateMain = do
   c <- lift askForGuess
   res <- newGuess c
   display
   case res of 
      Won -> lift $ putStrLn "You won!"
      Lost -> lift $ putStrLn "You lost!"
      _ -> stateMain

-- Run the whole thing
main = askForWord >>= evalStateT stateMain
