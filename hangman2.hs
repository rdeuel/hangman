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

data Result = GoodGuess | BadGuess | AlreadyGuessed | Won | Lost deriving Show
{--
newGuessFn :: Char -> Hangman -> (Result, Hangman)
newGuessFn guess state@(Hangman ans gs bad)
   | guess `elem` gs = (AlreadyGuessed, state)
   | wordComplete (guess : gs) ans = (Won, state {guesses = guess : gs})
   | guess `elem` ans = (GoodGuess, state {guesses = guess : gs})
   | bad < 6 = (BadGuess, state {guesses = guess : gs, numBad = bad + 1})
   | otherwise = (Lost, state {guesses = guess : gs})
   where 
      wordComplete guesses = foldr (\c r -> (c `elem` guesses) && r) True
--}
newGuessFn :: Char -> Hangman -> IO (Result, Hangman)
newGuessFn guess state@(Hangman ans gs bad)
   | guess `elem` gs = return (AlreadyGuessed, state)
   | wordComplete (guess : gs) ans = return (Won, state {guesses = guess : gs})
   | guess `elem` ans = return (GoodGuess, state {guesses = guess : gs})
   | bad < 6 = return (BadGuess, state {guesses = guess : gs, numBad = bad + 1})
   | otherwise = return (Lost, state {guesses = guess : gs})
   where 
      wordComplete guesses = foldr (\c r -> (c `elem` guesses) && r) True
--newtype MyState  = State (Hangman -> (Result, Hangman))
--newGuess :: State (Hangman -> (Result, Hangman))
--newGuess = State newGuessFn

askForGuess :: IO Char
askForGuess = do
	putStrLn "Enter a guess:"
	liftM head getLine

print :: Hangman -> IO ()
print (Hangman answer guessed numGuesses) = do
   let check c = if c `elem` guessed then c else '*' 
   putStrLn $ "progress:                          " ++ map check answer
   putStrLn $ "letters guessed so far :           " ++ guessed
   putStrLn $ "you lose when you spell 'HANGMAN': " 
      ++ take numGuesses "HANGMAN "

displayFn :: Hangman -> IO (String, Hangman)
displayFn state@(Hangman answer guessed numGuesses) =
   let check c = if c `elem` guessed then c else '*'
       outString = "progress:                          " ++ map check answer
            ++   "\nletters guessed so far :           " ++ guessed
            ++   "\nyou lose when you spell 'HANGMAN': " ++ take numGuesses "HANGMAN "
   in return (outString, state)

main = Hangman.print s0 >> askForGuess

--type HangmanSt = State Hangman

-- Create a state action from a Char
--newGuess :: Char -> State Hangman Result
--newGuess c = State (newGuessFn c)
newGuess :: Char -> StateT Hangman IO Result
newGuess c = StateT (newGuessFn c)

-- An action to create a display string from the state
display :: StateT Hangman IO String
display = StateT displayFn

bogus :: Result -> State Hangman Int
bogus = undefined 

--getGuess :: Char
--getGuess = let IO c = askForGuess in c

s0 = Hangman "elephant" "" 0
act = newGuess 'e' >> newGuess 'l' >> newGuess 'p' >> newGuess 'z' >> display

act1 = lift askForGuess >>= newGuess >> display >>= putStrLnT

runAct1 = runStateT act1 s0

{--
runActS0 = runState act s0 -- returns (new state, final result)
evalActS0 = evalState act s0 -- returns the final result
execActS0 = execState act s0 -- returns the new state
--}
runTActS0 = runStateT act s0 -- returns IO (new state, final result)
evalTActS0 = evalStateT act s0 -- returns the IO final result
execTActS0 = execStateT act s0 -- returns the IO new state

newState :: StateT Hangman IO Int
newState = undefined

--simHangman :: [Char] -> [
--newGuess guess = \guess -> State (newGuessFn guess)

putStrLnT msg = lift $ putStrLn msg :: StateT Hangman IO ()

