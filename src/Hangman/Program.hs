module Hangman.Program
  ( start,
  )
where

import BasicPrelude
import CommandLine qualified
import Data.Text qualified as Text
import Hangman.GameState (GameState)
import Hangman.GameState qualified

start :: IO ()
start = do
  putStrLn "Starting hangman. Type in a word to guess."
  CommandLine.runGame Hangman.GameState.Starting gameLoop

gameLoop :: (GameState, Text) -> (GameState, Text)
gameLoop (state, input) =
  let (output, newState) = Hangman.GameState.handleInput (Text.unpack input) state
   in (newState, formatOutput output)

formatOutput :: Hangman.GameState.Output -> Text
formatOutput = \case
  Hangman.GameState.EmptyGuessGiven -> ""
  Hangman.GameState.SavedWordToGuess -> "Start guessing the word! You can either enter a single letter or a whole word"
  Hangman.GameState.GuessIncorrect -> "Guess incorrect! Try again."
  Hangman.GameState.RevealedChar hint -> Text.pack hint
  Hangman.GameState.GuessCorrect -> "Guess correct! You can start a new game by typing in a new word to guess."