module Mastermind.Program (start) where

import BasicPrelude
import CommandLine qualified
import Data.Text qualified as Text
import Mastermind.GameState (GameState)
import Mastermind.GameState qualified

-- TODO 5: Write CLInput instances and CLOutput instances for Input and Output types inside of the Mastermind.GameState module
-- See the Hangman.GameState module for an example.
-- Refactor the "start" method to use CommandLine.runGenericGame instead of CommandLine.runGame.

start :: IO ()
start = do
  putStrLn "Starting Mastermind. Type in a list of colors to guess."
  CommandLine.runGame Mastermind.GameState.Starting gameLoop

gameLoop :: (GameState, Text) -> (GameState, Text)
gameLoop (state, input) =
  let (output, newState) = Mastermind.GameState.handleInput (Mastermind.GameState.parseInput $ Text.unpack input) state
   in (newState, formatOutput output)

formatOutput :: Mastermind.GameState.Output -> Text
formatOutput = \case
  Mastermind.GameState.SavedColorsToGuess -> "Start guessing the colors!"
  Mastermind.GameState.GuessCorrect -> "Great job, you win! Enter a list of colors to start a new game."
  Mastermind.GameState.GuessIncorrect -> "Guess incorrect! Try again."