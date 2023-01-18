module Mastermind.GameState (GameState (Starting), handleInput, Output (..), parseInput, Color (..)) where

import BasicPrelude

-- TODO 3: Implement the parseInput function. Currently it always returns an empty list of colors,
-- but it should e.g. return [Red, Blue, Green] for the String "rxbg".

-- TODO 4: A correct guess should already win you the game. However, incorrect guesses just tell you the guess
-- was incorrect without giving any hints.
-- Change the IncorrectGuess constructor to contain a Hint.
-- Choose any internal representation you like.
-- A hint should be displayed like this for example:
-- "1 correct colors, 2 correct positions"
-- A color is correct if the color occurs in the colors to guess but is not in the correct position
-- Example: Colors to guess "rgby", Guess "bgwr" -> b and r are correct colors, g is a correct position, w is neither.

data GameState = Starting | Guessing [Color]
  deriving (Eq, Show)

data Color = Red | Blue | Green | Yellow | White
  deriving (Eq, Show)

type Input = [Color]

data Output
  = SavedColorsToGuess
  | GuessCorrect
  | GuessIncorrect
  deriving (Show, Eq)

handleInput :: Input -> GameState -> (Output, GameState)
handleInput input Starting = (SavedColorsToGuess, Guessing input)
handleInput input (Guessing colorsToGuess)
  | input == colorsToGuess = (GuessCorrect, Starting)
handleInput _ state = (GuessIncorrect, state)

-- | Extract all color chars from a String
parseInput :: String -> Input
parseInput = const []

parseColor :: Char -> Maybe Color
parseColor 'r' = Just Red
parseColor 'b' = Just Blue
parseColor 'g' = Just Green
parseColor 'y' = Just Yellow
parseColor 'w' = Just White
parseColor _ = Nothing