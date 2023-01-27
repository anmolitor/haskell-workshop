module Mastermind.GameState
  ( GameState (Starting),
    handleInput,
    Output (..),
    Input,
    parseInput,
    Color (..),
    Hint (..),
  )
where

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
  deriving (Eq, Show, Ord)

data Hint = Hint {numberOfCorrectColors :: Int, numberOfCorrectPositions :: Int}
  deriving (Show, Eq)

type Input = [Color]

data Output
  = SavedColorsToGuess
  | GuessCorrect
  | GuessIncorrect Hint
  deriving (Show, Eq)

handleInput :: Input -> GameState -> (Output, GameState)
handleInput input Starting = (SavedColorsToGuess, Guessing input)
handleInput input (Guessing colorsToGuess)
  | input == colorsToGuess = (GuessCorrect, Starting)
handleInput input (Guessing colorsToGuess) = (GuessIncorrect $ calculateHint input colorsToGuess, Guessing colorsToGuess)

calculateHint :: [Color] -> [Color] -> Hint
calculateHint guess colorsToGuess =
  let (numberOfCorrectPositions, leftoverGuess, leftoverSolution) = removeCorrectPositions guess colorsToGuess
   in Hint
        { numberOfCorrectPositions = numberOfCorrectPositions,
          numberOfCorrectColors = numberOfMatchingColors leftoverGuess leftoverSolution
        }

numberOfMatchingColors :: [Color] -> [Color] -> Int
numberOfMatchingColors guess solution =
  let sortedGuess = sort guess
      sortedSolution = sort solution
      match [] _ = 0
      match _ [] = 0
      match (g : gs) (s : ss) | g == s = match gs ss + 1
      match (g : gs) (s : ss) | g > s = match (g : gs) ss
      match (_ : gs) (s : ss) = match gs (s : ss)
   in match sortedGuess sortedSolution

removeCorrectPositions :: [Color] -> [Color] -> (Int, [Color], [Color])
removeCorrectPositions guess =
  foldl' go (0, [], []) . zip guess
  where
    go (numberOfCorrectPositions, leftoverGuess, leftoverSolution) (guessedColor, solutionColor) =
      if guessedColor == solutionColor
        then (numberOfCorrectPositions + 1, leftoverGuess, leftoverSolution)
        else (numberOfCorrectPositions, guessedColor : leftoverGuess, solutionColor : leftoverSolution)

-- | Extract all color chars from a String
parseInput :: String -> Input
parseInput = mapMaybe parseColor

parseColor :: Char -> Maybe Color
parseColor 'r' = Just Red
parseColor 'b' = Just Blue
parseColor 'g' = Just Green
parseColor 'y' = Just Yellow
parseColor 'w' = Just White
parseColor _ = Nothing