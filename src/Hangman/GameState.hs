module Hangman.GameState
  ( GameState (Starting),
    handleInput,
    Output (..),
    Input,
  )
where

import BasicPrelude
import Data.Set qualified as Set

-- The logic behind the Hangman game :)

-- TODO 1: Correct guesses are currently not handled at all, meaning you cannot finish the game.
-- Can you write the necessary code to make the game congratulate you and restart if you enter a new word?

-- TODO 2: Can you complete the mask related functions such that the entered word is not given away after the first guess?

-- | The GameState is represented as a union type.
-- Either we are in a starting state (and waiting for a word to guess) or we are guessing the word.
-- In the latter case, we have some additional metadata:
--   * which word are we trying to guess
--   * which chars have we already entered
data GameState = Starting | Guessing String (Set Char)
  deriving (Eq, Show)

-- | Any Input is valid. A type alias can be helpful to make the intent of a parameter/return value clearer.
type Input = String

-- | This union type represents the result of the users action.
data Output
  = SavedWordToGuess
  | EmptyGuessGiven
  | GuessCorrect
  | GuessIncorrect
  | RevealedChar String
  deriving (Eq, Show)

-- | Pattern matching is a nice way of handling branching without increasing indentation.
-- The Haskell Compiler will remind us of any missing cases.
-- The first matching case will execute
handleInput :: Input -> GameState -> (Output, GameState)
handleInput [] state = (EmptyGuessGiven, state)
handleInput input Starting = (SavedWordToGuess, Guessing input Set.empty)
handleInput
  -- Haskells Lists are linked lists and constructed with the ":" binary operator and the empty list "[]".
  -- [1,2,3] is syntactic sugar for 1:2:3:[]. We can deconstruct a list in the same way using pattern matching.
  (firstChar : otherChars)
  -- Similarily, we can pattern match on the game state
  (Guessing wordToGuess revealedChars)
    -- The "guard" notation with pipes ("|") lets us handle if/else branching without increasing indentation
    -- The first block whose condition evaluates to True will be executed.
    | null otherChars =
        -- let...in is a nice way to give names to subexpressions. You can annotate subexpressions with a type
        -- but its not necessary - the most general type will be inferred. Hovering over it in your IDE
        -- should also get you the type information in case you need it.
        let newRevealedChars = Set.insert firstChar revealedChars
         in ( RevealedChar $ maskWord newRevealedChars wordToGuess,
              Guessing wordToGuess newRevealedChars
            )
handleInput guess (Guessing wordToGuess _) | guess == wordToGuess = (GuessCorrect, Starting)
handleInput _ state = (GuessIncorrect, state)

maskWord :: Set Char -> String -> String
maskWord revealedChars = fmap $ maskUnknownChar revealedChars

-- | If a char is not in the revealed chars we want to replace it with "_"
maskUnknownChar :: Set Char -> Char -> Char
maskUnknownChar revealedChars char =
  if Set.member char revealedChars then char else '_'