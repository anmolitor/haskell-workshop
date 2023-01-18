module MastermindSpec where

import BasicPrelude
import Mastermind.GameState (Color (..))
import Mastermind.GameState qualified
import Test.Hspec

spec :: Spec
spec = describe "mastermind" $ do
  let colorsToGuess = [Red, Green, Blue, White]
      (_, guessingState) = Mastermind.GameState.handleInput colorsToGuess Mastermind.GameState.Starting

  it "is possible to win by guessing the input colors" $ do
    let (output, stateAfterGuessing) = Mastermind.GameState.handleInput colorsToGuess guessingState
    output `shouldBe` Mastermind.GameState.GuessCorrect
    stateAfterGuessing `shouldBe` Mastermind.GameState.Starting
