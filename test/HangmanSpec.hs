module HangmanSpec where

import BasicPrelude
import Hangman.GameState qualified
import Test.Hspec

spec :: Spec
spec = describe "Hangman" $ do
  let (_, guessingState) = Hangman.GameState.handleInput "word" Hangman.GameState.Starting

  it "can make a wrong guess to remain in the same state" $ do
    let (output, stateAfterWrongGuess) =
          Hangman.GameState.handleInput "guess" guessingState
    stateAfterWrongGuess `shouldBe` guessingState
    output `shouldBe` Hangman.GameState.GuessIncorrect

  it "gives a hint when a character in the word is revealed" $ do
    let (output, _) =
          Hangman.GameState.handleInput "o" guessingState
    output `shouldBe` Hangman.GameState.RevealedChar "_o__"

  it "gives a hint when a character not in the word is revealed" $ do
    let (output, _) =
          Hangman.GameState.handleInput "x" guessingState
    output `shouldBe` Hangman.GameState.RevealedChar "____"

  it "gives a hint when a second character in the word is revealed" $ do
    let (output, _) =
          (Hangman.GameState.handleInput "d" . snd . Hangman.GameState.handleInput "o") guessingState
    output `shouldBe` Hangman.GameState.RevealedChar "_o_d"

  it "making the right guess restarts the game" $ do
    let (_, stateAfterRightGuess) =
          Hangman.GameState.handleInput "word" guessingState
    stateAfterRightGuess `shouldBe` Hangman.GameState.Starting
