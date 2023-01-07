module Hangman
  ( start,
  )
where

import BasicPrelude

start :: IO ()
start =
  runLoop initialGameState gameLoop

runLoop :: state -> ((state, Text) -> (state, Text)) -> IO ()
runLoop state loop = do
  line <- getLine
  let (newState, output) = loop (state, line)
  putStrLn output
  runLoop newState loop

data GameState = GameState {}

initialGameState :: GameState
initialGameState = GameState

gameLoop :: (GameState, Text) -> (GameState, Text)
gameLoop (state, input) = (state, "bla")
