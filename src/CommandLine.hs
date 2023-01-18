module CommandLine (runGame, runGenericGame) where

import BasicPrelude
import CommandLine.Types
import System.IO qualified

-- | Run a command line program starting with an initial state and a transition function
runGame :: state -> ((state, Text) -> (state, Text)) -> IO ()
runGame state handleInput = do
  programWasTerminated <- System.IO.isEOF
  unless programWasTerminated go
  where
    go = do
      line <- getLine
      let (newState, output) = handleInput (state, line)
      putStrLn output
      runGame newState handleInput

-- | Our two programs have a very similar structure.
-- Typeclasses provide a way to extract common code that works for a variety of data types.
-- They are very similar to Interfaces in other languages.
-- You can read this functions signature as:
--  - Provided the 'input' type is a valid CommandLineInput (implements the CLInput typeclass)
--  - and the 'output' type is a valid CommandLineOutput (implements the CLOutput typeclass)
--  - and you pass an initial state
--  - and you pass a state transitioning function
--  -> I will do some Input/Output (run the command line game)
runGenericGame :: (CLInput input, CLOutput output) => state -> ((state, input) -> (state, output)) -> IO ()
runGenericGame state handleInput =
  runGame state $ \(st, inputText) ->
    let (newSt, output) = handleInput (st, parseInput inputText)
     in (newSt, formatOutput output)