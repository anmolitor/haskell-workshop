module Hangman (main) where

import BasicPrelude
import Hangman.Program qualified

main :: IO ()
main = do
  Hangman.Program.start
