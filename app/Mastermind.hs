module Mastermind (main) where

import BasicPrelude
import Mastermind.Program qualified

main :: IO ()
main = do
  Mastermind.Program.start
