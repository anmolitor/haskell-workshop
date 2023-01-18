module CommandLine.Types (CLInput (..), CLOutput (..)) where

import BasicPrelude
import Data.Text qualified as Text

class CLInput input where
  parseInput :: Text -> input

class CLOutput output where
  formatOutput :: output -> Text

instance CLInput Text where
  parseInput = id

instance CLInput String where
  parseInput = Text.unpack

instance CLOutput Text where
  formatOutput = id

instance CLOutput String where
  formatOutput = Text.pack
