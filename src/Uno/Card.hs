{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MagicHash #-}

-- | This is an example of some type-level magic/hackery!
-- This is NOT what you see in your average Haskell codebase, but serves to show that Haskells type system
-- is incredibly advanced and expressive. Libraries can capitalize greatly by making illegal states irrepresentable
-- by the type system.
--
-- In this example, the 'play' method makes sure that only valid cards are played.
-- If you try to (for instance) play a Green 5 on top of a Red 4, the compiler complains (about not finding an instance)
module Uno.Card (test) where

import BasicPrelude
import GHC.Exts (proxy#)
import GHC.TypeNats (KnownNat, natVal')

test :: IO ()
test =
  play (NumberCard @4 @'Red) start
    >>= play (NumberCard @4 @'Red)
    >>= play (NumberCard @6 @'Red)
    >>= play (NumberCard @6 @'Blue)
    >>= play (Draw2 @'Blue)
    >>= play (NumberCard @9 @'Blue)
    >>= play (NumberCard @9 @'Blue)
    >>= play (Rainbow @'Green)
    >>= play (NumberCard @4 @'Green)
    >>= play (Reverse @'Green)
    >>= play (Skip @'Green)
    >>= play (RainbowDraw4 @'Yellow)
    >> pure ()

data Color = Red | Blue | Yellow | Green

data NumberCard color n where
  NumberCard :: KnownNat n => NumberCard color n

data SpecialCard color where
  Draw2 :: SpecialCard color
  Reverse :: SpecialCard color
  Skip :: SpecialCard color

data JokerCard color where
  Rainbow :: JokerCard color
  RainbowDraw4 :: JokerCard color

class IsPlayable currentCard nextCard where
  describeMove :: nextCard -> currentCard -> Text

instance
  {-# OVERLAPS #-}
  (ColorToText color, KnownNat n, KnownNat m) =>
  IsPlayable (NumberCard color n) (NumberCard color m)
  where
  describeMove = sameColorReasoning

instance
  {-# INCOHERENT #-}
  (ColorToText color, ColorToText color', KnownNat n) =>
  IsPlayable (NumberCard color n) (NumberCard color' n)
  where
  describeMove = sameNumberReasoning

instance
  (ColorToText color, KnownNat n) =>
  IsPlayable (SpecialCard color) (NumberCard color n)
  where
  describeMove = sameColorReasoning

instance (ColorToText color, KnownNat n) => IsPlayable (NumberCard color n) (SpecialCard color) where
  describeMove = sameColorReasoning

instance (ColorToText color) => IsPlayable (SpecialCard color) (SpecialCard color) where
  describeMove = sameColorReasoning

instance (ColorToText color, ColorToText anyColor, KnownNat n) => IsPlayable (NumberCard color n) (JokerCard anyColor) where
  describeMove = jokerReasoning

instance (ColorToText color, ColorToText anyColor) => IsPlayable (SpecialCard color) (JokerCard anyColor) where
  describeMove = jokerReasoning

instance (ColorToText color, KnownNat n) => IsPlayable (JokerCard color) (NumberCard color n) where
  describeMove = afterJokerReasoning

instance (ColorToText color) => IsPlayable (JokerCard color) (SpecialCard color) where
  describeMove = afterJokerReasoning

play :: IsPlayable currentCard nextCard => nextCard -> currentCard -> IO nextCard
play newCard currentCard = do
  putStrLn $ describeMove newCard currentCard
  pure newCard

start :: NumberCard 'Red 5
start = NumberCard @5

sameColorReasoning :: (Show currentCard, Show nextCard) => nextCard -> currentCard -> Text
sameColorReasoning nextCard currentCard =
  "You play "
    ++ tshow nextCard
    ++ " on top of "
    ++ tshow currentCard
    ++ ", since you can play cards with the same color on each other."

sameNumberReasoning :: (Show currentCard, Show nextCard) => nextCard -> currentCard -> Text
sameNumberReasoning nextCard currentCard =
  "You play "
    ++ tshow nextCard
    ++ " on top of "
    ++ tshow currentCard
    ++ ", since you can play cards with the same number on each other."

jokerReasoning :: (Show currentCard, Show nextCard) => nextCard -> currentCard -> Text
jokerReasoning nextCard currentCard =
  "You play "
    ++ tshow nextCard
    ++ " on top of "
    ++ tshow currentCard
    ++ ", since you can play a joker on any non-joker card."

afterJokerReasoning :: (Show currentCard, Show nextCard) => nextCard -> currentCard -> Text
afterJokerReasoning nextCard currentCard =
  "You play "
    ++ tshow nextCard
    ++ " on top of "
    ++ tshow currentCard
    ++ ", since the joker wishes for this color."

class ColorToText (color :: Color) where
  colorToString :: String

instance ColorToText 'Red where
  colorToString = "Red"

instance ColorToText 'Blue where
  colorToString = "Blue"

instance ColorToText 'Yellow where
  colorToString = "Yellow"

instance ColorToText 'Green where
  colorToString = "Green"

instance (ColorToText color, KnownNat n) => Show (NumberCard color n) where
  show _ = colorToString @color ++ " " ++ show (natVal' (proxy# @n))

instance (ColorToText color) => Show (SpecialCard color) where
  show card =
    colorToString @color ++ " " ++ case card of
      Draw2 -> "Draw2"
      Reverse -> "Reverse"
      Skip -> "Skip"

instance (ColorToText color) => Show (JokerCard color) where
  show card =
    ( case card of
        Rainbow -> "Rainbow"
        RainbowDraw4 -> "RainbowDraw4"
    )
      ++ " wishing for the color "
      ++ colorToString @color
      ++ " to be played next"