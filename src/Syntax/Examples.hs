module Syntax.Examples where

import BasicPrelude

-- Welcome to the first exercise of the Haskell workshop :)
-- We will start with some basic syntax so you understand what is going on
-- Below is a function that multiplies an integer by two.

-- Function name -----------------------------
----- | --------------------------------------
----- | ------ Type Signature (Input) --------
----- | --------- |---------------------------
----- | --------- |--Type Signature (Output) -
----- | --------- |----- | -------------------
----- V --------- V----- V -------------------
multiplyBy2 :: Int -> Int
multiplyBy2 n = n * 2

--------------- ^ ----------------------------
--------------- | ----------------------------
--------- Implementation ---------------------

-- Function application is just a space. No brackets necessary (although sometimes needed if order is unclear)
six :: Int
six = multiplyBy2 3

-- Functions always consist of an implementation which is signaled by
-- the equals (=) sign. You can optionally provide a type signature
-- with the double colon (::) syntax and arrows (->) for each input parameter

-- Write a function that adds 5 to an integer
add5 :: Int -> Int
add5 = error "Not implemented yet."

-- >>> add5 23
-- Not implemented yet.

-- Write a function that multiplies the first parameter by the second parameter and adds 5
multiplyAndAdd5 :: Int -> Int -> Int
multiplyAndAdd5 = error "Not implemented yet."

-- >>> multiplyAndAdd5 3 6
-- Not implemented yet.

-- You can pattern match on any input value of a function by using the same syntax as you would
-- on the right side of an equation.

notAVeryGoodFizzBuzz :: Int -> Text
notAVeryGoodFizzBuzz 3 = "Fizz"
notAVeryGoodFizzBuzz 5 = "Buzz"
notAVeryGoodFizzBuzz 6 = "Fizz"
notAVeryGoodFizzBuzz 9 = "Fizz"
notAVeryGoodFizzBuzz 10 = "Buzz"
notAVeryGoodFizzBuzz 12 = "Fizz"
notAVeryGoodFizzBuzz 15 = "FizzBuzz"
notAVeryGoodFizzBuzz n = tshow n

-- Write a function that returns True if the given Text is "FortyTwo" and False otherwise
isFortyTwo :: Text -> Bool
isFortyTwo = error "Not implemented yet."

-- >>> isFortyTwo "FortyTwo"

-- if/then/else or case/of can also be used for control flow
aBetterFizzBuzz :: Int -> Text
aBetterFizzBuzz n =
  if n `mod` 15 == 0
    then "FizzBuzz"
    else
      if n `mod` 5 == 0
        then "Buzz"
        else
          if n `mod` 3 == 0
            then "Fizz"
            else tshow n

boolToInt :: Bool -> Int
boolToInt b = case b of
  True -> 1
  False -> 0

-- You should see automatic refactoring opportunities.
-- Use them to simplify the code.
-- Guards are a solution to nested if/else chains.

-- Tuples can be used to return more than one value
plusOneAndPlusTwo :: Int -> (Int, Int)
plusOneAndPlusTwo n = (n + 1, n + 2)

-- Haskell is all about functions and composing things to build new things of the same type.
-- Naturally, there are many ways to compose functions.
-- The "." infix operator composes two functions such that the output of the second function is
-- piped into the first one.
-- You may know the notation `g . f` from Math:  (g . f) (x) == f(g(x))

-- Compose the `add5` and `multiplyBy2` functions such that the multiplication occurs first.
multiplyBy2ThenAdd5 :: Int -> Int
multiplyBy2ThenAdd5 = error "Not implemented!"

-- >>> multiplyBy2ThenAdd5 7

-- Maybe you noticed, but you did not have to explicitely pass all arguments to the functions
-- until they were fully evaluated. Just think of it as a method reference in Java or Javascript,
-- except all functions support it without "this" issues and also support partial application.

-- The following implementation does not bind any arguments on the left side, even though the type suggests it should
-- This is because "fmap" is applied partially. If it helps, consider the following desugaring:
-- fmap (+ 1) === (\list -> fmap (+ 1) list) === (\list -> fmap (\n -> n + 1) list)
add1ToEach :: [Int] -> [Int]
add1ToEach = fmap (+ 1)

-- Can you filter a list of texts such that only the ones that are "FortyTwo" remain?
filterFortyTwo :: [Text] -> [Text]
filterFortyTwo = error "Not implemented!"

-- >>> filterFortyTwo ["FortyTwo", "Bla", "FortyTwo"]
