# Hints

A lot of Haskell code uses different kinds of `containers` that can contain any type.

Examples include:
- Lists
- Maybe (analogous to Javas Optional)
- IO

All these types can be worked with in the same ways
and with the same functions.

## fmap

To run a function on the value inside of a container
(think: List.stream.map, Optional.map) use `fmap` or the infix operator version `<$>`.

```
fmap :: (a -> b) -> f a -> f b
```

Substitute a concrete type like List or Maybe for `f` and it should hopefully make sense.

## >>= and do notation

You might know `flatMap` from Java or Javascript. In Haskell, this is the infix operator `>>=`, which again is polymorphic on the kind of container.

```
(>>=) :: (a -> f b) -> f a -> f b
```

Essentially this is just like `fmap`, but it flattens the wrapping by one layer.

Do notation is syntactic sugar for this function.
This:

```
readAndPrint :: IO ()
readAndPrint = do
    input <- getLine
    putStrLn (input <> " received!")
```
is equivalent to this:
```
readAndPrint :: IO ()
readAndPrint = getLine >>= 
    (\input -> putStrLn (input <> " received!"))
```

## I have the wrong container order

Lookup `traverse` or `sequence` :)

## So many brackets

Use `$` to avoid brackets:
```
func1 (func2 (func3 input))
```
is the same as
```
func1 $ func2 $ func3 input
```

## I need to write a for loop

Try `foldl'` (not a typo) or recursion.

https://wiki.haskell.org/List_comprehension or the simple [m .. n] range syntax may also help.

## I'm writing a complex application, how do I wire things together

https://www.fpcomplete.com/blog/2017/06/readert-design-pattern/

https://www.parsonsmatt.org/2018/03/22/three_layer_haskell_cake.html

https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/