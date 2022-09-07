# Hints

## Things you could do in this workshop

Advent of code - small and fun exercises https://adventofcode.com/
Write a command line game
Write a parser (and interpreter) for your own toy language (for example a simple calculator)
Write a classic backend http server (for example a TODO Api)
...

## Containers

A lot of Haskell code uses different kinds of `containers` that can contain any type.

Examples include:
- Lists
- Maybe (analogous to Javas Optional)
- IO

All these types can be worked with in the same ways
and with the same functions (fmap, sequence, traverse, do-notation ...)

## I need to write a for loop

Try `foldl'` (not a typo) or recursion.

https://wiki.haskell.org/List_comprehension or the simple [m .. n] range syntax may also help.

## I don't know what function I need to complete my expression and I'm just trying out things

You can use the placeholder "_" where you don't know what to put there and the compiler will give you the type of the "hole".
For example:
```
giveMeAHint :: [Text] -> [Int]
giveMeAHint = fmap _
```
will result in the compiler telling you the binding "_" has the type signature `Text -> Int`.

## I'm writing a complex application, how do I wire things together

The ReaderT Design Pattern: https://www.fpcomplete.com/blog/2017/06/readert-design-pattern/

Structuring an application into three layers: https://www.parsonsmatt.org/2018/03/22/three_layer_haskell_cake.html

Parse, don't validate: https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/

Things I wish I knew before I was learning Haskell: https://web.archive.org/web/20220513191346/http://dev.stephendiehl.com/hask/