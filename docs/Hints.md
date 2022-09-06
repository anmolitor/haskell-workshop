# Hints

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

## I'm writing a complex application, how do I wire things together

https://www.fpcomplete.com/blog/2017/06/readert-design-pattern/

https://www.parsonsmatt.org/2018/03/22/three_layer_haskell_cake.html

https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/