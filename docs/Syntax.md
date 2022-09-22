# Syntax

To follow along, open the file `Syntax.hs` and try playing around with the syntax yourself.
If you want to evaluate an expression to see what it does, you can use
```
-- >>> yourExpression
```
and there should be an evaluate option from VSCode.


## Define a top level value
```
someInt :: Int
someInt = 420
```

A type signature is always declared after "::".
Note that the type signature is seperate from the implementation.

## Define a function

```
incrementByOne :: Int -> Int
incrementByOne n = n + 1
```

Feedback: Do not use binary operators. instead use "normal" functions

Similar syntax as above, since a function is also a value.
Note the variable binding in front of the equals sign.

We can call our function like this:

```
fortyTwo :: Int
fortyTwo = incrementByOne 41
```

Or we can use it to construct more complex functions:
```
addOneToAll :: [Int] -> [Int]
addOneToAll ints = fmap incrementByOne ints
```

Note that there are no brackets necessary. Instead spaces are used as function application.

## Partial application

The `fmap` function in the previous example has the signature `(a -> b) -> [a] -> [b]`.
We saw that when we provided two arguments to the function (`incrementByOne` and `ints`), we got a `[Int]` back.
But as the type signature already suggests, we can provide a single argument and get a function of type
`[Int] -> [Int]` back. That means the previous function could have also been written like this:

```
addOneToAll2 :: [Int] -> [Int]
addOneToAll2 = fmap incrementByOne
```

In fact, we can also partially applies operators like `+` so this is also an equivalent definition:

```
addOneToAll3 :: [Int] -> [Int]
addOneToAll3 = fmap (+ 1)
```

Note: When this went over the top of your head don't worry. You can just write every parameter on the left side of the equals sign and use them on the right side. A simple heuristic to keep in mind: If you apply the last parameter on the left side
as the last parameter on the right side, you can remove the parameters on both sides.

## Data Types

Some basic data types you can use as building blocks:
- Int
- Float
- Double
- Text
- Bool
- the Unit `()` 

Predefined containers:
- Lists `[t]`
- Set (from Data.Set)
- Map (from Data.Map)
- Maybe (== Optional -> Either a value or nothing)
- Either (Either the first case or the second case)
- Tuples e.g. `(Bool, Text, Float)`

### Defining a product type (record):

```
data ARecord = MakeARecord { fieldA :: Bool, fieldB :: Text }

aRecordInstance :: ARecord
aRecordInstance = MakeARecord { 
    fieldA = False, 
    fieldB = "a text" 
    }

toggleFieldA :: ARecord -> ARecord
toggleFieldA r = r { fieldA = not (fieldA r) }    
```

### Defining a union type:


```
data PlayingCard = Number Int | Jack | Queen | King | Ace

three :: PlayingCard
three = Number 3

jack :: PlayingCard
jack = Jack
```

Many of the standard types are implemented in the same way:

```
data MyBool = MyTrue | MyFalse
data MyList a = MyCons a (MyList a) | MyNil
```

Note here that lower case words in data declarations are type variables, similar to <T> in Java or Typescript. 

If we want to just wrap a type but have the same runtime representation (no performance cost), we can use newtypes:

```
newtype Email = Email Text
someMail = Email "abc@example.de"
```

At runtime, someMail will be a `Text` but at compile time it makes it possible to differentiate
between the two types `Text` and `Email`. We can also define aliases for types, which can make sense for readability:

```
type PlayingCards = [PlayingCard]

myHandOfCards :: PlayingCards
myHandOfCards = [Jack, Queen, Number 5] 
```

## Pattern matching

We can match on any type to seperate a functions logic for different cases.

```
cardValue :: PlayingCard -> Int
cardValue card = case card of
  Number n -> n
  Jack -> 2
  Queen -> 3
  King -> 4
  Ace -> 11
```

We can also use multiple function declarations to accomplish the same thing.

```
cardValue2 :: PlayingCard -> Int
cardValue2 (Number n) = n
cardValue2 Jack       = 2
cardValue2 Queen      = 3
cardValue2 King       = 4
cardValue2 Ace        = 11
```

This is useful e.g. when you want to match on multiple input values at the same time.

There is also useful syntactic sugar for the "take something as input and match on it" type of function (like `cardValue` above):

```
cardValue3 :: PlayingCard -> Int
cardValue3 = \case
  Number n -> n
  Jack -> 2
  Queen -> 3
  King -> 4
  Ace -> 11
```

The basic match on the two boolean cases `True` and `False` can also be done with `ìf ... then ... else`.

## Typeclasses

Typeclasses are like interfaces whose implementations are defined seperately from the data types (just a bit more powerful).

A classic example is the `Eq` typeclass for equality comparison.

It is defined like this:

Feedback: Use other example with no operators.

```
class Eq a where
  (==) :: a -> a -> Bool
```  

And instances (implementations) can be defined like this:

```
instance Eq PlayingCard where
  Number n1 == Number n2 = n1 == n2
  Jack      == Jack      = True
  Queen     == Queen     = True
  King      == King      = True
  Ace       == Ace       = True
  _         == _         = False
```

Note the usage of pattern matching and the wildcard pattern `_` 
for handling any cases not matched by the previous statements.

Most standard typeclasses can be "derived": they have a canonical implementation:
- Eq (equality)
- Ord (comparison)
- Show (like toString())
- Enum (enumerable - these is a mapping to the natural numbers)
- Bounded (the type has a finite amount of possible values and a min/max bound)
- Functor (mapping over something, like a List or a Maybe)
- Foldable (iterating over a container while accumulating a value, also known in other programming languages as "reduce")
- Generic (enables generic deriving of other typeclasses - a standard use case are the FromJSON/ToJSON typeclasses for JSON (de)serialization)

There are also some other flavours of the "deriving" approach, which go too far for this introduction.

We can also define instances that follow from other instances:

```
newtype Wrapper a = Wrapper a

instance (Show a) => Show (Wrapper a) where
  show (Wrapper a) = "Wrapped(" <> show a <> ")"
```

Here we say that a wrapper can be converted to a string when the wrapped type can.

## Helper functions

In the body of a top level function we can use `let .. in` or `where` to define helper functions/values.

```
complicatedOperation :: Int -> Int
complicatedOperation n = 
  let x = n * 2
      y = x - 5
  in x + y

complicatedOperation2 :: Int -> Int
complicatedOperation2 n =
  x + y
  where
    x = n * 2
    y = x - 5 
```

## Input and Output

IO is special in Haskell. How can Haskell be pure and at the same time be more than a calculator. The trick is simple, to understand it lets take a step back.

Starting with the following function
```
appendHello :: Text -> Text
appendHello = (<> "Hello") 
```

we want to have the same function **but it should track how often it was called!**

We can accomplish this without side effects:

```
appendHelloWithState :: Text -> Int -> (Int, Text)
appendHelloWithState text numberOfCalls =
    (numberOfCalls + 1, appendHello text)
```

And we could now define a type alias
```
type WithCalls a = Int -> (Int, a)
```

Haskells `IO` type is basically defined the same way:

```
type IO a = RealWorld -> (RealWorld, a)
```

Now Haskell does not actually keep a "RealWorld" object in memory (it can't), but its a useful abstraction none the less. 

Everything that has some sort of what we would call side effect, like writing to the console, making a network request etc. uses this `IO` type. So how do you use it?

### Do notation

```
readAndPrint :: IO ()
readAndPrint = do
    input <- getLine
    putStrLn (input <> " received!")
```

With `do` notation we can chain IO statements together, just like we would in an imperative language.

"<-" unwraps the IO layer around a type.

Every line needs to have type `IO` of something.
To wrap something in the IO layer, use `return`.

```
readTwoLines :: IO (Text, Text)
readTwoLines = do
    firstLine <- getLine
    secondLine <- getLine
    return (firstLine, secondLine)
```

## Modules

Any Haskell file should start with 

```
module YourModuleName where
```

The name needs to match the file path from the source directory i.e. the file `src/Internal/Syntax.hs` should have the module name `Internal.Spec`.

After that come imports

```
import BasicPrelude
import SomeOtherModule
import qualified Internal.Syntax as Syntax
```

For this workshop, you should always import `BasicPrelude`. This will get you a lot of standard functions and data types in scope (otherwise you will have nothing in scope at all and have to define everything from scratch).

Importing will add any members of the imported modules to the global scope of the current module. In case of name collisions, you can differentiate by prefixing the module name i.e. `BasicPrelude.mod` instead of `mod`.

To avoid name collisions, you can use a `qualified` import instead. Then, you can only access the imported modules members with the prefix notation.

There are also some other details/features here which we will skip for now.

Then you can provide any number of top level declarations without any indentation.

