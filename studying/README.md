# Haskell Notes
Notes I made while following [LearnYouAHaskell](http://learnyouahaskell.com/)

## Prefix vs Infix
- Prefix function calling: functionName param1 ... paramN   
- Infix function calling: param1 \`functionName\` param2
- Note functions can also be **defined** using infix the same way

## If statements
- If statements are expressons in Haskell (always return something) and always require an `else`.
```haskell
if <condition>
then <something>
else <something>
```

## Lists
- Lists are homogeneous (no different data types in same list).
- Strings are just character lists, so list functions can be used on them
- Dont ++ concatenate elements onto lists (poor performance (iterates over entire list to append))
- `[5]:[1, 2, 3]` ==> [5, 1, 2, 3]
- Index lists with `[1, 2, 3] !! 0` ==> 1, `[[1,2], [3,4]] !! 0 !! 1` ==> 2
- Lists can be compared lexicographically (head with head, 2nd with 2nd...)
- `head [1,2,3]`, `tail [1,2,3]`, `init [1,2,3]`and `last[1,2,3]` return 1, [2, 3], [1,2] and 3 respectively
- `length <list>` and `reverse <list>` behave as expected
- `null <list>` returns true if the list is empty
- `take n <list>` and `drop n <list>` take and delete first n elements of a list respectively
- `max <list>`, `min <list>`, `sum <list>` and `product <list>` all work as expected.
- `elem x <list>` returns true if x is contained in the list (usually x)

## Texas Ranges
- Generate arithmetic sequence: `[1..5]` ==> [1, 2, 3, 4, 5], `['a'..'c']` ==> "abc"
- Can figure out the step by giving it a few values: `[0, 5..20]` ==> [0, 5, 10, 15, 20]
- Descending lists need an extra value: `[20, 19..1]`
- Lists can be infinite `[10,20..]` (use `take` to get a subset)
- `cycle [1,2,3]` ==> [1,2,3,1,2,3,.....] (again use `take`)
- `repeat 5` ==> [5,5,5,5,...] (again use `take`)
- `replicate 3 10` ==> [10,10,10] (equivelant to take 3 (repeat 10))

## List Comprehensions
- Allows us to generate a list according to an output function, an input set and a predicate (constraint)
- Mathematically: **S = {2x | x ϵ n, x <= 10 }** ==> [2, 4, 6, 8, 10, 12, 14, 16, 18, 20]
- In the above, 2x is the output function, the natural numbers are the input set and x<= 10 is the predicate (maybe)
- Haskell: `[<f(x)> | x <- <input set>, predicate1, ... , predicateN]`
    - `[if x mod 3 then -1 | x <- [1..100] ]
- The input set can draw from multiple lists
    - `multiSourceMultiply xList yList = [x*y | x <- xList, y <- yList]`
- List Comprehensions work on strings
    - `` trimVowels str = [x | x <- str, not (x `elem` ['a', 'e', 'i', 'o', 'u'])] ``
- Nested List Comprehensions
    - Since the output function is just a function that returns something, we can use another list comprehension (which is also just a function that returns a list)
    - `even2dMatrix matrix = [ [elem | elem <- row, even elem] | row <- matrix]`

## Tuples
- Analogous to objects or rows in an SQL table. 
- A list of tuples must all be the same size eg: `[(1,2), (2,3), (3,4)]`.
- Each tuple in the list must also have the same data types inside it eg: `[(1,2), (3, "Four")]` is invalid.
- Use tuples when we know *how many* components a piece of data should have.
- `zip list1 list2` forms a list of tuples containing `[..., ( list1[j], list2[j] ), ...)]`
    - Note this will truncate the longer list if lists are of different size

## Typeclasses
- Less like (java) classes, more like interfaces.
- If a type is part of a typeclass,  it implements the behaviour specified in the typeclass.
- An example is the `Eq` typeclass which is an interface for testing equality.
    - Any type which can be logically tested for equality should be a member of `Eq`
- `:t (==)` - checks the type of the equality operator
    - `(==) :: (Eq a) => a -> a -> Bool`
        - This states: `==` operator takes two parameters of the same type and maps them to a boolean.
        - The first part (before the `=>`) states that this function is a member of the `Eq` typeclass
- `:t (<)` - checks the type of the less than operator
    - `(<) :: Ord a => a -> a -> Bool`
        - The `<` operator maps two parameters of the same type which are a memeber of the `Ord` (order) typeclass to a Boolean.
- `Show` is another typeclass whos member's implement a method for their data types to be converted to strings.
    - Eg `show 3` ==> `"3"`
- `Read` is basically the opposite of `Show`
    - Eg `read "3"` but this actually wont work!
        - `:t read` ==> `Read a => String -> a`
        - Haskell doesn't know (cant infer) what type we want `a` to be (unless we use it after by `and`ing it for example - then it can infer we want a bool)
        - To fix this use **type annotations** to define the type we want
        - Eg `read "5" :: Int` will produce an int
        - Eg `read "(5, 'c')" :: (Int, Char)` will produce a tuple with (Int, Char)
- `Enum` is a typeclass for sequentially ordered types
    - `Int`, `Char` and a bunch of others make use of this.
- `Bounded` members have upper and lower bound
- `Num` is the type class for numbers
    - `:t 20` produces `(Num t) => t`.
        - This shows that numbers are polymorphic can be of any type that implements `Num`.
        - This allows the numbers to change from being floats / ints / doubles etc
    
## Guards
- Essentially if statements where the function's body is dependent on the value of its parameters
- Note no `=` in the syntax when using guards
```
someFunc someParam
    | someParam < 10 = "Less than 10"
    | someParam > 10 = "Larger than 10"
    | otherwise = "10"
```

## *where*
- Where allows us to bind certain expressions (that may be used multiple times) to a name.
- This reduces code duplication
- Essentially macros
 
## *let*
- Allows us to bind expressions locally
- `let <bindings> in <expression>`
- Can also be used to declare functions in local scope
- Can deconstruct tuples: `(let (a, b, c) = (1, 2, 3) in a + b + c)`
- Can be used in list comprehensions
    - They appear along side predicates but are just bindings (dont actually filter)
    - `bmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]`
        - The bindings are useable in the output function (before |) and in all predicates *after* the binding
            
## Case Expressions
- Used already in defining functions multiple times for different input parameters
    ```
    head' [] = error "List was Empty
    head' (x:_) = x
    ```
- Equivalent to:
    ```
    head' xs = case xs of    [] -> error "List was Empty"
                             (x:_) -> x
    ```
- In General
    ```
    case expression of  pattern -> result
                        pattern -> result
                        ...
    ```
    
## Higher Order Functions
- In actuality haskell functions can only take one parameter.
- Functions with multiple parameters actually form intermediate functions that can *return* functions
- Eg `:t max a b` is `(Num a) => a -> a -> a` === `(Num a) => a -> (a -> a)`
    - This can be read as `max` takes an `a` and returns a function which takes an `a` which returns an `a`
    - Consider a function `funcA` which takes two parameters and returns something *(thats not a function)*.
        - Calling `funcA` which one parameter, say `x1`, returns a function `funcB` which takes **one** parameter,
         say `x2` and calls `funcA` with `x2` and `x1`.
        - Eg `let maxWithFour = max 4`
            - `maxWithFour 10` = 10 as it is equivalent to `max 4 10`
- Note as `max 4` returns a **function** which takes one parameter, the definition of `maxWithFour` 
inherits these so we **dont* need to specify an input on the left side
    - So its **not** `let maxWithFour x = max 4`
- This can also be done with infix functions
    - The parameter missing from either side of the infix will be filled with the passed parameter
    - Need to wrap the infix operator in parenthesis
    - ```
        divideByTen :: (RealFloat a) => a -> a
        divideByTen = (/10)
        
        divideTenBy :: (RealFloat a) => a -> a
        divideTenBy = (10/)
        ```
- Careful of using `(-10)` and thinking it would be param - 10
    - This doesnt work as (-10) is defined as - 10 the number
    - Use `(subtract 10)` instead
    
## List Functions
- `map <func> list]` - returns list with the function applied to each element
- `filter <predicate function> list` - returns list that satisfies predicate function (eg `>3`)
- Can all be done with list comprehensions either
   
## Lambdas
- Essentially annonymous-throwaway functions
- `(\param1 .. paramN -> <body>)`
- Pattern Matching can be used in lambdas but only one pattern
    - No multiple patterns to check input parameter against
        - Generates a runtime exception if matching fails
## Folds / Scans
- Handy way to process a list and turn it into a single value.
- ```
  foldl (\acc current -> <some func>) <acc_start> <list>
  ```
- Fold takes:
    - A function which takes two parameters:
        - The accumulated value so far
        - The current item in the list
    - The starting value for the accumulator
    - The List to fold
- A better way to write this is to use currying
    - eg just insert the operator we wish to apply
        - `foldl (+) 0 [1,2,3]` == `6`
- `foldl1` and `foldr1` use first/last element accumulator 
- Note the lambda syntax for folding from left vs right
    - `foldl (\acc current -> <func>) <init_acc> <list>`
    - `foldr (\current acc -> <func>) <init_acc> <list>`
- `scanl`, `scanr`, `scanl1` and `scanr1` are the same as their respetive folds.
    - The difference is they return a **list** of the intermediate accumulator states, 
    - The final accumulator will be the last / first element of the result when using `scanl` and `scanr` respectively.
## If parameter is on both sides of the expression, it can be removed from both sides due to currying
- Im not really sure if this is right
- But for example this works:
    - `reverseWithFold xs = foldl (\acc x -> x : acc) [] xs`
    - `reverseWithFold = foldl (\acc x -> x : acc) []`
        - This returns a function which is takes one parameter which is a list
    
## Monoids
- A monoid is a collection of things with a rule for combining the things
    - Eg a clock is a monoid - has a collection of numbers and a rule for combining them: `(x + y)  % 12`
    - The rule itself obeys some rules
        - Associativity: `x <rule applied to> (y <rule applied to> z) === z <rule applied to> (x <rule applied to> y)` [note `rule applied to` is infix here]
        - Has a special member: such that `x <rule applied to> special member == x` and special member `rule applied to` x = x 
            - This special member is a function such that
            ```
            id :: a -> a
            id a = a
            ```
            - Eg in a clock this special member is 12: 3 `rule applied to` 12 = `(3+12)%12` = 3
            - Note monoids are basically function composition and f(g(x)) \= g(f(x)) (not nescesarily)
- In programming sense, monoids are just function compositions
    - Best if types line up : eg f :: a -> a, g :: a -> a so defining h = f (g a) or h = g (f a) implies h :: a -> a
        - If we can define all our functions like this, we can *always* compose them without worry of anything failing
        - Otherwise, to the land of monads.
- In haskell terms, monoids have the following:
    1. An associative binary (as in two operands) function - `mappend`
        - This function describes how they are to be combined
    2. A value which acts as an identity with respect to the binary function - `mempty`
    - Eg List: `mappend = ++`, `mempty = []`
        - `++` is associatve : `[1,2] ++ ([3,4] ++ [5,6]) = [1,2,3,4,5,6]` === `([1,2] ++ [3,4]) ++ [5,6]
        - `[]` returns the same list: `[1,2,3] ++ [] = [1,2,3]`
    3. A flatten function that takes a list of the monoids and flattens them into a single monoid - `mconcat`
        - This is implemented by default by simply `fold` over the list with mempty as the starting value and `mappend` each value as the callback function
        - Eg `mconcat [[1,2],[3,4]]` = [] --> [] ++ [1,2] --> [1,2] ++ [3,4] = [1,2,3,4]
            - `mconcat` for a list is simply `concat`
- Numbers can also be considered monoids:
    - `mappend = *`, `mempty = 1`
        - `x 'mappend' (y 'mappend' z) === (x 'mappend' y) 'mappend' z` - 3*(4 * 5) = (3 * 4) * 5
        - `mempty = 1` ---> `x 'mappend' 1 = x` - 3 * 1 = 3  
    - Could also use `mappend = +`, `mempty = 0`
- The `Any` type is an instance of monoid (for booleans)
    ```    
    instance Monoid Any where
        mempty = Any False
        Any x `mappend` Any y = x || y
   ```
- All is also a monoid just uses && and True
- `Ordering`is also an instance of monoid (compares and returns LT/EQ/GT)
	- We compare from left to right and once we find anything that was not EQ we stop and return that value
	- For example comparing two strings, first compare their length, compare them alphabetically
		- Only if their lenghts are EQ do we need to compare them alphabetically
	```haskell
	instance Monoid Ordering where
		mempty = EQ
		LT `mappend` _ = LT
		EQ `mappend` y = y
		GT `mappend` _ = GT	
	```
	- If we want to append two instances of `Ordering`, we look at the second instance only if the result of the first
	 comparison was EQ, otheriwse we return the first one
	  ```haskell
			compareString s1 s2 = (length s1 `compare` length s2) `mappend` (s1 `compare` s2)
		```
	- The `mappend` implementation of `Ordering` allows us to compare things and specify priorities on each comparison

## Functors
### In JS context from [here](https://www.youtube.com/watch?v=DisD9ftUyCk)
- ~~Functions that passed a *wrapped* or *boxed* value and a function~~
- Objects that ***implement*** the `map` method (eg arrays - we can `map` over an array)
    - Their implementation of `map` then is a function that is passed a *wrapped * or *boxed* value and a function
    - This passed value is then *unwrapped/unboxed* and each element inside is passed to the supplied function
    - Once the inerds have been processed by the supplied function, the elements are *reboxed/rewrapped*
- For something to be a functor it must:
    1. Transform the contents: transforms the contents of the supplied value using the provided function
    2. Functors also maintain structure - eg array : mapping over an array returns an array (same parent type) of the **same length** (although may not be an array of same type)
        - Functors are *generic* containers (eg array) - they must work with *any* type
            - Eg string: although we could map over a string, the returned value must now be a string which can't wrap any generic type
    3. Return a new functor: the value a functor returns (when mapped) is another functor
        - We can then *chain* functors together
        - Eg `js [1,2,3].map(elem => return elem*2)` = [2,4,6] 
            - [2, 4, 6] can then be mapped again. 


## Applicative Functors
- Give me a *functor* (iteritable(?) object) of functions from a -> b, a functor of `a` and ill give you back 
  a functor of type `b` where each element had the function applied to it.
- They Applicative type class has two functions (where f is a functor):
	1. `pure :: a -> f a`
		- Wraps up a value in the applicative functor f
		- We choose this as we define `pure` when making a type an instance of Applicative
		- It takes a value and wraps it up in a *pure* (default predefined) context which contains that value
	2. `(<*>) :: f (a -> b) -> f a -> f b`
		- This basically takes a functor (eg list - remember functors are just things that implement map) of functions from `a -> b`   
		and a functor of elements of `a` and maps over them, unboxes each function from the functor and applies it to produce a functor of b.
		```haskell
		let functorOfFuncs = map (*) [1,2,3] -- = [*1, *2, *3, *4]
		functorOfFuncs <*> [10] -- = [10,20,30,40]
		```
- Another example is `Maybe` which is an instance of `Applicative`
			```haskell
			instance Applicative Maybe where
				pure = Just -- Equivelant to pure x = Just x
				Nothing <*> _ = Nothing
				(Just f) <*> something = fmap f something
			```
			- `pure` simply takes a value and lifts it into the default context (Just for Maybe)
			- `Nothing <*> _ = Nothing` - We won't get any functions to apply by unwrapping `Nothing`
			- `(Just f) <*> something` - `f` is the function wrapped in the functor, so (with access to f) we just map over `something` and apply `f`
- The `Control.Applicative` module also exports `<$>`
	- This is simply an infix version of `fmap`
	- `f <$> x = fmap f x`
- Lists are also instances of Applicative
	- Eg: `[(^2), (*2), (+2)] <*> [1,2]` = ~~\[1, 2, 3, 4, 4, 4\]~~ [1, 4, 2, 4, 3, 4]
	- Eg: `[(^), (*), (+)] <*> [1,2] <*> [2]` = [1, 4, 2, 4, 3, 4] as before
	- Eg: `(++) <$> ["One", "Two", "Three"] <*> ["!", "?", "."]` = ["One!", "One?" ... "Three."]


## Monad
- Monad is a type class that implements bind and return
    - Bind: `(>>=) :: m a -> (a -> m b) -> m b`
        - Monads are like boxes that wrap a value
        - This monad can be arbitrarily complex (eg does some concurrent stuff, IO, mutable state)
        - However `bind` takes this monadic value and a function.
            - It then unboxes this monadic value and performs the function on the **normal** data that was inside it (eg an int) and then returns this value wrapped up in another monad.
            - The benefit here is that the passed function can be **purely** functional, allowing us to reason / describe our functions as normal.
            - However the overall monad can be doing some arbitrarily complex things.



## Random Notes
- `'` can be used in variable names (eg myName' or my'Name) and is used to denote slightly modified functions or non lazy (strict) functions
- `functionName :: <inputType1> ... <inputTypeN> -> <outputType>` is shorthand for "functionName maps <input types> to <output type>" and should be used for function declarations (Return type always last).
- `Integer` is a slower but unbounded form of `Int`
- `a` is used to denote a type variable (can be of any type)
    - An example is List's `:t head` function which returns `[a] -> a` as it is passed a list and returns the first element of that list. 
    - However the type of data inside the list is unknown and thus is type variable.
    - This allows us to write generic functions.
- `->` vs `=>`.
    - `->` seperates parameters to a function.
    - `=>` represents class constraints - anything before these (when `:t <somefunc` is called) are typeclasses that this type is a member of.
- `_` represents a value which we dont care about 
    - Eg getting first value of a triple might be `first (a, _, _) = a
- `x:y:z:[]` is just another way of making a list containing `[x,z,y]`
    - This is the cons operator and prepends **elements** to a list
    - Note this requires ~~an empty~~ a list at the end (to prepend to) to **create** a list
        -- This can be an empty list `[]` or a list with elements already in it `[1, 2]` 
- List concatenation is done with infix `++` operator: eg `[1,2] ++ [3,4] == [1,2,3,4]`
- ~~Destructuring~~ Pattern Matching can be used to extract out values from a list (and thus a string)
    - `(elem1: elem2 : ... : restOfList)`
    - This can be used on the input parameters of functions
    - Destructuring requires the parenthesis separated by commas
- Patterns are an extension of destructuring which allow you to keep a reference to the entire object and destructure it into variables
    - `wholeList@(first: rest)` - wholeList = [1,2,3], first = 1, rest = [2, 3]
- Recursive (List) algorithms
    - Seems like code is cleaner if you define the edge case as the empty list as oppose to the list containing one element.
    - Its usually equivalent but will just take one extra function call to reach the edge case but this also handles the empty list exception.
    - Start with the expression that actually builds the final list (or the intermediate lists for each recursive call)
        - This will define how the algorithm needs to output the data at each stage
- `takeWhile <predicate> list`
    - Iterates over list while predicate is true
    - Stops once predicate is false
- Destructuring
	- `myList@(head:tail)` defines head and tail
        
    