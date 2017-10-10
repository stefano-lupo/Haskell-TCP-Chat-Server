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
    - Note this requires an empty list at the end (to prepend to) to **create** a list
- List concatenation is done with infix `++` operator: eg `[1,2] ++ [3,4] == [1,2,3,4]`
- Destructuring can be used to extract out values from a list (and thus a string)
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