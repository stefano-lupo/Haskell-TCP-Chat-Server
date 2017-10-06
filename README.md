# Haskell Notes
Notes I made while following [LearnYouAHaskell](http://learnyouahaskell.com/)

## Prefix vs Infix
- Prefix function calling: functionName param1 ... paramN   
- Infix function calling: param1 \`functionName\` param2

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

## Random Notes
- `'` can be used in variable names (eg myName' or my'Name) and is used to denote slightly modified functions or non lazy (strict) functions




