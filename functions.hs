doubleMe x = x + x

doubleUs x y z = doubleMe x + doubleMe y + doubleMe z

doubleSmallNumber x = 
    if x > 100
    then x
    else doubleMe x

-- Very bad fizzbuzz
fizz x = if x `mod` 3 == 0 then -1 else x
buzz x = if mod x 5 == 0 then -2 else x
fizzbuzz x = 
    if fizz x == -1 && buzz x == -2
    then -99
    else 
        if fizz x == -1
        then fizz x
        else
            if buzz x == -2
            then buzz x
            else x



-- Iterates over x list first
multiSourceMultiply xList yList = [x*y | x <- xList, y <- yList] 

-- String manipulator
trimVowels str = [x | x <- str, not (x `elem` ['a', 'e', 'i', 'o', 'u'])]
trimSentenceVowels sentence = [trimVowels word | word <- sentence ]

-- Nested List Comprehensions
even2dMatrix matrix = [ [elem | elem <- row, even elem] | row <- matrix]

-- Traingle Problem: which right triangle with all integer sides less than 10 has a permiter of 24?
-- Generate all triangles with sides < 10
getAllTrianglesSidesLT10 = [(a,b,c) | a <- [1..10], b <- [1..10], c <- [1..10]]
getAllRightTrianglesSidesLT10 = [(a,b,c) | a <- [1..10], b <- [1..10], c <- [1..10], c^2 == a^2 + b^2]
getAllRightTrianglesSidesLT10Permiter24 = [(a,b,c) | a <- [1..10], b <- [1..10], c <- [1..10], c^2 == a^2 + b^2, a + b + c == 24]

-- Pattern Matching
-- Declare function (takes any type that implements Integral typeclass and returns a string)
luckyNo7 :: (Integral a) => a -> String
-- We can have multiple declarations and they will be checked from top to bottom
luckyNo7 7 = "Lucky number seven!"
luckyNo7 x = "Wrong number!"


-- We Should always include a catch all statement at the bottom of our pattern matched functions
factorial :: (Integral n) => n -> n
factorial 0 = 1
factorial n = n * factorial (n-1)

add2dVectors :: (Num x) => (x, x) -> (x,x) -> (x, x)
add2dVectors a b = (fst a + fst b, snd a + snd b) 

-- Extracting elements from triples
first :: (a, b, c) -> a
first (x, _, _) = x 

second :: (a, b, c) -> b
second (_, x, _) = x

third :: (a, b, c) -> c
third (_, _, c) = c

-- Our own head function
head' :: [a] -> a
head' [] = error "Cant Call head on an empty list"
head' (x:_) = x


tell :: (Show a) => [a] -> String
tell [] = "The List is empty"

-- tell (x: []) = "The list has one element: " ++ show x
tell [x] = "The List has one element: " ++ show x   -- More logical

tell (x: y: []) = "The List has two elements: " ++ show x ++ ", " ++ show y
tell (x: y: _) = "The List has a bunch of elements, the first two of which are: " ++ show x ++ ", " ++ show y

-- Length using list comprehension (generate a list of 1's (of length of same list), then sum them)
length' :: [a] -> Integer
length' a = sum [1 | b <-a] 

-- Recursive definition of length
length'' :: [a] -> Integer
length'' [] = 0
-- length'' someList = 1 + length'' (tail someList) -- Works but using the destructuring
length'' (_: restOfList) = 1 + length'' restOfList

-- Recursive definition of sum
sum' :: (Num a) => [a] -> a
sum' [a] = a
sum' (first:rest) = first + sum' rest

capitalizer :: String -> String
capitalizer [] = "No empty strings please"
-- Note firstChar requires the square brackets or show firstChar to make it a string
capitalizer wholeString@(firstChar: restOfChars) = "The first char of "  ++ wholeString ++ " is " ++ show firstChar ++ " and the rest are " ++ restOfChars


-- Guards
someFunc someParam
    | someParam < 10 = "Less than 10"
    | someParam > 10 = "Larger than 10"
    | otherwise = "10"

-- Comparator
myComparator :: (Ord a) => a -> a -> Ordering
a `myComparator` b 
    | a < b = LT
    | a == b = EQ
    | otherwise = GT

-- Where clause
xSquaredChecker :: (Real a ) => a -> String
xSquaredChecker x 
    | y < 0 = "That's negative"
    | y == 0 = "Thats zero!"
    | y > 0 = "Thats positive"
    where   y = x^2
            y :: Real