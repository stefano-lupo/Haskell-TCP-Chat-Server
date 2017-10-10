{-# LANGUAGE ParallelListComp #-}

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

-- where clause

-- Let Clause
-- Calculate list of bmis
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / (h^2)]


-- Case expression
-- Recall
head2 [] = error "No empty Lists"
head2 [x:_] = x

-- Equivelant to
head3 :: [a] -> a
head3 xs = case xs of
    [] -> error "No empty lists"
    (x: _) -> x

-- Cases can be used not only for parameter checking
describeList :: [a] -> String
describeList xs = "The list is a " ++ case xs of
    [] -> "empty list"
    [x] -> "Singleton list"
    xs -> "long list"

-- Or Equivelantly
describeList2 :: [a] -> String
describeList2 xs = "The lists is a " ++ whatList xs
    where   whatList [] = "empty list"
            whatList [x] = "Singleton"
            whatList xs = "Long list"

-- Recursive max function
maxMe :: (Ord a) => [a] -> a
maxMe [x] = x
maxMe (head : rest)
    | head > maxTail = head
    | otherwise = maxTail
    where maxTail = maxMe rest

-- Better yet
maxMe2 :: (Ord a) => [a] -> a
maxMe2 [] = error "Empty list"
maxMe2 [x] = x
maxMe2 (x:xs) = max x (maxMe xs)

-- Replicate eg replicate 3 5 = [555]
-- Remember : operator concantenates elements into a list and must be terminated with []
-- Note Num is not a subclass of ord, we need Ord for n to be compared with 0
replicate2 :: (Num n, Ord n) => n -> x -> [x]
replicate2 n x
    | n <= 0 = []
    | otherwise = x : replicate2 (n-1) x

-- take takes the first n elements from a list
take2 :: (Num n, Ord n, Eq a) => n -> [a] -> [a]
take2 n xs
    | xs == [] = []
    | n <= 0 = []
    | otherwise = head xs : take2 (n-1) (tail xs)

-- Reverse a list
reverse2 :: [a] -> [a]
reverse2 [] = error "Cant reverse an empty list"
reverse2 [x] = [x]
reverse2 (myHead : myTail) = reverse2 myTail ++ [myHead]

-- Recursive repeat function - repeats an element to inifity (infinite list)
repeat2 :: a -> [a]
repeat2 x = [x] ++ repeat2 x

-- Zip: takes two lists and zips them into a list of tuples (truncates a longer list)
zip2 ::  [a] -> [b] -> [(a, b)]
zip2 [] _ = []
zip2 _ [] = []
zip2 l1 l2 = (head l1, head l2) : zip2 (tail l1) (tail l2)

-- elem: checks if element is in list
elem2 :: (Eq a) => a -> [a] -> Bool
elem2 x [] = False
elem2 x (first: rest)
    | first == x = True
    | otherwise = elem2 x rest

-- Quick Sort
qs :: (Ord a) => [a] -> [a]
qs [] = []
qs (x:xs) =
    let smallerThanHead = [a | a <- xs, a <= x]
        largerThanHead = [b | b <- xs, b > x]
    in qs smallerThanHead ++ [x] ++ qs largerThanHead

-- Higher order functions
multThree :: (Num a) => a -> (a -> (a -> a))
multThree a b c = a * b * c

maxWithFour :: (Num a, Ord a) => a -> a
maxWithFour = max 4
-- max 4 here returns a ***function*** which takes one parameter and returns an a
-- We dont need the x in maxWithFour x = blah as max 4 will return that info

-- Compare with 100
-- Note this is compare 100 with a so 99 will be GT as 100 > 99
compareWith100 :: (Num a, Ord a) => a -> Ordering
compareWith100 = compare 100

-- Example with infix - Note must wrap with parenthesis
-- The parameter missing from the infix operation is replaced with the provided parameter
divideByTen :: (RealFloat a) => a -> a
divideByTen = (/10)

-- Using other parameter
divideTenBy :: (RealFloat a) => a -> a
divideTenBy = (10/)

-- Passing functions as parameters
-- Brackets bit: doMeTwice takes a  **function** which takes an a and returns a a
-- doMeTwice  also takes an a and then returns an a
doMeTwice :: (a -> a) -> a -> a
doMeTwice f x = f (f x)

-- Note even though doMeTwice requires a function that takes one parameter and another parameter
-- which is the parameter to that functino
-- We can pass it a function that takes two parameters but just partially call
multiplyBy9 :: (Num a) => a -> a
multiplyBy9 a = doMeTwice(*3) a

-- zip note needs parallel list comp
myZip :: [a] -> [b] -> [(a, b)]
myZip a b = [(ai, bi) | ai <- a | bi <- b]

-- zipWith - zips two lists and applies function to them
myZipWith ::  (a -> a) -> [a] -> [a] -> [(a, a)]
myZipWith f l1 l2 = [(f ai, f bi) | ai <- l1 | bi <- l2]

-- Same with recursion
myRecZipWith :: (a -> a) -> (b -> b) -> [a] -> [b] -> [(a, b)]
myRecZipWith _ _ [] _ = []
myRecZipWith _ _ _ [] = []
myRecZipWith f g (head1 : tail1) (head2 : tail2) = [(f head1, g head2)] ++ myRecZipWith f g tail1 tail2


-- Given a function flip it and return the swapped ordering of parameters
myFlip :: (a -> b -> c) -> (b -> a -> c)
myFlip f x y = f y x

-- Filter / Map
-- Greatest under 100,000 that divides by 3829
largest = head (filter p [100000,99999..])
    where p x = x `mod` 3829 == 0

-- Find sum of odd squares up to 10000 Using List Comprehension
-- Note this WONT work - it will not add in any wrong elements to the list
-- but will never terminate checking the natural numbers
-- basically it does NOT operate like takeWhile
sumOddSquares = sum [y | x <- [1..], let y = x^2, y < 100000, odd (y) == True ]

sumOddSquares2 = sum (filter odd (takeWhile (<10000) (map (^2) [1..])))
-- map (^2) [1..] generates infinite list of squares
-- take while of that list limits them to those <10000
-- filter odd filters that list to only contain the odd entries


-- Better spit up
listOfSquares = map (^2) [1..]

first10000Squares = takeWhile (<10000) listOfSquares

first10000SquaresOdd = filter (odd) first10000Squares

sumFirst1000SquaresOdd = sum first10000SquaresOdd

-- Collatz Sequence
-- start at some number
    -- If its even divide by 2, if its odd multiply by 3 and add 1
collatzChain :: Int -> [Int]
collatzChain 1 = [1]
collatzChain a
    | even a = a : collatzChain (a `div` 2)
    | otherwise = a : collatzChain ((3*a)+1)

-- For starting numbers between 1 and 100 how many collatz chains have lengths greater than 15
collatzChainQ = length [y | x <- [1..100], let y = collatzChain x, length y > 15]

-- Or using filtering
collatzChainQ2 = length (filter isLong (map collatzChain [1..100]))
    where isLong xs = length xs > 15

-- Or using lambdas
collatzChainQ3 = length (filter (\x -> length x > 15) (map collatzChain [1..100]))

-- Lambdas are often (wrongly) used in place of partials
add3ToList xs = map (+3) xs
add3ToList2 xs = map (\x -> x + 3) xs

-- Pattern matching lambdas
swap :: [(Integer, Integer)] -> [(Integer, Integer)]
swap xs = map (\(a, b) -> (b, a)) xs

-- Map using folds
map2 :: (a -> b) -> [a] -> [b]
map2 f xs = foldr (\x acc -> f x : acc) [] xs

maximumWithFold :: (Ord a) => [a] -> a
maximumWithFold xs = foldl1 (\acc x -> if x>acc then x else acc) xs

reverseWithFold :: [a] -> [a]
reverseWithFold xs = foldl (\acc x -> x : acc) [] xs

productWithFold :: (Num a) => [a] -> a
productWithFold = foldl1 (*)

filterWithFold :: (a -> Bool) -> [a] -> [a]
filterWithFold f = foldl (\acc x -> if f x then x : acc else acc) []

headWithFold :: [a] -> a
headWithFold = foldl1 (\acc x -> acc)


lastWithFold :: [a] -> a
lastWithFold = foldr1 (\_ acc -> acc)