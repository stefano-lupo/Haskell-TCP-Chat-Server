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