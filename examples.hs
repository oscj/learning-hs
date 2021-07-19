cdoubleMe x = x + x

-- Produces a list from 1 to x
getListInRange :: Int -> [Int]
getListInRange x = [1..x]

-- Factorial - Multiply elements in list
factorial :: Int -> Int
factorial n = product [1..n]

-- Factorial - Recursion
factorialRec :: Int -> Int
factorialRec 0 = 1 -- Base Case
factorialRec n = n * factorialRec(n - 1)

-- Functions
circumfrence :: Float -> Float
circumfrence r = 2 * pi * r

-- Polymorphic Functions 
--  - Functions that use type variables (basically generics)

-- Pattern Matching
addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2) 

-- List Comprehension
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

-- Multiple parameters
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + x

-- FizzBuzz
fizzBuzzhelper :: Int -> String
helper n
  | mod n 3 == 0 && mod n 5 == 0 = "FizzBuzz"
  | mod n 3 == 0                 = "Fizz"
  | mod n 5 == 0                 = "Buzz"
  | otherwise                    = show n

fizzBuzz :: Int -> [String]
fizzBuzz n = map helper [1..n]

-- Find the triangle that meets these conditions:
--  1. Length of the three sides are all integers
--  2. The length of each side is less than or equal to 10
--  3. The triangle's perimeter is equal to 24

rightTriangles :: Int -> [(Int, Int, Int)]
rightTriangles p = [ (a,b,c) | c <- [1..10], a <- [1..c], b <- [1..a], a^2 + b^2 == c^2, a + b + c == p]