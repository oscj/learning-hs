-- Find the triangle that meets these conditions:
--  1. Length of the three sides are all integers
--  2. The length of each side is less than or equal to 10
--  3. The triangle's perimeter is equal to 24

rightTriangles :: Int -> [(Int, Int, Int)]
rightTriangles p = [ (a,b,c) | c <- [1..10], a <- [1..c], b <- [1..a], a^2 + b^2 == c^2, a + b + c == p]