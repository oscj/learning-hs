import Data.List
import Data.Maybe

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

twoSum :: (Eq a, Num a) => [a] -> a -> [(Int, Int)]
twoSum nums target = map (\(x,y) -> (validIndicies x y nums)) (validPairs nums target)
  where
    validPairs n t = filter (\(x,y) -> x + y == t) (pairs n)
    validIndicies x y n = (fromJust(findIndex (==x) n), fromJust (findIndex (==y) n))