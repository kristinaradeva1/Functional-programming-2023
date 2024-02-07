import Data.Char
main :: IO()
main = do
    print $ getPrimesLC 1 100 == [7,17,37,47,67,71,73,79,97]
    print $ getPrimesLC 100 1 == [7,17,37,47,67,71,73,79,97]

    print $ getPrimesHOF 1 100 == [7,17,37,47,67,71,73,79,97]
    print $ getPrimesHOF 100 1 == [7,17,37,47,67,71,73,79,97]
 
isPrime :: Int -> Bool
isPrime n = n > 1 && null [d | d <- [2 .. n - 1], mod n d == 0]

getPrimesLC :: Int -> Int -> [Int]
getPrimesLC x y = [ p | p <- [min x y .. max x y], isPrime p && elem (intToDigit 7) (show p)]

getPrimesHOF :: Int -> Int -> [Int]
getPrimesHOF x y = filter (\ p -> isPrime p && elem (intToDigit 7) (show p)) [min x y .. max x y]