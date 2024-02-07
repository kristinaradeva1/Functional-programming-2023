import Data.List
import Data.Char

main :: IO()
main = do
    print $ height numberBTree == 4
    print $ height charBTree == 3

    print $ average numberBTree == 16.22
    --print $ average charBTree -- should not work
    print $ isLeaf numberBTree 2
    print $ sumLeaves numberBTree == 119
    --print $ sumLeaves charBTree -- shouldn't work

    print $ areEqual numberBTree (Node 5 (Node 12 (Node 1 (Node 96 Nil Nil) Nil) (Node 0 Nil Nil)) (Node 4 (Node 2 Nil Nil) (Node 5 Nil Nil))) == False
    print $ areEqual charBTree charBTree == True
    print $ areEqual numberBTree (Node 5 (Node 12 (Node 1 (Node 96 Nil Nil) Nil) (Node 0 Nil Nil)) (Node 8 (Node 2 Nil Nil) (Node 5 Nil (Node 21 Nil Nil)))) == False

    print $ setLevels numberBTree == Node (0,5) (Node (1,12) (Node (2,1) (Node (3,96) Nil Nil) Nil) (Node (2,0) Nil Nil)) (Node (1,4) (Node (2,2) Nil Nil) (Node (2,5) Nil (Node (3,21) Nil Nil)))
    print $ setLevels charBTree == Node (0,'k') (Node (1,'a') (Node (2,'h') Nil Nil) (Node (2,'s') Nil Nil)) (Node (1,'l') (Node (2,'e') Nil Nil) (Node (2,'l') Nil Nil))

    print $ mirrorTree numberBTree == Node 5 (Node 4 (Node 5 (Node 21 Nil Nil) Nil) (Node 2 Nil Nil)) (Node 12 (Node 0 Nil Nil) (Node 1 Nil (Node 96 Nil Nil)))
    print $ mirrorTree charBTree == Node 'k' (Node 'l' (Node 'l' Nil Nil) (Node 'e' Nil Nil)) (Node 'a' (Node 's' Nil Nil) (Node 'h' Nil Nil))

data BTree a = Nil | Node a (BTree a) (BTree a)
 deriving (Show, Eq)

numberBTree :: (Fractional a, Num a) => BTree a
numberBTree = Node 5 (Node 12 (Node 1 (Node 96 Nil Nil) Nil) (Node 0 Nil Nil)) (Node 4 (Node 2 Nil Nil) (Node 5 Nil (Node 21 Nil Nil)))

charBTree :: BTree Char
charBTree = Node 'k' (Node 'a' (Node 'h' Nil Nil) (Node 's' Nil Nil)) (Node 'l' (Node 'e' Nil Nil) (Node 'l' Nil Nil))

height :: BTree a -> Int
height Nil = 0
height (Node value left right) = maximum ([1 + height left] ++ [1 + height right])

average :: (Fractional a, Num a, RealFrac a) => BTree a -> a
average tree = roundTwoDigits ((sumTree tree) / (sizeTree tree)) 
 where
    sumTree :: (Num a) => BTree a -> a
    sumTree Nil = 0
    sumTree (Node value left right) = value + sumTree left + sumTree right

    sizeTree :: (Num a) => BTree a -> a
    sizeTree Nil = 0
    sizeTree (Node value left right) = 1 + sizeTree left + sizeTree right

    roundTwoDigits n = (fromIntegral (round (n * 100))) / 100
  

isLeaf :: (Num a, Eq a) => BTree a -> a -> Bool
isLeaf Nil _ = False
isLeaf (Node value Nil Nil) k = value == k
isLeaf (Node _ left right) k = isLeaf left k || isLeaf right k

sumLeaves :: (Num a, Eq a) => BTree a -> a
sumLeaves Nil = 0
sumLeaves (Node value left right) = if isLeaf (Node value left right) value then value + sumLeaves left + sumLeaves right else sumLeaves left + sumLeaves right

areEqual :: (Eq a) => BTree a -> BTree a -> Bool
areEqual _ Nil = False
areEqual Nil _ = False
areEqual (Node value Nil Nil) (Node value1 Nil Nil) = value == value1
areEqual (Node value left right) (Node value1 left1 right1)
 | value == value1 = areEqual left left1 && areEqual right right1
 | otherwise = False

getLevel :: BTree a -> Int -> [a]
getLevel Nil _ = []
getLevel (Node value _ _) 0 = [value]
getLevel (Node value left right) k = getLevel left (k - 1) ++ getLevel right (k - 1)

setLevels :: (Eq a) => BTree a -> BTree (Int, a)
setLevels (Node value left right) = helper (Node value left right) 0
 where
    helper Nil _ = Nil
    helper (Node value Nil Nil) counter = (Node (counter, value) Nil Nil)
    helper (Node value left right) counter 
     | elem value (getLevel (Node value left right) counter) = (Node (counter, value) (helper left (counter + 1)) (helper right (counter + 1)))
     | (counter >= (height numberBTree) || counter >= (height charBTree)) = (Node (counter, value) Nil Nil)
     | otherwise = (Node (counter, value) (helper left (counter + 1)) (helper right (counter + 1)))

mirrorTree :: BTree a -> BTree a
mirrorTree Nil = Nil
mirrorTree (Node value left right) = (Node value (mirrorTree right) (mirrorTree left))