import Data.List
main :: IO()
main = do
    print $ validWords t1 vocabulary
    print $ isPrimeDictionary t1 vocabulary == False
    print $ isPrimeDictionary t2 vocabulary == False
    print $ isPrimeDictionary t3 vocabulary == True

type Vocabulary = [String]

data BTree = Nil | Node Char BTree BTree 
 deriving (Show)

vocabulary :: Vocabulary
vocabulary = ["the", "a", "Some", "swimming", "liStS", "lisp"]

t1 :: BTree
t1 = Node 'a' (Node 't' (Node 'l' (Node 't' Nil Nil) (Node 'h' Nil Nil)) (Node 'i' (Node 'e' Nil Nil) (Node 'l' Nil Nil))) (Node 'h' (Node 's' (Node 'i' Nil Nil) (Node 'S' Nil Nil)) (Node 'a' (Node 't' Nil Nil) (Node 'S' Nil Nil)))

t2 :: BTree
t2 = Node 'a' (Node 't' (Node 'l' (Node 't' Nil Nil) (Node 'h' Nil Nil)) (Node 'i' (Node 'e' Nil Nil) (Node 'l' Nil Nil))) (Node 'h' (Node 's' (Node 'i' Nil Nil) (Node 's' Nil Nil)) (Node 'p' (Node 'p' Nil Nil) (Node 'S' Nil Nil)))

t3 :: BTree
t3 = Node 'a' (Node 't' (Node 'l' Nil Nil) (Node 'i' Nil Nil)) (Node 'h' (Node 's' Nil Nil) (Node 'p' Nil Nil))

getLevel :: BTree -> Int -> [Char]
getLevel Nil _ = []
getLevel (Node value left right) 0 = [value]
getLevel (Node value left right) k = getLevel left (k - 1) ++ getLevel right (k - 1)
 
allWords :: BTree -> [String]
allWords tree = takeWhile (not . null) [ getLevel tree num | num <- [0 ..]]

isPrime :: Int -> Bool
isPrime n = n > 1 && helper 2
 where
    helper d
     | d >= n = True
     | mod n d == 0 = False
     | otherwise = helper (d + 1)

levels :: BTree -> [String] -> Vocabulary -> Int
levels tree xs vocabulary = sum [snd lvl | lvl <- zip (allWords tree) [0 ..], word <- (allWords tree), vocabWord <- vocabulary, isInfixOf vocabWord word]

validWords :: BTree -> Vocabulary -> [String]
validWords tree vocabulary = [ vocabWord | word <- (allWords tree), vocabWord <- vocabulary, isInfixOf vocabWord word] 

lengthValidWords :: BTree -> Vocabulary -> Int
lengthValidWords tree vocabulary = sum [ (length vocabWord) | word <- (allWords tree), vocabWord <- vocabulary, isInfixOf vocabWord word] 

isPrimeDictionary :: BTree -> Vocabulary -> Bool
isPrimeDictionary tree vocabulary = isPrime num
 where
   num = (levels tree (validWords tree vocabulary) vocabulary) + lengthValidWords tree vocabulary
