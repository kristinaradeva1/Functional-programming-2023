main :: IO()
main = do
    print $ countLetters "Hello World" == 5
    print $ countLetters "  haskell is    great" == 2
    print $ countLetters "Information Systems 2023" == 7

countWords :: String -> Int
countWords str = length (words str)

makeTuples :: String -> [(String, Int)]
makeTuples str = zip (splitWords str) [1 ..]
 where 
    splitWords str = words str

countLetters :: String -> Int
countLetters str = helper (makeTuples str) (countWords str) 
 where
    helper xs wordCount = length $ concat [ fst x | x <- xs, snd x == (wordCount - 1)]