main :: IO()
main = do
    print $ (repeater "I love Haskell") 3 " " -- == "I love Haskell I love Haskell I love Haskell"
    print $ (repeater "Quack") 5 "!" -- == "Quack!Quack!Quack!Quack!Quack

repeater :: String -> (Int -> String -> String)
repeater str = (\ count glue -> if count == 0 then str else str ++ glue ++ (repeater str) (count - 1) glue)