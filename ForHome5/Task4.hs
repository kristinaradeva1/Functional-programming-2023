import Data.Char

main :: IO()
main = do
    print $ specialSum 1 100 == 195 -- 61, 65, 69

specialSum :: Int -> Int -> Int
specialSum x y = sum [s | s <- [x .. y], elem (intToDigit 6) (show s) && mod s 4 == 1]