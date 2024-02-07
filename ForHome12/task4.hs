import Data.List

main :: IO()
main = do
    print $ averageTemp [(Temp 1 23.6), (Temp 6 24.2), (Temp 11 24.2), (Temp 16 21.2), (Temp 21 23.8), (Temp 26 26.5), (Temp 31 24.5)]
    print $ closestAverage [(Temp 1 23.6), (Temp 6 24.2), (Temp 11 24.2), (Temp 16 21.2), (Temp 21 23.8), (Temp 26 26.5), (Temp 31 24.5)]

data Measuring = Temp Int Float

averageTemp :: [Measuring] -> Float
averageTemp measuring = sum [ temp | (Temp day temp) <- measuring] / fromIntegral (length measuring)

closestAverage :: [Measuring] -> Int
closestAverage measuring = fst $ foldr1 (\ x1@(d1, t1) x2@(d2, t2) -> if t1 < t2 then x1 else x2) [(day, abs ((averageTemp measuring) - temp)) | (Temp day temp) <-  measuring]