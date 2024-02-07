main :: IO()
main = do
    print $ eqSumPowDig 100 2 == 0
    print $ eqSumPowDig 1000 2 == 0
    print $ eqSumPowDig 2000 2 == 0
    print $ eqSumPowDig 200 3 == 153
    print $ eqSumPowDig 370 3 == 523
    print $ eqSumPowDig 370 3 == 523
    print $ eqSumPowDig 400 3 == 894
    print $ eqSumPowDig 500 3 == 1301
    print $ eqSumPowDig 1000 3 == 1301
    print $ eqSumPowDig 1500 3 == 1301

    print $ getNthSevenlikeNum 1 == 1
    print $ getNthSevenlikeNum 2 == 7
    print $ getNthSevenlikeNum 3 == 8
    print $ getNthSevenlikeNum 4 == 49 

-- 1 задача

isSpecial :: Int -> Int -> Bool 
isSpecial num pow = isSpecial' num 0
 where
    isSpecial' 0 result 
     | result == num = True -- проверяваме дали result и num съвпадат, защото това е условието за специално число
     | otherwise = False 
    isSpecial' leftover result = isSpecial' (div leftover 10) (result + mod leftover 10^pow) -- формираме result, който се образува като събираме всяка цифра на числото, повдигната на дадената степен

eqSumPowDig :: Int -> Int -> Int
eqSumPowDig hMax power = eqSumPowDig' hMax power 0 
 where
    eqSumPowDig' hMax power sum
     | power <= 1 = error "The power must be greater or equal to 1"
     | hMax < 1 = error "The special numbers must be greater than 1, so the upper interval must be too" 
     | hMax == 1 = sum -- дъното на рекурсията е когато hMax стигне 1, защото по условие числата, които проверяваме дали са специални, са по-големи от 1
     | isSpecial hMax power = eqSumPowDig' (hMax - 1) power (sum + hMax) -- намаляваме hMax, докато не достигне 1 и ако отговаря на условието да е специално, го прибавяме към сумата
     | otherwise = eqSumPowDig' (hMax - 1) power sum -- в противен случай отново намаляваме 

-- 2 задача 

convertToBinary :: Int -> Int                                                         -- напр. 4 
convertToBinary n = convertToBinary' n 1 0                                            -- 4:2 = 2, ост. 0
 where                                                                                --         => 1 * 0 + 0
    convertToBinary' n counter result      
     | n < 0 = error "The number must be a non-negative"                              -- 2:2 = 1, ост. 0
     | n > 0 = convertToBinary' (div n 2) (counter * 10) (counter * mod n 2 + result) --         => 10 * 0 + 0
     | otherwise = result                                                             -- 1:2 = 0, ост. 1
                                                                                      --         => 100 * 1 + 0
                                                                                      -- 0:2 не се изълнява, защото n вече не е по-голямо от 0, следователно се връща result => 100
getNthSevenlikeNum :: Int -> Int
getNthSevenlikeNum n = getNthSevenlikeNum' binaryN 0 0
 where
     binaryN = convertToBinary n
     getNthSevenlikeNum' 0 counter result = result -- дъното на рекурсията е, когато "изчерпаме" числото binaryN, т.е резултатът при (div binaryN 10) стане 0
     getNthSevenlikeNum' binaryN counter result = getNthSevenlikeNum' (div binaryN 10) (counter + 1) (mod binaryN 10 * 7^counter + result) -- напр. 1011 => 1 * 7^0 + 0 = 1
                                                                                                                                           --            => 1 * 7^1 + 1 = 8
                                                                                                                                           --            => 0 * 7^2 + 8 = 8
                                                                                                                                           --            => 1 * 7^3 + 8 = 351