module HW1 where 

f1_1 :: Int -> Int
f1_1 n = n

f2_1 :: Int -> Bool
f2_1 n = let
    isPrime :: Int -> Bool
    isPrime a | a == 0 = False
              | a == 1 = False
              | a == 2 = True
              | a > 2 = (length [x | x <- [2 .. ceiling $ sqrt $ fromIntegral $ a - 1]
                        ,a `mod` x == 0]) == 0
              | otherwise = False
    in isPrime $ abs n

f3_1 :: Bool -> Bool -> Int
f3_1 a b = let
    fromEnumS :: Bool -> Int
    fromEnumS f = if f == True then 1 else 0
    in (+) (fromEnumS a) (fromEnumS b)

f4_1 :: Int -> Int
f4_1 m = let
    helper :: Int -> Int -> Int -> Int
    helper acc n 1 = acc + 1
    helper acc n d = if mod n d == 0 
                        then helper ((acc + ) d + (div n d)) (n) (d - 1)
                        else helper acc n (d - 1)
    in helper 0 m $ round $ sqrt $ fromIntegral $ abs m

f5_1 :: Int -> Int
f5_1 n = if (f4_1 $ n + 1) == n + 1 then n + 1 else f5_1(n + 1)

f6_1 :: Integer -> Integer
f6_1 n = if (f4_but_integer $ n + 1) == n + 1 then n + 1 else f6_1(n + 1)

f7_1 :: Int -> Int -> Int
f7_1 m n | m == 0 = (+) n 1
         | m > 0 && n == 0 = f7_1 (m - 1) (1)
         | m > 0 && n > 0 = f7_1 (m - 1) (f7_1 m $ n - 1)
         | otherwise = 0

f8_1 :: Int -> Int -> Integer
f8_1 m n | m == 0 = toInteger $ (+) n 1
         | m > 0 && n == 0 = toInteger $ f7_1 (m - 1) (1)
         | m > 0 && n > 0 = toInteger $ f7_1 (m - 1) (f7_1 m $ n - 1)
         | otherwise = 0

f10_1 :: Double -> Double -> (Double, Double)
f10_1 n m = let 
    intPart :: Double -> Double -> Double
    intPart a b | a > 0 && b > 0 = fromInteger $ truncate $ (/) a b
                | a < 0 && b > 0 = fromInteger $ ((-1) *) $ ceiling $ ( / b) $ abs a
                | a > 0 && b < 0 = fromInteger $ ((-1) *) $ truncate $ (a / ) $ abs b
                | a < 0 && b < 0 = fromInteger $ ceiling $ ((/) $ abs a) $ abs b
                | b == 0 = undefined
                | otherwise = 0
    in  (intPart n m, (n - ) $ (*) m $ intPart n m)

f12_1 :: Int
f12_1 = (+) 1 1

f4_but_integer :: Integer -> Integer
f4_but_integer m = let
    helper :: Integer -> Integer -> Integer -> Integer
    helper acc n 1 = acc + 1
    helper acc n d = if mod n d == 0 
                        then helper ((acc + ) d + (div n d)) (n) (d - 1)
                        else helper acc n (d - 1)
    in helper 0 m $ round $ sqrt $ fromIntegral $ abs m