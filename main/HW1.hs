
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
    in isPrime $ abs n

f3_1 :: Bool -> Bool -> Int
f3_1 a b = let
    fromEnum :: Bool -> Int
    fromEnum f = if f == True then 1 else 0
    in (+) (fromEnum a) (fromEnum b)

f4_1 :: Int -> Int
f4_1 n = let
    helper :: Int -> Int -> Int -> Int
    helper acc n 1 = acc + 1
    helper acc n d = if mod n d == 0 
                        then helper ((acc + ) d + (div n d)) (n) (d - 1)
                        else helper acc n (d - 1)
    in helper 0 n $ round $ sqrt $ fromIntegral $ abs n

f5_1 :: Int -> Int
f5_1 n = if (f4_1 $ n + 1) == n + 1 then n + 1 else f5_1(n + 1)

f6_1 :: Integer -> Integer
f6_1 n = if (f4_1 $ n + 1) == n + 1 then n + 1 else f6_1(n + 1)

f12_1 :: Int
f12_1 = (+) 1 1