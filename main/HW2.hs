antiprimes :: Int -> [Integer]
antiprimes k = let
    helper :: [Integer] -> Int -> Int -> [Integer]
    helper acc i 0 = acc
    helper acc i k = if not $ isPrimeAbs i 
        then toInteger i : helper acc (i + 1) (k - 1)
        else helper acc (i + 1) (k)
    in helper [] 1 k


isPrimeAbs :: Int -> Bool
isPrimeAbs n = let
    isPrime :: Int -> Bool
    isPrime a | a == 0 = False
              | a == 1 = False
              | a == 2 = True
              | a > 2 = (length [x | x <- [2 .. ceiling $ sqrt $ fromIntegral $ a - 1]
                        ,a `mod` x == 0]) == 0
              | otherwise = False
    in isPrime $ abs n