
import Data.List

antisort :: Ord a => [a] -> [a]
antisort list = 
    let
        isSort (a : b : c : xs) = if (a < b && b < c) || (a > b && b > c)
            then True else False
        broke (a : b : xs) = (:) b $ (:) a xs
    in
        if isSort list == True then broke list else list

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

isIn :: Eq t => t -> [t] -> Bool
isIn a [] = False
isIn a (x : xs) = if a == x then True else isIn a xs

antiunion :: Eq a => [a] -> [a] -> [a]
antiunion l1 l2 = reverse $ (++) (pull [] l1 l2) (pull [] l2 l1) where
    pull res l1 [] = res
    pull res l1 (x : l2) = if isIn x l1 == False
                                then pull (x : res) (l1) (l2)
                                else pull res l1 l2

antimerge :: Eq a => [a] -> [(Int, a)]
antimerge list = reverse $ antimerge' [] [] list list where
    antimerge' :: Eq a => [(Int, a)] -> [a] -> [a] -> [a] -> [(Int, a)] 
    antimerge' res used l [] = res
    antimerge' res used l (x : xs) = if isIn x used 
        then antimerge' res used l xs
        else antimerge' ((getCount x l, x) : res) (x : used) l (xs) 
            where
                getCount a l = getCount' 0 a l where
                    getCount' :: Eq a => Int -> a -> [a] -> Int
                    getCount' res a [] = res
                    getCount' res a (l : ls) = if a == l
                        then getCount' (res + 1) a ls
                        else getCount' res a ls

    