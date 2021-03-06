
import Data.List
import Data.Char
import Data.Maybe

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

antiintercalate :: Eq a => [a] -> [(Int, a)]
antiintercalate list = reverse $ helper [] (head list) 1 (tail list) where
    helper :: Eq a => [(Int, a)] -> a -> Int -> [a] -> [(Int, a)]
    helper res prev cnt [] = ((cnt, prev) : res)
    helper res prev cnt (l : list) = if l == prev 
        then helper res prev (cnt + 1) list
        else helper ((cnt, prev) : res) l 1 list


unpacking :: (Int, a) -> [a]
unpacking package = unpack [] (fst package) (snd package) where
        unpack res 0 a = res
        unpack res cnt a = unpack (a : res) (cnt - 1) (a)

antiantiintercalate :: [(Int, a)] -> [a]
antiantiintercalate pp = 
    let
        helper res [] = res
        helper res (p : ps) = helper (res ++ (unpacking p)) (ps)
    in 
        helper [] pp


getNumberOrNot :: String -> Maybe Integer
getNumberOrNot str = 
    let
        isOk ch = if isDigit ch || ch == ' ' || ch == '\n' || ch =='\t'
            then True
            else False
        helper :: Integer -> [Char] -> Maybe Integer
        helper res [] = Just res
        helper res (s : str) = if isOk s 
            then if isDigit s
                then helper (res * 10 + (toInteger $ digitToInt s)) (str)
                else helper res str
            else Nothing
    in helper 0 str

maybeMaybeMaybeMaybeMaybeMaybeMaybeOrNot :: Maybe (Maybe (Maybe (Maybe (Maybe (Maybe a))))) -> a -> a
maybeMaybeMaybeMaybeMaybeMaybeMaybeOrNot mb a = if isJust mb
    then fromJust$ fromJust$ fromJust$ fromJust$ fromJust$ fromJust mb
    else a

stupidTraverse :: [Maybe a] -> Maybe [(a, a, a, a)]
stupidTraverse list = 
    let
        split :: [(a, a, a, a)] -> [a] -> [(a, a, a, a)]
        split res [] = reverse res
        split res (x1 : x2 : x3 : x4 : xs) =  split ((x1, x2, x3, x4) : res) xs 

        check' :: [a] -> Maybe [(a, a, a, a)]
        check' list = if length list `mod` 4 == 0  && not (length list == 0)
            then Just $ split [] list
            else Nothing
    in check' $ map fromJust list

-- dfs

dfs :: [(Int, Int)] -> Int -> Int -> Bool
dfs list from to =
    let
        split :: [[Int]] -> Int -> [Int] -> [(Int, Int)] -> [[Int]]
        split res prevFrom acc [] = reverse (acc : res)
        split res prevFrom acc (l : list) = if fst l == prevFrom
            then split res prevFrom (snd l : acc) list
            else split (acc : res) (fst l) [snd l] list
    in
        dfs' (split [] (fst $ head list) ([snd $ head list]) (tail $ sort list)) from to from [] where
            dfs' :: [[Int]] -> Int -> Int -> Int -> [Int] -> Bool
            dfs' list from to v used | to == v = True
                                     | isIn v used == True = False
                                     | otherwise = 
                                            let
                                                nextStep :: [[Int]] -> [Int] -> Bool
                                                nextStep list [] = False
                                                nextStep list (x : xs)  | dfs' list from to x (x : used) == True = True
                                                                        | otherwise = nextStep list xs

                                                getN :: [[Int]] -> Int -> Maybe [Int]
                                                getN [] n = Nothing
                                                getN (l : list) 0 = Just l
                                                getN (l : list) n = getN list (n - 1)                                                                

                                                helper :: [[Int]] -> Maybe [Int] -> Bool
                                                helper list mb  | mb == Nothing = False
                                                                | otherwise = nextStep list $ fromJust mb
                                            in 
                                                helper list (getN list v)

