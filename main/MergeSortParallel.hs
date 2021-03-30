
import System.Random
import Control.Parallel
import Control.Parallel.Strategies
import Data.List

-- cabal install --lib parallel
-- ghc -threaded MergeSortParallel.hs -rtsopts


get_random_list :: Int -> Int -> Int -> [Int]
get_random_list n m seed = map (mod m) $ take n $ random_list m seed

random_list :: Int -> Int -> [Int]
random_list m seed = randomRs (1, m) (mkStdGen seed)
            

merge_sort :: [Int] -> [Int]
merge_sort list
        | length list < 2 = list
        | otherwise = 
            let 
                merge :: [Int] -> [Int] -> [Int]
                merge [] right = right
                merge left [] = left
                merge (l : left) (r : right) = if l < r
                        then l : (merge left (r : right))
                        else r : (merge (l : left) right)
            in
                par sortLeft (pseq sortRight (merge sortLeft sortRight))
        where 
            left = take half list 
            right = drop half list
            half = (length list) `div` 2

            sortLeft = merge_sort left
            sortRight = merge_sort right


merge_sort_parallel :: (Show a, Ord a) => [a] -> [a]
merge_sort_parallel list | length list == 1 = list
                         | otherwise = 
                            let
                                merge :: Ord a => [a] -> [a] -> [a] -> [a]
                                merge res [] [] = reverse res
                                merge res left [] = reverse res ++ left
                                merge res [] right = reverse res ++ right
                                merge res (l : left) (r : right) = if l < r 
                                    then merge (l : res) (left) (r : right)
                                    else merge (r : res) (l : left) (right)
                                
                                split :: Ord a => [a] -> [a] -> [a] -> Int -> ([a], [a])
                                split left right [] len = (left, right)
                                split left right (x : list) len =
                                    if length left < div len 2
                                        then split (x : left) (right) (list) len
                                        else split (left) (x : right) (list) len
                            in
                                let
                                    pair = split [] [] list $ length list
                                    sortLeft = merge_sort_parallel $ fst pair
                                    sortRight = merge_sort_parallel $ snd pair
                                in
                                    par sortLeft (pseq sortRight (merge [] sortLeft sortRight))
                                

m = 1000
seed = 645623526

test n = 
    let
        rndList = get_random_list n m seed
    in
        merge_sort rndList

main = print$ take 10 $ test 10000000

-- for n = 50000000
-- -N1   : 11s
-- -N2   : 7.5s
-- -N3   : 6.5s
-- -N4   : 5.8s
-- -N6   : 6.4s

-- -O2 -N4  : 3.7s