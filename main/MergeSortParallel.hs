
import System.Random
import Control.Parallel
import Control.Parallel.Strategies
import Data.List

-- cabal install --lib parallel
-- ghc -threaded MergeSortParallel.hs -rtsopts

-- ghc -threaded -O3 -XBangPatterns MergeSortParallel.hs -rtsopts
-- +RTS -N4 -s -RTS

-- take 10 (merge_sort $ get_random_list 10000000 100 124325)

-- feager-blackholing

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
                merge sortLeft sortRight
            where 
                left = take half list 
                right = drop half list
                half = (length list) `div` 2

                sortLeft = merge_sort left
                sortRight = merge_sort right 


merge_sort_parallel1 :: [Int] -> [Int]
merge_sort_parallel1 list
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
                sortLeft `par` sortRight `pseq` (merge sortLeft sortRight)
            where 
                left = take half list 
                right = drop half list
                half = (length list) `div` 2

                sortLeft = merge_sort_parallel1 left
                sortRight = merge_sort_parallel1 right


merge_sort_parallel2 :: [Int] -> [Int]
merge_sort_parallel2 list
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
                runEval $ do 
                    !sortLeft <- rseq (merge_sort_parallel2 left)
                    !sortRight <- rseq (merge_sort_parallel2 right)
                    let !res = merge sortLeft sortRight
                    return res

                    where
                        left = take  half list 
                        right = drop half list
                        half = (length list) `div` 2
                                

m = 1000
seed = 645623526

test n = 
    let
        rndList = get_random_list n m seed
    in
        merge_sort_parallel2 rndList

main :: IO ()
main = do
    !sorted <- print (take 10 (test 1000000))
    return sorted

-- for n = 50000000
-- -N1   : 11s
-- -N2   : 7.5s
-- -N3   : 6.5s
-- -N4   : 5.8s
-- -N6   : 6.4s

-- -O2 -N4  : 3.7s