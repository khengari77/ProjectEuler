import Data.Ord (comparing)
import Data.List (unfoldr, maximumBy)

collatzLength :: Int -> Int
collatzLength n  
            | n <= 1    = 0
            | even n    = 1 + collatzLength (div n 2) 
            | otherwise = 1 + collatzLength (3 * n + 1)

solve :: Int -> (Int, Int)
solve n =  maximumBy (comparing snd) . zip l $ (map collatzLength l)
        where l = [1..n]

main :: IO()
main = putStrLn $ show . fst . solve $ 1000000

