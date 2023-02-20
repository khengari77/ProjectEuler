fibonacci :: [Int]
fibonacci = scanl (+) 1 (1:fibonacci)
solve :: Int -> Int 
solve n = sum $ filter even $ takeWhile (<n) fibonacci
main :: IO()
main = do
 putStrLn $ show $ solve 4000000
