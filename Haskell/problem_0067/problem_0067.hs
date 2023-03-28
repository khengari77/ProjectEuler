maximumSums :: [Int] -> [Int] -> [Int]
maximumSums [_] shorter = []
maximumSums longer [] = []
maximumSums longer shorter = (max (x+z) (y+z)) : maximumSums longer' shorter'
 where
   shorter' = tail shorter
   longer' = tail longer
   x = head longer
   y = head longer'
   z = head shorter

solve :: [[Int]] -> [Int]
solve = (foldl1 (maximumSums)) . reverse 

solve2 :: [[Int]] -> [Int]
solve2 [xs] = xs
solve2 (xs:xss) = let cs = solve2 xss in zipWith (+) xs (zipWith max (init cs) (tail cs))

main :: IO()
main =   putStrLn =<< show 
                    . head 
                    . solve2 
                    . map(map (read) . words) 
                    . lines <$> readFile "./input.txt"  
