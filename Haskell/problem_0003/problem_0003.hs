primes :: [Int]
primes = 2:([3..] `minus` composites)
   where
       composites = union [multiples p | p <- primes]

multiples :: Int -> [Int]
multiples n = map (n*) [n..]

(x:xs) `minus` (y:ys) | x < y = x:(xs `minus` (y:ys))
                      | x == y = xs `minus` ys
                      | x > y = (x:xs) `minus` ys

union = foldr merge []
   where
       merge (x:xs) ys = x:merge' xs ys
       merge' (x:xs) (y:ys) | x < y = x:merge' xs (y:ys)
                            | x == y = x:merge' xs ys
                            | x > y = y:merge' (x:xs) ys

isqrt = floor . sqrt . fromIntegral

solve n = maximum . filter (\x -> mod n x == 0) . takeWhile (< isqrt n) $ primes

main :: IO()
main = putStrLn $ show $ solve 600851475143
