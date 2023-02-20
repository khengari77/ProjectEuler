triangular :: Int -> Int
triangular n = sum [1..n]

isqrt = floor . sqrt . fromIntegral

factors :: Int -> [Int]
factors n = concat [[x, div n x] | x <- [1 .. isqrt n], mod n x == 0]

solve n = head [b | a <- [1..], let b = triangular a, let nFactors =  length . factors $ b, nFactors > n]
main :: IO()
main = putStrLn $ show $ solve 500
