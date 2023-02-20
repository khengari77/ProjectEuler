solve :: Int -> Int 
solve n = sum $ filter (\x -> mod x 3 == 0 || mod x 5 == 0) [1..n]

main :: IO()
main = do
	putStrLn $ show $ solve $ (1000 -1)
