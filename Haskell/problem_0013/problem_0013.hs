sumFromText :: String -> String
sumFromText = show .  sum . map(read) . lines 

solve :: Int -> String -> Int
solve n m = read . take n $ sumFromText m 

main :: IO()
main = readFile "numbers.txt"  >>= putStrLn . show . solve 10

