import Data.Char (digitToInt)

sumOfDigits :: Integer -> Int 
sumOfDigits n = sum . (map digitToInt) . show $ n

main :: IO()
main = putStrLn $ show . sumOfDigits $ (2^1000)
