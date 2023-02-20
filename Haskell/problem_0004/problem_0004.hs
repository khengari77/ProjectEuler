is_palindrome :: String -> Bool
is_palindrome x = x == reverse x

solve :: Int 
solve = maximum .
        filter (\x -> is_palindrome $ show x ) 
        $ [x*y | x <- [100..1000], y <- [100..1000]]

main :: IO()
main = putStrLn $ show $ solve
