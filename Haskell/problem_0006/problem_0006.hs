solve n = ((^2) . sum $ range) -  (sum . map (^2) $ range)
      where range = [1..n]
main :: IO()
main = putStrLn $ show $ solve 100
