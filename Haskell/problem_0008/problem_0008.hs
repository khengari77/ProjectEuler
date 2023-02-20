everyN :: Int -> [a] -> [[a]]
everyN n l = [take n $ drop a l | a<-[0..length l - n] ]

digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]


solve m n =  maximum . map (product) . everyN m . concat . map (digs . read) $  lines n 

main :: IO()
main = readFile "number.txt"  >>= putStrLn . show . solve 13
