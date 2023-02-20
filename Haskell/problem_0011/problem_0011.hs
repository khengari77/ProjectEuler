import Data.List

solve :: String -> Int
solve n = maximum . concat $ concat [f m, f $ transpose m, f $ diagonals m, f $ diagonals $ reverse m]
        where m = format_input n
              f n = map ( map (product) . everyN 4) n

format_input :: String -> [[Int]]
format_input n = map (map (read) . words) $ lines n

diagonals :: [[a]] -> [[a]]
diagonals = tail . go [] where
    -- it is critical for some applications that we start producing answers
    -- before inspecting es_
    go b es_ = [h | h:_ <- b] : case es_ of
        []   -> transpose ts
        e:es -> go (e:ts) es
        where ts = [t | _:t <- b]

everyN :: Int -> [a] -> [[a]]
everyN n l = [take n $ drop a l | a<-[0..length l - n] ]

main :: IO()
main = readFile "numbers.txt" >>= putStrLn . show . solve
