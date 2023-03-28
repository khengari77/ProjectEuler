--import qualified Data.Map as M

--type Point = (Int,Int)

--routes :: Point -> Point -> Int
--routes start@(row,col) end@(endRow, endCol)  
-- | row > endRow || col > endCol = 0
-- | start == end = 1
-- | otherwise = routes (row+1,col) end  + routes (row,col+1) end

-- This function assumes always starting from (0,0) and ending at (n,n)
-- Basically it assumes square grids
routes :: Int -> Integer
routes n = iterate (next) [1,1] !! (2*n - 1) !! (n) 
 where
 next xs = zipWith (+) (0:xs) (xs ++ [0])

main :: IO ()
main = putStrLn $ show . routes $ 20
