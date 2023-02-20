import Data.Function
import Data.List
import Data.Ord
 
collatzLength :: (Int -> Int) -> Int -> Int
collatzLength f 1 = 1
collatzLength f n | even n = (f $  div n 2) + 1
                  | odd n = (f $ 3*n +1) + 1

memoize :: (Int -> a) -> (Int -> a)
memoize f = (map f [0 ..] !!)

collatzLengthMemo :: Int -> Int
collatzLengthMemo = fix(memoize . collatzLength)

main :: IO()
main = undefined
