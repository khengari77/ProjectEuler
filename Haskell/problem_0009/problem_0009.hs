-- a^2 + b^2 = c^2
-- a + b + c = n
-- (a+b)^2 =  c^2 + 2ab
-- (n - c)^2 = c^2 + 2ab
-- n^2 - 2nc = 2ab
-- 2(ab + nc) = n^2
-- n^2/2 - nc = ab
-- n(n/2-c) = ab
-- n/2 - c = ab/n
-- c = (n/2 - ab/n)
solve n= [p*c | a <- range, b <- range, 
                    let p = a*b, let c = div n 2 - div p n,
                    mod p n == 0, a+b+c == n, a < b]
    where range = [1..n] 

main :: IO()
main = putStrLn $ show $ solve 1000

