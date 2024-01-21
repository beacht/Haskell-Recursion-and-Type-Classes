isPrime :: Integer -> Bool
isPrime n
    | n <= 1    = False
    | otherwise = not $ any (\x -> n `mod` x == 0) [2..intSqrt n]

intSqrt :: Integer -> Integer
intSqrt = floor . sqrt . fromIntegral

primes :: [Integer]
primes = filter isPrime [2..]

problem1 :: Int -> [Integer]
problem1 n = take n $ everyOther primes

everyOther :: [a] -> [a]
everyOther [] = []
everyOther [x] = [x]
everyOther (x:_:xs) = x : everyOther xs

fibList :: Integer -> [Integer]
fibList limit = fibList' 1 1
  where
    fibList' a b
        | a > limit = []
        | otherwise = a : fibList' b (a + b)

fibWith3 :: [Integer] -> [Integer]
fibWith3 = filter (\x -> x `mod` 10 == 3)

problem2 :: Integer -> [Integer]
problem2 n = fibWith3 $ fibList n

multipleOf5 :: Integer -> Bool
multipleOf5 n = n `mod` 5 == 0

threeFactors :: Integer -> Bool
threeFactors n = (length [x | x <- [1..n], n `mod` x == 0] == 3)

problem3 :: Integer -> [Integer]
problem3 n = filter (\x -> multipleOf5 x || threeFactors x) [1..n]

main :: IO ()
main = do
    let n1 = 10
    let result1 = problem1 n1
    putStrLn $ "#1: First " ++ show n1 ++ " every other prime numbers: " ++ show result1
    let n2 = 100
    let result2 = problem2 n2
    putStrLn $ "#2: Fibonacci numbers with a three as the right-most digit and <= " ++ show n2 ++ ": " ++ show result2
    let n3 = 100
    let result3 = problem3 n3
    putStrLn $ "#3: Numbers 1 to " ++ show n3 ++ " that are multiples of 5 or have 3 factors: " ++ show result3
