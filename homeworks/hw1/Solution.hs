module Solution where

import Data.List (nub, transpose)

-- Exercise 1: Goldbach Pairs
-- Helper functions from Exercise 3 are defined first as they are a dependency.

-- Sieve for generating primes up to n (from Exercise 3)
sieve :: [Int] -> [Int]
sieve [] = []
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

primesTo :: Int -> [Int]
primesTo n | n < 2 = []
           | otherwise = sieve [2..n]

goldbachPairs :: Int -> [(Int, Int)]
goldbachPairs n = [(p, q) | p <- ps, let q = n - p, p <= q, q `elem` ps]
  where
    ps = primesTo n

-- Exercise 2: Coprime Pairs
coprimePairs :: [Int] -> [(Int, Int)]
coprimePairs xs = [(x, y) | x <- unique_xs, y <- unique_xs, x < y, gcd x y == 1]
    where unique_xs = nub xs

-- Exercise 3: Sieve of Eratosthenes
-- The functions `sieve` and `primesTo` are defined above Exercise 1.
-- The `isPrime` part of the question is defined here.
isPrime :: Int -> Bool
isPrime k | k <= 1 = False
          | otherwise = k `elem` primesTo k


-- Exercise 4: Matrix Multiplication
matMul :: [[Int]] -> [[Int]] -> [[Int]]
matMul a b
    | null a || null b || null (head a) || null (head b) = []
    | length (head a) /= length b = error "Matrix dimensions are not compatible for multiplication"
    | otherwise = [[ sum [ (a !! i !! k) * (b !! k !! j) | k <- [0 .. p - 1] ] | j <- [0 .. n - 1] ] | i <- [0 .. m - 1] ]
  where
    m = length a
    p = length b
    n = length (head b)

-- Exercise 5: Permutations
permutations :: Eq a => Int -> [a] -> [[a]]
permutations 0 _ = [[]]
permutations k xs
    | k < 0     = []
    | otherwise = [y:p | y <- xs, p <- permutations (k-1) (filter (/= y) xs)]


-- Exercise 6: Lazy/Eager Evaluation, seq, and Bang Patterns

-- (a) Merge sorted lists
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x < y     = x : merge xs (y:ys)
    | x > y     = y : merge (x:xs) ys
    | otherwise = x : merge xs ys -- x == y

-- (b) Hamming numbers
hamming :: [Integer]
hamming = 1 : merge (map (*2) hamming) (merge (map (*3) hamming) (map (*5) hamming))

-- Exercise 7: Integer Power with Bang Patterns
power :: Int -> Int -> Int
power base exp
    | exp < 0   = error "power: negative exponent"
    | otherwise = go exp 1
  where
    go 0 acc = acc
    go n !acc = go (n-1) (acc * base)

-- Exercise 8: Running Maximum

-- Using `seq`
listMaxSeq :: [Int] -> Int
listMaxSeq [] = error "listMaxSeq: empty list"
listMaxSeq (x:xs) = go xs x
  where
    go [] currentMax = currentMax
    go (y:ys) currentMax =
        let newMax = max y currentMax
        in newMax `seq` go ys newMax

-- Using bang patterns
listMaxBang :: [Int] -> Int
listMaxBang [] = error "listMaxBang: empty list"
listMaxBang (x:xs) = go xs x
  where
    go [] currentMax = currentMax
    go (y:ys) !currentMax = go ys (max y currentMax)

-- Exercise 9: Infinite Prime Stream

-- (a) Infinite list of primes
primes :: [Int]
primes = sieve [2..] -- Reusing sieve from Ex 3

-- (b) isPrime using the infinite stream
isPrime' :: Int -> Bool
isPrime' n
    | n <= 1 = False
    | otherwise = go n primes
  where
    go k (p:ps)
        | p * p > k    = True
        | k `mod` p == 0 = False
        | otherwise    = go k ps

-- Exercise 10: Strict Accumulation and Space Leaks

-- (a) Naive mean (with space leak)
mean :: [Double] -> Double
mean xs = go 0 0 xs
  where
    go sum' count [] = sum' / count
    go sum' count (z:zs) = go (sum' + z) (count + 1) zs

-- (b) Strict mean (space leak fixed)
mean' :: [Double] -> Double
mean' xs = go 0 0 xs
  where
    go !sum' !count [] = sum' / count
    go !sum' !count (z:zs) = go (sum' + z) (count + 1) zs

-- (c) Strict mean and variance in a single pass
meanVariance :: [Double] -> (Double, Double)
meanVariance xs = go 0 0 0 xs
  where
    go !s !s2 !n [] = (mu, s2 / n - mu * mu)
      where mu = s / n
    go !s !s2 !n (z:zs) = go (s + z) (s2 + z*z) (n + 1) zs