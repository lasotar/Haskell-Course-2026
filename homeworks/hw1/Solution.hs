{-# LANGUAGE BangPatterns #-}

-- | Homework 1 solutions with very detailed explanations.
--   The goal is to explain Haskell syntax and intent line-by-line.
--
--   NOTE ABOUT BangPatterns:
--   - BangPatterns is a language extension.
--   - It lets us write patterns like !acc.
--   - That "!" forces the value to be evaluated immediately (strictness),
--     which can prevent large chains of unevaluated expressions ("thunks").

-- ============================================================================
-- PART 0: SMALL HASKELL SYNTAX PRIMER
-- ============================================================================
--
-- 1) Guards:
--    f x
--      | condition1 = result1
--      | condition2 = result2
--      | otherwise  = result3
--    This is like a chain of if/else statements.
--
-- 2) List comprehensions:
--    [ expression | generator1, generator2, condition ]
--    Example: [x*x | x <- [1..5], even x]  ==>  [4,16]
--
-- 3) Recursion:
--    Haskell uses recursion instead of loops.
--    The base case is usually the empty list [].
--
-- 4) Pattern matching:
--    (x:xs) matches a list with head x and tail xs.
--    [] matches an empty list.

-- ============================================================================
-- 1) GOLDBACH PAIRS
-- ============================================================================

-- goldbachPairs :: Int -> [(Int, Int)]
-- Read as:
--   Given an integer n, return a list of pairs (p, q)
--   such that p and q are primes and p + q = n.
--
-- The result type [(Int, Int)] is a list of pairs of integers.
goldbachPairs :: Int -> [(Int, Int)]
goldbachPairs n
  -- Guards are like if/else chains.
  -- This guard says: if n is too small OR n is odd, return [].
  | n < 4 || odd n = []
  -- Otherwise, build the result list with a list comprehension.
  | otherwise =
      [ (p, q)
      -- p ranges over the integers from 2 to n/2.
      -- We only go to n/2 to avoid duplicate pairs:
      -- if (p, q) is a solution, then (q, p) is the same solution.
      | p <- [2 .. n `div` 2]
      -- let introduces a local binding inside the comprehension.
      -- q is the value that makes p + q = n.
      , let q = n - p
      -- Both p and q must be prime.
      , isPrime p
      , isPrime q
      ]

-- EXAMPLE:
-- goldbachPairs 10
-- p can be 2,3,4,5
-- (2,8) not prime, (3,7) is valid, (5,5) is valid
-- result: [(3,7),(5,5)]

-- ============================================================================
-- 2) COPRIME PAIRS
-- ============================================================================

-- coprimePairs :: [Int] -> [(Int, Int)]
-- Read as: given a list of integers, return all pairs (x, y)
-- where x and y are distinct elements and gcd x y == 1.
coprimePairs :: [Int] -> [(Int, Int)]
coprimePairs xs =
  [ (x, y)
  -- x and y range over unique elements of xs.
  | x <- uniq
  , y <- uniq
  -- x < y avoids duplicates and prevents (x, x).
  , x < y
  -- gcd x y == 1 means they are coprime.
  , gcd x y == 1
  ]
  where
    -- uniq is a local name for the list with duplicates removed.
    uniq = unique xs

-- unique :: Eq a => [a] -> [a]
-- Remove duplicates while keeping the first occurrence of each element.
unique :: Eq a => [a] -> [a]
unique [] =
  -- Base case: an empty list has no duplicates.
  []
unique (x : xs) =
  -- x is the head, xs is the tail.
  -- We keep x, then remove any elements equal to x from xs.
  x : unique [y | y <- xs, y /= x]

-- EXAMPLE:
-- unique [1,2,1,3,2]  ==>  [1,2,3]

-- ============================================================================
-- 3) SIEVE OF ERATOSTHENES
-- ============================================================================

-- sieve :: [Int] -> [Int]
-- Read as:
--   Given a list of integers (assumed increasing),
--   remove non-primes by filtering multiples of each prime.
sieve :: [Int] -> [Int]
sieve [] =
  -- Base case: no numbers left.
  []
sieve (p : xs) =
  -- p is the first number in the list, so it is prime.
  -- We keep p, then filter out all multiples of p from xs.
  p : sieve [x | x <- xs, x `mod` p /= 0]

-- primesTo :: Int -> [Int]
-- Read as:
--   Generate all primes up to n.
primesTo :: Int -> [Int]
primesTo n =
  -- We start from [2..n] and apply the sieve.
  sieve [2 .. n]

-- isPrime :: Int -> Bool
-- Read as:
--   Check if n is in the list of primes up to n.
isPrime :: Int -> Bool
isPrime n
  | n < 2 = False
  | otherwise = n `elem` primesTo n

-- EXAMPLE:
-- primesTo 10  ==>  [2,3,5,7]
-- isPrime 11   ==>  True

-- ============================================================================
-- 4) MATRIX MULTIPLICATION
-- ============================================================================

-- matMul :: [[Int]] -> [[Int]] -> [[Int]]
-- Read as:
--   Multiply two integer matrices a and b.
--   a is a list of rows. Each row is a list of Int.
matMul :: [[Int]] -> [[Int]] -> [[Int]]
matMul a b
  -- If either matrix is empty, the result is empty.
  | null a || null b = []
  -- If dimensions don't match, throw an error.
  -- a has size m x p, b must have size p x n.
  | length (head a) /= length b = error "matMul: incompatible dimensions"
  | otherwise =
      [ [ sum [a !! i !! k * b !! k !! j | k <- [0 .. p - 1]]
          | j <- [0 .. n - 1]
        ]
        | i <- [0 .. m - 1]
      ]
  where
    -- m = number of rows in a.
    m = length a
    -- p = number of columns in a (and number of rows in b).
    p = length (head a)
    -- n = number of columns in b.
    n = length (head b)

-- EXAMPLE:
-- a = [[1,2],[3,4]]  (2x2)
-- b = [[5,6],[7,8]]  (2x2)
-- matMul a b = [[19,22],[43,50]]

-- ============================================================================
-- 5) K-PERMUTATIONS
-- ============================================================================

-- permutations :: Int -> [a] -> [[a]]
-- Read as:
--   Generate all length-k permutations of a list (without repetition).
permutations :: Int -> [a] -> [[a]]
permutations k _
  -- If k is negative, there are no permutations.
  | k < 0 = []
permutations 0 _ =
  -- There is exactly one permutation of length 0: the empty list.
  [[]]
permutations _ [] =
  -- If the list is empty but k > 0, there are no permutations.
  []
permutations k xs =
  [ y : ys
  -- Choose one element y and the remaining elements rest.
  | (y, rest) <- pickOne xs
  -- Recursively permute the rest to get a list of length k-1.
  , ys <- permutations (k - 1) rest
  ]

-- pickOne :: [a] -> [(a, [a])]
-- Read as:
--   For each element, return that element and the list of remaining elements.
pickOne :: [a] -> [(a, [a])]
pickOne [] = []
pickOne (x : xs) =
  -- First: choose x, rest is xs.
  (x, xs) :
  -- Then: choose each element from xs, and keep x in the remainder.
  [(y, x : ys) | (y, ys) <- pickOne xs]

-- EXAMPLE:
-- permutations 2 [1,2,3] ==> [[1,2],[1,3],[2,1],[2,3],[3,1],[3,2]]

-- ============================================================================
-- 6) HAMMING NUMBERS
-- ============================================================================

-- merge :: Ord a => [a] -> [a] -> [a]
-- Read as:
--   Merge two sorted lists into one sorted list, removing duplicates.
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys)
  | x < y = x : merge xs (y : ys)
  | x > y = y : merge (x : xs) ys
  | otherwise = x : merge xs ys

-- hamming :: [Integer]
-- Read as:
--   Infinite list of Hamming numbers (numbers of the form 2^i * 3^j * 5^k).
-- This relies on Haskell's lazy evaluation to generate values on demand.
hamming :: [Integer]
hamming =
  1 : merge
    (map (2 *) hamming)
    (merge (map (3 *) hamming) (map (5 *) hamming))

-- EXAMPLE:
-- take 10 hamming ==> [1,2,3,4,5,6,8,9,10,12]

-- ============================================================================
-- 7) INTEGER POWER WITH BangPatterns
-- ============================================================================

-- power :: Int -> Int -> Int
-- Read as:
--   Compute b^e (b to the power e), for non-negative exponent e.
power :: Int -> Int -> Int
power _ e
  | e < 0 = error "power: negative exponent not supported"
power b e = go e 1
  where
    -- go is a helper with an accumulator acc.
    -- !acc forces acc to be evaluated at each step (strict recursion).
    go 0 !acc = acc
    go n !acc = go (n - 1) (acc * b)

-- ============================================================================
-- 8) RUNNING MAXIMUM: seq vs BangPatterns
-- ============================================================================

-- listMaxSeq uses explicit seq to force evaluation of acc'.
listMaxSeq :: [Int] -> Int
listMaxSeq [] = error "listMaxSeq: empty list"
listMaxSeq (x : xs) = go x xs
  where
    go acc [] = acc
    go acc (y : ys) =
      let acc' = max acc y
       in acc' `seq` go acc' ys

-- listMaxBang uses BangPatterns to force strictness.
listMaxBang :: [Int] -> Int
listMaxBang [] = error "listMaxBang: empty list"
listMaxBang (x : xs) = go x xs
  where
    go !acc [] = acc
    go !acc (y : ys) = go (max acc y) ys

-- ============================================================================
-- 9) INFINITE PRIME STREAM
-- ============================================================================

-- primes :: [Int]
-- Infinite list of primes using the sieve on an infinite list.
primes :: [Int]
primes = sieve [2 ..]

-- isPrimeUnbounded checks primality by dividing only by primes <= sqrt(n).
isPrimeUnbounded :: Int -> Bool
isPrimeUnbounded n
  | n < 2 = False
  | otherwise = all (\p -> n `mod` p /= 0) (takeWhile (\p -> p * p <= n) primes)

-- ============================================================================
-- 10) MEANS AND VARIANCE
-- ============================================================================

-- meanLazy computes average without forcing strictness.
-- This can build up deferred computations (thunks).
meanLazy :: [Double] -> Double
meanLazy [] = error "meanLazy: empty list"
meanLazy xs = total / fromIntegral count
  where
    (total, count) = go xs (0, 0 :: Int)
    go [] (s, n) = (s, n)
    go (y : ys) (s, n) = go ys (s + y, n + 1)

-- meanStrict forces strict evaluation to avoid space leaks.
meanStrict :: [Double] -> Double
meanStrict [] = error "meanStrict: empty list"
meanStrict xs = total / fromIntegral count
  where
    (total, count) = go xs 0 0
    go [] !s !n = (s, n)
    go (y : ys) !s !n = go ys (s + y) (n + 1)

-- mean uses the strict version by default.
mean :: [Double] -> Double
mean = meanStrict

-- meanVariance computes mean and variance in one strict pass.
meanVariance :: [Double] -> (Double, Double)
meanVariance [] = error "meanVariance: empty list"
meanVariance xs = (mu, sigma2)
  where
    (sumX, sumX2, count) = go xs 0 0 0
    mu = sumX / fromIntegral count
    sigma2 = sumX2 / fromIntegral count - mu * mu

    go [] !s !s2 !n = (s, s2, n)
    go (y : ys) !s !s2 !n = go ys (s + y) (s2 + y * y) (n + 1)

-- Simple entry point for manual execution.
main :: IO ()
main = putStrLn "Homework 01 solutions loaded."
