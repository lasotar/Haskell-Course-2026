{-# LANGUAGE BangPatterns #-}

-- 1) Goldbach pairs
goldbachPairs :: Int -> [(Int, Int)]
goldbachPairs n
  | n < 4 || odd n = []
  | otherwise =
      [ (p, q)
      | p <- [2 .. n `div` 2]
      , let q = n - p
      , isPrime p
      , isPrime q
      ]

-- 2) Coprime Pairs
coprimePairs :: [Int] -> [(Int, Int)]
coprimePairs xs =
  [ (x, y)
  | x <- uniq
  , y <- uniq
  , x < y
  , gcd x y == 1
  ]
  where
    uniq = unique xs

unique :: Eq a => [a] -> [a]
unique [] = []
unique (x : xs) = x : unique [y | y <- xs, y /= x]

-- 3) Sieve of Eratosthenes
sieve :: [Int] -> [Int]
sieve [] = []
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

primesTo :: Int -> [Int]
primesTo n = sieve [2..n]

isPrime :: Int -> Bool
isPrime n
    | n < 2 = False
    | otherwise = n `elem` primesTo n

-- 4) Matrix Multiplication
matMul :: [[Int]] -> [[Int]] -> [[Int]]
matMul a b
  | null a || null b = []
  | length (head a) /= length b = error "matMul: incompatible dimensions"
  | otherwise =
      [ [ sum [a !! i !! k * b !! k !! j | k <- [0 .. p - 1]]
          | j <- [0 .. n - 1]
        ]
        | i <- [0 .. m - 1]
      ]
  where
    m = length a
    p = length (head a)
    n = length (head b)

-- 5) k-Permutations
permutations :: Int -> [a] -> [[a]]
permutations k _
  | k < 0 = []
permutations 0 _ = [[]]
permutations _ [] = []
permutations k xs =
  [ y : ys
  | (y, rest) <- pickOne xs
  , ys <- permutations (k - 1) rest
  ]

pickOne :: [a] -> [(a, [a])]
pickOne [] = []
pickOne (x : xs) = (x, xs) : [(y, x : ys) | (y, ys) <- pickOne xs]

-- 6) Hamming Numbers
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys)
  | x < y = x : merge xs (y : ys)
  | x > y = y : merge (x : xs) ys
  | otherwise = x : merge xs ys

hamming :: [Integer]
hamming =
  1 : merge
    (map (2 *) hamming)
    (merge (map (3 *) hamming) (map (5 *) hamming))

-- 7) Integer Power with Bang Patterns
power :: Int -> Int -> Int
power _ e
  | e < 0 = error "power: negative exponent not supported"
power b e = go e 1
  where
    go 0 !acc = acc
    go n !acc = go (n - 1) (acc * b)


-- 8) Running Maximum: seq vs Bang Patterns
listMaxSeq :: [Int] -> Int
listMaxSeq [] = error "listMaxSeq: empty list"
listMaxSeq (x : xs) = go x xs
  where
    go acc [] = acc
    go acc (y : ys) =
      let acc' = max acc y
       in acc' `seq` go acc' ys

listMaxBang :: [Int] -> Int
listMaxBang [] = error "listMaxBang: empty list"
listMaxBang (x : xs) = go x xs
  where
    go !acc [] = acc
    go !acc (y : ys) = go (max acc y) ys


-- 9) Infinite Prime Stream
primes :: [Int]
primes = sieve [2 ..]

isPrimeUnbounded :: Int -> Bool
isPrimeUnbounded n
  | n < 2 = False
  | otherwise = all (\p -> n `mod` p /= 0) (takeWhile (\p -> p * p <= n) primes)


-- 10a) Mean without strictness
meanLazy :: [Double] -> Double
meanLazy [] = error "meanLazy: empty list"
meanLazy xs = total / fromIntegral count
  where
    (total, count) = go xs (0, 0 :: Int)
    go [] (s, n) = (s, n)
    go (y : ys) (s, n) = go ys (s + y, n + 1)

-- 10b) Strict mean with bang patterns.
meanStrict :: [Double] -> Double
meanStrict [] = error "meanStrict: empty list"
meanStrict xs = total / fromIntegral count
  where
    (total, count) = go xs 0 0
    go [] !s !n = (s, n)
    go (y : ys) !s !n = go ys (s + y) (n + 1)

mean :: [Double] -> Double
mean = meanStrict

-- 10c) Mean and variance in one strict pass.
meanVariance :: [Double] -> (Double, Double)
meanVariance [] = error "meanVariance: empty list"
meanVariance xs = (mu, sigma2)
  where
    (sumX, sumX2, count) = go xs 0 0 0
    mu = sumX / fromIntegral count
    sigma2 = sumX2 / fromIntegral count - mu * mu

    go [] !s !s2 !n = (s, s2, n)
    go (y : ys) !s !s2 !n = go ys (s + y) (s2 + y * y) (n + 1)

main :: IO ()
main = putStrLn "Homework 01 solutions loaded."
