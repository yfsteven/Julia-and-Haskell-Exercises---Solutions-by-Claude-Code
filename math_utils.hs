module MathUtils where

import Data.List (nub, group, subsequences)
import Data.Time (Day, diffDays)

-- | Volume of a sphere with radius r.
sphereVolume :: Double -> Double
sphereVolume r = (4.0 / 3.0) * pi * r ^ 3

-- | Real roots of a*x^2 + b*x + c = 0. Returns [] when discriminant < 0.
quadraticEquation :: Double -> Double -> Double -> [Double]
quadraticEquation a b c
  | disc > 0  = [(-b + sqrtDisc) / (2 * a), (-b - sqrtDisc) / (2 * a)]
  | disc == 0 = [-b / (2 * a)]
  | otherwise = []
  where
    disc     = b * b - 4 * a * c
    sqrtDisc = sqrt disc

-- | Count zeros in a list.
numberOfZeros :: (Num a, Eq a) => [a] -> Int
numberOfZeros = length . filter (== 0)

-- | Print the first n rows of Pascal's triangle.
drawPascal :: Int -> IO ()
drawPascal n = mapM_ (putStrLn . unwords . map show) (take n rows)
  where
    rows        = iterate nextRow [1]
    nextRow row = zipWith (+) (0 : row) (row ++ [0])

-- | Unique positive integer whose square has form 1_2_3_4_5_6_7_8_9_0.
-- Searches n divisible by 10 (required since square ends in 0) in [10^9, ~3.16*10^9].
euler :: Integer
euler = head [n | n <- [1000000000, 1000000010 .. 3162277660],
                  let sq = n * n, isMatch sq]
  where
    isMatch sq =
      let s = show sq
      in  length s == 19
          && s !! 0  == '1' && s !! 2  == '2' && s !! 4  == '3'
          && s !! 6  == '4' && s !! 8  == '5' && s !! 10 == '6'
          && s !! 12 == '7' && s !! 14 == '8' && s !! 16 == '9'
          && s !! 18 == '0'

-- | Days from date1 to date2 inclusive. Dates must be in "YYYY-MM-DD" format.
days :: String -> String -> Integer
days date1 date2 = diffDays d2 d1 + 1
  where
    d1 = read date1 :: Day
    d2 = read date2 :: Day

-- | Remove consecutive duplicates.
removeConsecutiveDups :: Eq a => [a] -> [a]
removeConsecutiveDups = map head . group

-- | Remove all duplicates, preserving first-occurrence order.
removeDups :: Eq a => [a] -> [a]
removeDups = nub

-- | Replicate each element of lst n times.
replicateList :: [a] -> Int -> [a]
replicateList lst n = concatMap (replicate n) lst

-- | Split lst into (first n elements, remainder).
splitList :: [a] -> Int -> ([a], [a])
splitList lst n = splitAt n lst

-- | Min, max, and median of lst. Median uses quickselect for expected O(n) time.
minMaxMedian :: (Ord a, Fractional a) => [a] -> (a, a, a)
minMaxMedian lst = (minimum lst, maximum lst, med)
  where
    n   = length lst
    med
      | odd n     = quickSelect lst (n `div` 2)
      | otherwise =
          ( quickSelect lst (n `div` 2 - 1)
          + quickSelect lst (n `div` 2)
          ) / 2

quickSelect :: Ord a => [a] -> Int -> a
quickSelect xs k
  | length xs == 1 = head xs
  | k < lLen       = quickSelect lows k
  | k < lLen + pLen = pivot
  | otherwise      = quickSelect highs (k - lLen - pLen)
  where
    pivot = xs !! (length xs `div` 2)
    lows  = filter (< pivot) xs
    highs = filter (> pivot) xs
    lLen  = length lows
    pLen  = length xs - lLen - length highs

-- | Binomial coefficient n choose k.
-- Computes C(n,1), C(n,2), ... C(n,k) iteratively so each step is an integer,
-- avoiding the large intermediate products of the naive formula.
bc :: Integer -> Integer -> Integer
bc n k
  | k < 0 || k > n = 0
  | k == 0 || k == n = 1
  | otherwise = foldl (\acc i -> acc * (n - i + 1) `div` i) 1 [1 .. k']
  where
    k' = min k (n - k)

-- | All n-element subsets of s (as a list of lists).
subsets :: [a] -> Int -> [[a]]
subsets s n = filter ((== n) . length) (subsequences s)

-- | Contiguous subarray of arr with the largest sum (Kadane's algorithm).
maxSubarray :: (Num a, Ord a) => [a] -> [a]
maxSubarray []  = []
maxSubarray arr = take (bestEnd - bestStart + 1) (drop bestStart arr)
  where
    -- state: (currentSum, currentStart, bestSum, bestStart, bestEnd)
    initial = (head arr, 0, head arr, 0, 0)
    step (curSum, curStart, bestSum, bestStart, bestEnd) (i, x)
      | curSum + x >= x =
          let ns = curSum + x
              (bs, bss, bse)
                | ns > bestSum = (ns, curStart, i)
                | otherwise    = (bestSum, bestStart, bestEnd)
          in (ns, curStart, bs, bss, bse)
      | otherwise =
          let (bs, bss, bse)
                | x > bestSum = (x, i, i)
                | otherwise   = (bestSum, bestStart, bestEnd)
          in (x, i, bs, bss, bse)
    (_, _, _, bestStart, bestEnd) =
      foldl step initial (tail (zip [0 ..] arr))
