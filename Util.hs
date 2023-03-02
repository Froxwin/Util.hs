-- Module      :  Util
-- Description :  Miscleaneous functions
-- Copyright   :  (c) Froxwin 2023

module Util where

import           Data.List                      ( group
                                                , sort
                                                )
import           Data.Tree                      ( Tree(Node)
                                                , drawTree
                                                )

-- | The 'primes' function returns prime numbers up to the provided number
primes :: Integral a => a -> [a]
primes n = [ x | x <- [1 .. n], factors x == [1, x] ]
  where factors n = [ x | x <- [1 .. n], n `mod` x == 0 ]

-- | The 'primeFactors' function returns the prime factors of a number.
--
--  Implemented using list comprehensions.
primeFactors :: Integral a => a -> [a]
primeFactors n = [ x | x <- factors n, factors x == [1, x] ]
  where factors n = [ x | x <- [1 .. n], n `mod` x == 0 ]

-- | The 'primeFactors'' function returns the prime factors of a number.
--
--  Implemented using the filter function
primeFactors' :: Integral a => a -> [a]
primeFactors' m = filter ((== 2) . length . factors) $ factors m
  where factors n = filter ((== 0) . mod n) [1 .. n]

-- | The 'FizzBuzz' function returns a list of strings, where each string
--  is either :-
--
--  "Fizz" if the number is a multiple of 3
--
--  "Buzz" if the number is a multiple of 5
--
--  "FizzBuzz" if the number is a multiple of both
--
--  @The number itself@ if it's a multiple of neither.
fizzBuzz :: [String]
fizzBuzz = map check [1 .. 100]
 where
  check x | isDiv 15  = "FizzBuzz"
          | isDiv 3   = "Fizz"
          | isDiv 5   = "Buzz"
          | otherwise = show x
    where isDiv = (== 0) . mod x

-- | The 'pythagoreanTriplets' function returns 'x' number of pythagorean
--  triplets.
pythagoreanTriplets :: (Num c, Eq c, Enum c) => Int -> [(c, c, c)]
pythagoreanTriplets x = take
  x
  [ (a, b, c)
  | c <- [1 ..]
  , b <- [1 .. c]
  , a <- [1 .. b]
  , a ^ 2 + b ^ 2 == c ^ 2
  ]

-- | The 'fibonacci' function returns 'x' number of fibonacci numbers.
--
--  Implemented using an iterative helper function.
fibonacci :: (Eq t, Num t, Num a) => t -> [a]
fibonacci x = (x - 2) ∈ [0, 1]
 where
  x ∈ xs | x == 0    = xs
           | otherwise = (x - 1) ∈ (xs ++ [last xs + last (init xs)])

-- | The 'fibonacci'' function returns 'x' number of fibonacci numbers.
--
--  Implemented using an recursive helper function.
fibonacci' :: Num a => Int -> [a]
fibonacci' n = take n (0 <~> 1) where a <~> b = a : b <~> (a + b)

-- | The 'factorial' function returns the factorial of a number.
--
--  Implemented using the product function.
factorial :: (Num a, Enum a) => a -> a
factorial n = product [1 .. n]

-- | The 'factorial'' function returns the factorial of a number.
--
--  Implemented using recursion.
factorial' :: (Integral a) => a -> a
factorial' x | x == 0    = 1
             | otherwise = x * factorial (x - 1)

-- | The 'quicksort' function returns a sorted list. The list is sorted
--  using the quicksort algorithm.
quickSort :: Ord a => [a] -> [a]
quickSort xs = case xs of
  []     -> []
  x : xs -> ts ++ [x] ++ bs
   where
    ts = quickSort $ filter (<= x) xs
    bs = quickSort $ filter (> x) xs

-- | The 'bubbleSort' function returns a sorted list. The list is sorted
--  using the bubble sort algorithm.
bubbleSort :: Ord a => [a] -> [a]
bubbleSort xs =
  let sort' xs i | i == length xs = xs
                 | otherwise      = sort' (sort xs) (i + 1)
         where
          sort (x : y : xs) | x > y     = y : sort (x : xs)
                            | otherwise = x : sort (y : xs)
          sort x = x
  in  sort' xs 0

-- | The 'mergeSort' function returns a sorted list. The list is sorted
--  using the merge sort algorithm.
mergeSort :: Ord a => [a] -> [a]
mergeSort xs | length xs < 2 = xs
             | otherwise     = mergeSort left |+| mergeSort right
 where
  (left, right) = splitAt (length xs `div` 2) xs
  [] |+| ys = ys
  xs |+| [] = xs
  (x : xs) |+| (y : ys) | x < y     = x : xs |+| (y : ys)
                        | otherwise = y : (x : xs) |+| ys

-- | The 'factorTree' function generates a table that somewhat looks like a
--  factor tree.
factorTree :: (Show t, Integral t) => t -> IO ()
factorTree n = do
  putStrLn $ concat [show n, " = ", show f1, " x ", show f2]
  if f1 /= 1 then factorTree f2 else putStr ""
 where
  (f1, f2) : _ =
    [ (x, n `div` x) | x <- [2 .. n - 1], n `mod` x == 0 ] ++ [(1, n)]

-- | The 'highestCommonFactor' function returns the highest common factor of
--  two numbers.
highestCommonFactor :: (Integral a, Show a) => a -> a -> String
highestCommonFactor n m | null $ cfs n m = "Coprime"
                        | otherwise      = show $ maximum $ cfs n m
 where
  cfs a b = [ x | x <- [2 .. max a b], a `mod` x == 0, b `mod` x == 0 ]
  cfs' a b = filter ((== 0) . mod (mod a b)) [2 .. max a b]

-- | The 'leastCommonMultiple' function returns the least common multiple of
--  two numbers.
leastCommonMultiple :: Integral a => a -> a -> a
leastCommonMultiple n m = product $ cfs n m ++ ufs n m
 where
  cfs a b = [ x | x <- pfs $ a `min` b, x `elem` pfs (a `max` b) ]
  ufs a b = [ x | x <- pfs a ++ pfs b, x `notElem` cfs a b ]
  pfs n | null $ fs n = [n]
        | otherwise   = head (fs n) : pfs (last $ fs n)
    where fs n = [ x | x <- [2 .. n - 1], n `mod` x == 0 ]

newtype Vector3 = Vector3 (Double , Double, Double)
  deriving Eq

instance Num Vector3 where
  (+) :: Vector3 -> Vector3 -> Vector3
  (Vector3 (x1, y1, z1)) + (Vector3 (x2, y2, z2)) =
    Vector3 (x1 + x2, y1 + y2, z1 + z2)

  (*) :: Vector3 -> Vector3 -> Vector3
  Vector3 (x1, y1, z1) * Vector3 (x2, y2, z2) =
    Vector3 (y1 * z2 - z1 * y2, z1 * x2 - x1 * z2, x1 * y2 - y1 * x2)

  negate :: Vector3 -> Vector3
  negate (Vector3 (x, y, z)) = Vector3 (-x, -y, -z)

  abs :: Vector3 -> Vector3
  abs (Vector3 (x, y, z)) = Vector3 (abs x, abs y, abs z)

  signum :: Vector3 -> Vector3
  signum (Vector3 (x, y, z)) = Vector3 (signum x, signum y, signum z)

  fromInteger :: Integer -> Vector3
  fromInteger l = Vector3 (d, d, d) where d = fromInteger l

instance Show Vector3 where
  show :: Vector3 -> String
  show (Vector3 (x, y, z)) =
    concat ["(", show x, ", ", show y, ", ", show z, ")"]

instance Ord Vector3 where
  (<=) :: Vector3 -> Vector3 -> Bool
  a <= b = vmag a <= vmag b

cross :: Vector3 -> Vector3 -> Vector3
cross = (*)

dot :: Vector3 -> Vector3 -> Double
Vector3 (x1, y1, z1) `dot` Vector3 (x2, y2, z2) =
  (x1 * x2) + (y1 * y2) + (z1 * z2)

vmag :: Vector3 -> Double
vmag (Vector3 (x1, y1, z1)) = sqrt ((x1 ^ 2) + (y1 ^ 2) + (z1 ^ 2))

(*:) :: Vector3 -> Double -> Vector3
Vector3 (x1, y1, z1) *: k = Vector3 (x1 * k, y1 * k, z1 * k)

cartesianProd :: [a] -> [b] -> [(a, b)]
a `cartesianProd` b = [ (x, y) | x <- a, y <- b ]

intersection :: (Foldable t, Eq a) => [a] -> t a -> [a]
x `intersection` y = [ a | a <- x, a `elem` y ]

union :: Ord b => [b] -> [b] -> [b]
x `union` y = map head . group . sort $ x ++ y

disjoiuntUnion :: Ord b => [b] -> [b] -> [b]
x `disjoiuntUnion` y =
  filter (\a -> a `notElem` (x `intersection` y)) (x `union` y)

reduceFrac :: (Show a, Integral a) => a -> a -> [Char]
reduceFrac n d = concat [show $ div n k, " / ", show $ div d k]
 where
  k = last $ [ x | x <- [1 .. max n d], n `mod` x == 0, d `mod` x == 0 ]

loop :: (Eq t, Num t, Monad m) => t -> m a -> m a
loop i f
  | i == 1 = f
  | otherwise = do
    f
    loop (i - 1) f

loopExposed :: (Eq t, Num t, Monad m) => t -> (t -> m a) -> m a
loopExposed i f
  | i == 1 = f i
  | otherwise = do
    f i
    loopExposed (i - 1) f

bubble :: Ord a => [a] -> [a]
bubble xs = iterate sort xs !! length xs
 where
  sort (x : y : xs) | x > y     = y : sort (x : xs)
                    | otherwise = x : sort (y : xs)
  sort x = x

factTree :: (Show a, Integral a) => a -> IO ()
factTree a = putStr $ drawTree $ show <$> f a
 where
  f n = Node n [Node f1 [], if f1 /= 1 then f f2 else Node f2 []]
   where
    (f1, f2) : _ =
      [ (x, n `div` x) | x <- [2 .. n - 1], n `mod` x == 0 ] ++ [(1, n)]

putTriangle :: Integer -> IO ()
putTriangle =
  putStr
    . concatMap ((++ "\n") . concatMap ((++ " ") . show))
    . reverse
    . triangle
 where
  triangle x = row x : if x > 0 then triangle $ x - 1 else []
   where
    row n = (: if n > 0 then row $ n - 1 else [])
      $ div (product [1 .. x]) (product [1 .. n] * product [1 .. x - n])

deviation :: Fractional a => [a] -> a
deviation xs = sum (map (abs . subtract mean) xs) / n
 where
  mean = sum xs / n
  n    = fromIntegral $ length xs


