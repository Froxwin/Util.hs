-- |
-- Module      :  Util
-- Description :  Miscleaneous functions
-- Copyright   :  (c) Froxwin 2023
-- License     : MIT
module Util where

import           Data.Char                      ( digitToInt
                                                , isSpace
                                                , toLower
                                                , toUpper
                                                )
import           Data.List                      ( group
                                                , intercalate
                                                , sort
                                                )
import           Data.Map                       ( Map
                                                , fromList
                                                )
import           Data.Text                      ( Text
                                                , pack
                                                , replace
                                                , splitOn
                                                , unpack
                                                )
import           Data.Tree                      ( Tree(Node)
                                                , drawTree
                                                )
import           Text.Printf

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
  check x | mod x 15 == 0 = "FizzBuzz"
          | mod x 3 == 0  = "Fizz"
          | mod x 5 == 0  = "Buzz"
          | otherwise     = show x

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

fib :: (Eq t, Num t, Num a) => t -> a
fib 1 = 1
fib 2 = 1
fib x = fib (x - 1) + fib (x - 2)

fibs :: (Eq a, Num a, Num b, Enum a) => a -> [b]
fibs n = map fib [1 .. n]

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
  x1 * x2 + y1 * y2 + z1 * z2

vmag :: Vector3 -> Double
vmag (Vector3 (x1, y1, z1)) = sqrt (x1 ^ 2 + y1 ^ 2 + z1 ^ 2)

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

reduceFrac :: (Show a, Integral a) => a -> a -> String
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

isLeapYear :: Integer -> Bool
isLeapYear x = x `mod` 4 == 0 && (x `mod` 100 /= 0 || x `mod` 400 == 0)

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

ageOn :: Planet -> Float -> Float
ageOn planet seconds = seconds / 3.15576e7 * scale
 where
  scale = case planet of
    Mercury -> 0.2408467
    Venus   -> 0.61519726
    Earth   -> 1.0
    Mars    -> 1.8808158
    Jupiter -> 11.862615
    Saturn  -> 29.447498
    Uranus  -> 84.016846
    Neptune -> 164.79132

unique :: Eq a => [a] -> [a]
unique []       = []
unique (x : xs) = x : unique (filter (x /=) xs)

isPangram :: String -> Bool
isPangram =
  (== 26) . length . unique . filter (`elem` ['a' .. 'z']) . map toLower

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

responseFor :: String -> String
responseFor "" = "Fine. Be that way!"
responseFor xs
  | all (`elem` [' ', '\t', '\n', '\r']) xs
  = "Fine. Be that way!"
  | last (trim xs) == '?' && all (`notElem` ['a' .. 'z']) xs && (/= 0)
    (length $ filter (`elem` ['A' .. 'Z']) xs)
  = "Calm down, I know what I'm doing!"
  | last (trim xs) == '?'
  = "Sure."
  | all (`notElem` ['a' .. 'z']) xs
    && (/= 0) (length $ filter (`elem` ['A' .. 'Z']) xs)
  = "Whoa, chill out!"
  | otherwise
  = "Whatever."

collatz :: Integer -> Maybe Integer
collatz n | n <= 0    = Nothing
          | otherwise = Just $ count 0 n
 where
  count x n | n == 1    = x
            | even n    = count (x + 1) $ n `div` 2
            | otherwise = count (x + 1) $ n * 3 + 1

toRNA :: String -> Either Char String
toRNA xs | all (`elem` "GCTA") xs = Right $ map getComplement xs
         | otherwise = Left $ head $ filter (`notElem` "GCTA") xs
 where
  getComplement x = case x of
    'G' -> 'C'
    'C' -> 'G'
    'T' -> 'A'
    'A' -> 'U'

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs
  | all (`elem` "GCTA") xs = Right $ fromList
    [(A, count 'A'), (C, count 'C'), (G, count 'G'), (T, count 'T')]
  | otherwise = Left $ error "Bruh"
  where count x = length $ filter (== x) xs

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum $ unique $ concat
  [ take (fromIntegral $ limit - 1) [x, x + x .. limit - 1]
  | x <- factors
  ]
 where
  unique []       = []
  unique (x : xs) = x : unique (filter (x /=) xs)

square :: Integer -> Maybe Integer
square n | n `elem` [1 .. 64] = Just $ 2 ^ (n - 1)
         | otherwise          = Nothing

total :: Integer
total = sum $ map ((\(Just x) -> x) . square) [1 .. 64]

abbreviate :: Text -> Text
abbreviate xs = pack $ map toUpper $ concat
  [ (if all (`notElem` ['a' .. 'z']) word || all (`elem` ['a' .. 'z']) word
      then [head word]
      else filter (`elem` ['A' .. 'Z']) word
    )
  | word <- words
  , word /= ""
  ]
 where
  lookup = ['-', '_']
  words  = map unpack $ splitOn (pack " ") $ pack
    [ if x `elem` lookup then ' ' else x | x <- unpack xs ]

discard :: (a -> Bool) -> [a] -> [a]
discard p xs = [ x | x <- xs, not $ p x ]

keep :: (a -> Bool) -> [a] -> [a]
keep p xs = [ x | x <- xs, p x ]

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss =
  [ x | x <- xss, sort (t x) == sort (t xs), t x /= t xs ]
  where t = map toLower

data Clock = Clock Int Int Int
  deriving Eq

instance Show Clock where
  show :: Clock -> String
  show (Clock h m s) = intercalate ":"
    $ map ((\x -> replicate (2 - length x) '0' ++ x) . show) [h, m, s]

fromInt :: Int -> Int -> Int -> Clock
fromInt h m s = Clock ((h + m `div` 60) `mod` 24)
                      ((m + s `div` 60) `mod` 60)
                      (s `mod` 60)

addDelta :: Int -> Int -> Int -> Clock -> Clock
addDelta dh dm ds (Clock h m s) = fromInt (h + dh) (m + dm) (s + ds)

splitN :: Int -> [a] -> [[a]]
splitN _ [] = []
splitN n ys =
  take n ys : splitN n (reverse $ take (length ys - n) $ reverse ys)

encode :: String -> String
encode xs = intercalate "\n" $ splitN m $ take0
  (map (\x -> x ++ replicate (n - length x) ' ') $ splitN n normalized)
  n
 where
  take0 x i = if i == 0 then [] else map (!! (n - i)) x ++ take0 x (i - 1)
  normalized =
    filter (`elem` ['a' .. 'z'] ++ ['0' .. '9']) $ map toLower xs
  (m, n) | t ^ 2 >= length normalized = (t, t)
         | t ^ 2 < length normalized  = (t, t + 1)
    where t = round $ sqrt $ fromIntegral $ length normalized

isValid :: String -> Bool
isValid n | length (filter (`elem` ['0' .. '9']) n) < 2 = False
          | all (`notElem` ' ' : ['0' .. '9']) n = False
          | otherwise                            = check n
 where
  check m = sumX (reverse $ filter (`elem` ['0' .. '9']) m) `mod` 10 == 0
  sumX xs =
    sum
      $  map digitToInt oddElems
      ++ map ((\x -> if x > 9 then x - 9 else x) . (* 2) . digitToInt)
             evenElems
   where
    oddElems  = map (xs !!) [0, 2 .. length xs - 1]
    evenElems = map (xs !!) [1, 3 .. length xs - 1]

nth :: Int -> Maybe Integer
nth n | n == 0    = Nothing
      | otherwise = Just $ (!! pred n) $ map fromIntegral $ sieve $ n * 20
 where
  sieve n = del 0 [2 .. n]
   where
    del i xs
      | i ^ 2 > last xs = xs
      | otherwise = del (i + 1)
      $ filter (\x -> x == xs !! i || x `mod` xs !! i /= 0) xs

-- splitN :: Int -> [a] -> [[a]]
-- splitN _ [] = []
-- splitN n ys =
--   take n ys : splitN n (reverse $ take (length ys - n) $ reverse ys)


-- number :: String -> Maybe String
number :: String -> Maybe String
number xs | length x == 11 && head x == '1' = Just x
          | -- tail x
            length x == 10                  = Just x
          | otherwise                       = Nothing
  where x = filter (`elem` ['0' .. '9']) xs

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n | n < 1  = Nothing
           | s == n = Just Perfect
           | s > n  = Just Abundant
           | s < n  = Just Deficient
  where s = sum $ init [ x | x <- [1 .. n], n `mod` x == 0 ]

pythagoreanTriplets'' n =
  [ (a, b, c)
  | (a, b, c) <-
    [ (a, b, c)
    | c <- [1 .. n]
    , b <- [1 .. c]
    , a <- [1 .. b]
    , a ^ 2 + b ^ 2 == c ^ 2
    ]
  , a + b + c == n
  ]

pallete =
  [ (46 , 52 , 64)
  , (59 , 66 , 82)
  , (67 , 76 , 94)
  , (76 , 86 , 106)
  , (216, 222, 233)
  , (229, 233, 240)
  , (236, 239, 244)
  , (143, 188, 187)
  , (136, 192, 208)
  , (129, 161, 193)
  , (94 , 129, 172)
  , (191, 97 , 106)
  , (208, 135, 112)
  , (235, 203, 139)
  , (163, 190, 140)
  , (180, 142, 173)
  ]


ctp =
  [ (245, 224, 220)
  , (242, 205, 205)
  , (245, 194, 231)
  , (203, 166, 247)
  , (243, 139, 168)
  , (235, 160, 172)
  , (250, 179, 135)
  , (249, 226, 175)
  , (166, 227, 161)
  , (148, 226, 213)
  , (137, 220, 235)
  , (116, 199, 236)
  , (137, 180, 250)
  , (180, 190, 254)
  , (205, 214, 244)
  , (186, 194, 222)
  , (166, 173, 200)
  , (147, 153, 178)
  , (127, 132, 156)
  , (108, 112, 134)
  , (88 , 91 , 112)
  , (69 , 71 , 90)
  , (49 , 50 , 68)
  , (30 , 30 , 46)
  , (24 , 24 , 37)
  , (17 , 17 , 27)
  ]


palletize :: Integral a => (a, a, a) -> [(a, a, a)] -> (a, a, a)
palletize (xr, xg, xb) = mini . map
  (\(yr, yg, yb) ->
    ( (yr, yg, yb)
    , sqrt
      (fromIntegral ((yr - xr) ^ 2 + (yg - xg) ^ 2 + (yb - xb) ^ 2) / 3)
    )
  )
 where
  mini xs = fst $ head $ filter ((==) min . snd) xs
    where min = minimum $ map snd xs
