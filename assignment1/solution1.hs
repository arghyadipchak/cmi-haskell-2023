-- Problem 1
choose :: Integer -> Integer -> Integer
choose _ 0 = 1
choose n r = n * div (choose (n - 1) (r - 1)) r

-- Problem 2
primeSum :: Int -> Integer
primeSum n = sum [x | x <- [2 .. fromIntegral n], isPrime x]
  where
    isPrime n
      | n <= 1 = False
      | n == 2 = True
      | even n = False
      | otherwise = all (\x -> mod n x /= 0) [3, 5 .. limit]
      where
        limit = floor (sqrt (fromIntegral n))

-- Problem 3
leftRotate :: Integer -> Integer
leftRotate n
  | n < -9 = -leftRotate (-n)
  | n < 10 = n
  | otherwise = 100 * div m 10 + 10 * mod n 10 + mod m 10
  where
    m = leftRotate (div n 10)

-- Problem 4
cLength :: Integer -> Maybe Int
cLength = cLength_ 0
  where
    cLength_ x 1 = Just x
    cLength_ 9999 _ = Nothing
    cLength_ x n = cLength_ (x + 1) (if even n then div n 2 else 3 * n + 1)

-- Problem 5
frac :: (Int, Int, Int, Int, Int) -> Maybe (Int, Int, Int)
frac (s, x, y, z, w)
  | x < 0 || y < 0 || y > 9 || z < 0 || z > 9 || w < 0 || w > 9 = Nothing
  | s == 1 = Just (y, z, w)
  | s == -1 = Just (f, d, b)
  | otherwise = Nothing
  where
    a = 1000 - (100 * y + 10 * z + w)
    (c, b) = divMod a 10
    (e, d) = divMod c 10
    f = mod e 10

-- Problem 6
ilog :: Integer -> Integer -> Integer
ilog n b = ilog_ 0 1
  where
    ilog_ k c = if bk > n then k else ilog_ (k + 1) bk
      where
        bk = c * b

-- Problem 7
cos1 :: Integer -> Double -> Double
cos1 _ 0 = 1.0
cos1 n x = cos1_ 1 1 1
  where
    cos1_ m l s = if m == n then s else cos1_ (m + 1) z (s + z)
      where
        z = (-1 * l * x * x) / fromInteger ((2 * m - 1) * (2 * m))

cos2 :: Double -> Double -> Double
cos2 _ 0 = 1.0
cos2 e x = cos2_ 1 1 1
  where
    cosx = cos x
    cos2_ m l s = if abs (s - cosx) <= e then s else cos2_ (m + 1) z (s + z)
      where
        z = (-1 * l * x * x) / fromInteger ((2 * m - 1) * (2 * m))
