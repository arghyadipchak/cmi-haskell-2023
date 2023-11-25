import Data.Int (Int8)
import Data.List (foldl')
import Data.Ratio (denominator, numerator, (%))
import Data.Word (Word8)

-- Problem 1
isAP :: [Integer] -> Bool
isAP [] = True
isAP [x] = True
isAP [x, y] = True
isAP (x : y : xs) = isAP_ (y - x) (y : xs)
  where
    isAP_ :: Integer -> [Integer] -> Bool
    isAP_ z [x] = True
    isAP_ z (x : y : xs)
      | y - x == z = isAP_ z (y : xs)
      | otherwise = False

-- Problem 2
targetSum :: Integer -> [Integer] -> Bool
targetSum _ [] = False
targetSum _ [x] = False
targetSum t (x : xs) = targetSum_ xs || targetSum t xs
  where
    targetSum_ :: [Integer] -> Bool
    targetSum_ (y : ys) = (x + y == t) || targetSum_ ys

-- Problem 3
incr :: [Word8] -> [Word8]
incr [] = [1]
incr l = map fromInteger (if t then 1 : l' else l')
  where
    (l', t) = incr_ (map toInteger l)
    incr_ :: [Integer] -> ([Integer], Bool)
    incr_ [255] = ([0], True)
    incr_ [x] = ([x + 1], False)
    incr_ (x : xs)
      | z = if x == 255 then (0 : y, True) else ((x + 1) : y, False)
      | otherwise = (x : y, False)
      where
        (y, z) = incr_ xs

-- Problem 4
twosVal :: [Bool] -> Int8
twosVal (x : xs) = if x then if v then -u else -z else z
  where
    (z, u, v) = twosVal_ xs
    twosVal_ :: [Bool] -> (Int8, Int8, Bool)
    twosVal_ [y] = (if y then 1 else 0, 2, x && not y)
    twosVal_ (y : ys) = (a + if not c && x then (if y then 0 else b) else (if y then b else 0), 2 * b, c && not y)
      where
        (a, b, c) = twosVal_ ys

twosRep :: Int8 -> [Bool]
twosRep x = t : z
  where
    t = x < 0
    (z, _, _) = twosRep_ 7
    twosRep_ :: Int -> ([Bool], Int8, Bool)
    twosRep_ 1 = ([f], d, t && not f)
      where
        (d, e) = divMod (if t then -x else x) 2
        f = e == 1
    twosRep_ n = (((not c && t) /= f) : a, d, c && not f)
      where
        (a, b, c) = twosRep_ $ n - 1
        (d, e) = divMod b 2
        f = e == 1

-- Problem 5
invert :: Rational -> Rational
invert x = denominator x % numerator x

cf :: Rational -> [Integer]
cf 0 = [0]
cf r = n : if r == n % 1 then [] else cf $ invert (r - n % 1)
  where
    n = div (numerator r) (denominator r)

computeRat :: [Integer] -> Rational
computeRat [] = 0
computeRat [x] = x % 1
computeRat (x : xs) = (x * denominator y + numerator y) % denominator y
  where
    y = invert $ computeRat xs

-- Problem 6
computeFrac :: Rational -> Double
computeFrac x = fromIntegral (numerator x) / fromIntegral (denominator x)

approxTarget :: Double -> Rational
approxTarget eps = approxTarget_ [3]
  where
    sq15 = sqrt 15
    approxTarget_ l = if abs (sq15 - computeFrac cfl) < eps then cfl else approxTarget_ $ 3 : 1 : 6 : tail l
      where
        cfl = computeRat l
