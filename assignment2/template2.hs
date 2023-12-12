import Data.Int (Int8)
import Data.List (foldl')
import Data.Ratio (denominator, numerator, (%))
import Data.Word (Word8)

-- Problem 1
-- Check if input list is an AP. Length <= 2 means it is a trivial AP.
isAP :: [Integer] -> Bool
isAP = undefined

-- Problem 2
-- targetSum t l checks if two elements at distinct positions of l sum to t
targetSum :: Integer -> [Integer] -> Bool
targetSum = undefined

-- Problem 3
-- increment a list of Word8-s representing an integer
incr :: [Word8] -> [Word8]
incr = undefined

-- Problem 4
-- Find Int8 value represented by list of 8 bits (in twos complement form), and find twos complement representation (consisting of 8 bits) of an Int8.
twosVal :: [Bool] -> Int8
twosVal = undefined

twosRep :: Int8 -> [Bool]
twosRep = undefined

-- Problem 5
-- Compute a finite continued fraction representing a rational, and the rational represented by a finite continued fraction.
invert :: Rational -> Rational
invert x = denominator x % numerator x

cf :: Rational -> [Integer]
cf = undefined

computeRat :: [Integer] -> Rational
computeRat = undefined

-- Problem 6
-- Compute approximations of sqrt 15, based on infinite continued fraction.
computeFrac :: Rational -> Double
computeFrac x = fromIntegral (numerator x) / fromIntegral (denominator x)

approxTarget :: Double -> Rational
approxTarget = undefined