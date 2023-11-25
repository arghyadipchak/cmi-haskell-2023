import Data.Array (listArray, (!))

-- Problem 1
hamming :: (Eq a) => [a] -> [a] -> Maybe Int
hamming xs ys = if length xs == length ys then Just $ hamming_ xs ys else Nothing
  where
    hamming_ :: (Eq a) => [a] -> [a] -> Int
    hamming_ [] [] = 0
    hamming_ (x : xs) (y : ys) = (if x == y then 0 else 1) + hamming_ xs ys

-- Problem 2
parity :: Int -> Bool
parity 0 = True
parity n = if r == 0 then pq else not pq
  where
    (q, r) = divMod n 2
    pq = parity q

-- Problem 3
mss :: [Integer] -> (Integer, [Integer])
mss xs = kadane xs (0, []) (0, [])
  where
    kadane :: [Integer] -> (Integer, [Integer]) -> (Integer, [Integer]) -> (Integer, [Integer])
    kadane [] _ z = z
    kadane (x : xs) (cSum, cSeg) mS = kadane xs nS $ max nS mS
      where
        nS = max (cSum + x ^ 3, cSeg ++ [x]) (0, [])

-- Problem 4
squares :: [(Int, Int)]
squares = [(x, y) | x <- [0 .. 7], y <- [0 .. 7]]

knightMove :: (Int, Int) -> Int -> [(Int, Int)]
knightMove (x, y) n = undefined

-- Problem 5
editDistance :: String -> String -> (Int, String)
editDistance as bs = arr ! (0, 0)
  where
    (la, lb) = (length as, length bs)
    aArr = listArray (0, la) as
    bArr = listArray (0, lb) bs
    arr = listArray ((0, 0), (la, lb)) [ed i j | i <- [0 .. la], j <- [0 .. lb]]
    ed :: Int -> Int -> (Int, String)
    ed i j
      | i == la = (2 * (lb - j), replicate (lb - j) 'i')
      | j == lb = (2 * (la - i), replicate (la - i) 'd')
      | aArr ! i == bArr ! j = (di1j1, '-' : si1j1)
      | otherwise = min (di1j1 + 1, 'm' : si1j1) $ min (di1j + 2, 'd' : si1j) (dij1 + 2, 'i' : sij1)
      where
        (di1j, si1j) = arr ! (i + 1, j)
        (dij1, sij1) = arr ! (i, j + 1)
        (di1j1, si1j1) = arr ! (i + 1, j + 1)
