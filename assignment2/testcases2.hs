-- Problem 1
isAP []
isAP [-13]
isAP [-10, -143]
isAP [16, -2, -20, -38, -56, -74, -92]
not $ isAP [2, 20, 37, 56, 74, 92]

-- Problem 2
not $ targetSum 20 [1 .. 10]
not $ targetSum 20 [10]
targetSum 20 [-13, 33]
targetSum 20 ([-1, -2, -3, -4, -5] ++ [10] ++ [6 .. 10])
targetSum 20 ([-1, -2, -3, -4, -5] ++ [33] ++ [6 .. 9] ++ [-13])

-- Problem 3
incr [] == [1]
incr [0] == [1]
incr [255, 255, 255, 254, 255, 255, 255, 255] == [255, 255, 255, 255, 0, 0, 0, 0]
incr [255, 255, 255, 255, 255, 255, 255, 255] == [1, 0, 0, 0, 0, 0, 0, 0, 0]
incr [255] == [1, 0]

-- Problem 4
twosVal [False, False, False, False, False, False, False, False] == 0
twosVal [False, False, False, False, False, False, False, True] == 1
twosVal [False, True, True, False, True, False, True, False] == 106
twosVal [True, False, False, True, True, False, True, False] == -102
twosVal [True, True, True, False, True, False, True, False] == -22

twosRep 0 == [False, False, False, False, False, False, False, False]
twosRep 1 == [False, False, False, False, False, False, False, True]
twosRep 106 == [False, True, True, False, True, False, True, False]
twosRep (-102) == [True, False, False, True, True, False, True, False]
twosRep (-22) == [True, True, True, False, True, False, True, False]

-- Problem 5
computeRat [33, 957, 997, 977, 763] == 23472285165790 % 711258846951
computeRat (replicate 100 1) == 573147844013817084101 % 354224848179261915075
computeRat [25, 24 .. 1] == 33973805081805192501772225 % 1356695125590833848455793
computeRat [12, 3, 43, 2564, 3, 332243] == 4097221463699 % 332276225380
computeRat [1 .. 100] == 210628326606030925509504995396115751215082237173770949560136689651065020446811215535403721603460152269102594085067245222090841983806373539433329769060736986251 % 146971108555054591263734471078160507352050045572198741030186489794108431298591736983812285328810821610957278981960240168856854819813194627616076643581220741550

cf (23472285165790 % 711258846951) == [33, 957, 997, 977, 763]
cf (573147844013817084101 % 354224848179261915075) == replicate 98 1 ++ [2]
cf (33973805081805192501772225 % 1356695125590833848455793) == [25, 24 .. 3] ++ [3]
cf (4097221463699 % 332276225380) == [12, 3, 43, 2564, 3, 332243]
cf (210628326606030925509504995396115751215082237173770949560136689651065020446811215535403721603460152269102594085067245222090841983806373539433329769060736986251 % 146971108555054591263734471078160507352050045572198741030186489794108431298591736983812285328810821610957278981960240168856854819813194627616076643581220741550) == [1 .. 100]

-- Problem 6
checkApproxTarget :: Double -> Bool
checkApproxTarget e = abs (sq15 - computeFrac (approxTarget e)) < e
  where
    sq15 = sqrt 15
    computeFrac :: Rational -> Double
    computeFrac x = fromIntegral (numerator x) / fromIntegral (denominator x)

checkApproxTarget 1
checkApproxTarget 10
checkApproxTarget 0.0001
checkApproxTarget 0.0000000001
checkApproxTarget 0.00000000000000001