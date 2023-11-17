-- Problem 1
choose 57646 0 == 1
choose 57646 57646 == 1
choose 30 15 == 155117520
choose 4 2 == 6
choose 0 0 == 1

-- Problem 2
primeSum (-1000) == 0
primeSum 0 == 0
primeSum 1 == 0
primeSum 998 == 76127
primeSum 997 == 76127
primeSum 996 == 75130

-- Problem 3
leftRotate 99900000 == 99000009
leftRotate 9900000999 == 9000009999
leftRotate 900000999900099000009999 == 9999000990000099999
leftRotate 9 == 9
leftRotate 0 == 0

-- Problem 4
cLength 47 == Just 104
cLength 997 == Just 49
cLength 1994 == Just 50
cLength 9998 == Just 91
isNothing $ cLength (2^10000)

-- Problem 5
isNothing $ frac (1,0,1,-3,0)
isNothing $ frac (-1,120,1,13,0)
isNothing $ frac (2,0,1,3,5)
isNothing $ frac (1,-6,2,8,5)
frac (-1,89,0,9,1) == Just (9,0,9)

-- Problem 6
ilog 1010 1011 == 0
ilog 1010 1010 == 1
ilog 3 2 == 1
ilog 1234987653 4 == 15
ilog 1234987653 481 == 3

-- Problem 7
abs (cos1 25 pi - cos pi) < 1.0e-8
abs (cos1 100 (pi/2) - cos (pi/2)) < 1.0e-8
abs (cos2 1.0e-5 pi - cos pi) < 1.0e-5
abs (cos2 1.0e-6 (5*pi/7) - cos (5*pi/7)) < 1.0e-6
abs (cos2 1.0e-10 (5*pi/7) - cos (5*pi/7)) < 1.0e-10