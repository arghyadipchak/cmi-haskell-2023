-- Problem 1
vanadurga (1, 0, 10) == ([], [1 .. 10])
vanadurga (2, 10, 20) == ([2, 4 .. 20], [1, 3 .. 19])
vanadurga (3, 20, 20) == ([3, 6, 9, 12, 15, 18, 1, 5, 10, 14, 19, 4, 11, 17, 7, 16, 8, 2, 13, 20], [])
vanadurga (9, 11, 20) == ([9, 18, 7, 17, 8, 20, 12, 4, 19, 14, 11], [1, 2, 3, 5, 6, 10, 13, 15, 16])
vanadurga (11, 5, 10) == ([1, 3, 6, 10, 8], [2, 4, 5, 7, 9])

-- Problem 2
instance Eq Tree where
  (==) :: Tree -> Tree -> Bool
  (==) E E = True
  (==) (NE t1) (NE t2) = t1 == t2
  (==) _ _ = False

instance Eq T where
  (==) :: T -> T -> Bool
  (==) (Leaf x) (Leaf y) = x == y
  (==) (Branch (ml1, mr1) tl1 tr1) (Branch (ml2, mr2) tl2 tr2) = ml1 == ml2 && mr1 == mr2 && tl1 == tl2 && tr1 == tr2
  (==) _ _ = False

tInsert 5 E == NE (Leaf 5)
tInsert 4 (NE (Leaf 5)) == NE (Branch (4, 5) (Leaf 4) (Leaf 5))
tInsert 7 (NE (Leaf 5)) == NE (Branch (5, 7) (Leaf 5) (Leaf 7))
tInsert 5 (NE (Branch (5, 7) (Leaf 5) (Leaf 7))) == NE (Branch (5, 7) (Leaf 5) (Leaf 7))
tInsert 4 (NE (Branch (5, 7) (Leaf 5) (Leaf 7))) `elem` [NE (Branch (5, 7) (Branch (4, 5) (Leaf 4) (Leaf 5)) (Leaf 7)), NE (Branch (4, 7) (Leaf 4) (Branch (5, 7) (Leaf 5) (Leaf 7)))]
tInsert 6 (NE (Branch (5, 7) (Leaf 5) (Leaf 7))) == NE (Branch (5, 7) (Leaf 5) (Branch (6, 7) (Leaf 6) (Leaf 7)))
tInsert 9 (NE (Branch (5, 7) (Leaf 5) (Leaf 7))) `elem` [NE (Branch (5, 9) (Leaf 5) (Branch (7, 9) (Leaf 7) (Leaf 9))), NE (Branch (7, 9) (Branch (5, 7) (Leaf 5) (Leaf 7)) (Leaf 9))]
tInsert 8 (NE (Branch (5, 9) (Leaf 5) (Branch (7, 9) (Leaf 7) (Leaf 9)))) == NE (Branch (5, 9) (Leaf 5) (Branch (7, 9) (Leaf 7) (Branch (8, 9) (Leaf 8) (Leaf 9))))

tDelete 5 (NE (Leaf 5)) == E
tDelete 5 (NE (Leaf 7)) == NE (Leaf 7)
tDelete 7 (NE (Branch (5, 7) (Leaf 5) (Leaf 7))) == NE (Leaf 5)
tDelete 4 (NE (Branch (5, 7) (Branch (4, 5) (Leaf 4) (Leaf 5)) (Leaf 7))) == NE (Branch (5, 7) (Leaf 5) (Leaf 7))
tDelete 6 (NE (Branch (5, 7) (Leaf 5) (Branch (6, 7) (Leaf 6) (Leaf 7)))) == NE (Branch (5, 7) (Leaf 5) (Leaf 7))
tDelete 9 (NE (Branch (5, 9) (Leaf 5) (Branch (7, 9) (Leaf 7) (Leaf 9)))) == NE (Branch (5, 7) (Leaf 5) (Leaf 7))
tDelete 2 (NE (Branch (5, 7) (Branch (4, 5) (Branch (2, 4) (Leaf 2) (Leaf 4)) (Leaf 5)) (Leaf 7))) == NE (Branch (5, 7) (Branch (4, 5) (Leaf 4) (Leaf 5)) (Leaf 7))
tDelete 8 (NE (Branch (5, 9) (Leaf 5) (Branch (7, 9) (Leaf 7) (Branch (8, 9) (Leaf 8) (Leaf 9))))) == NE (Branch (5, 9) (Leaf 5) (Branch (7, 9) (Leaf 7) (Leaf 9)))

tSearch 5 E == False
tSearch 5 (NE (Leaf 5))
tSearch 5 (NE (Branch (5, 7) (Leaf 5) (Leaf 7)))
tSearch 4 (NE (Branch (5, 7) (Leaf 5) (Leaf 7))) == False
tSearch 4 (NE (Branch (5, 7) (Branch (4, 5) (Leaf 4) (Leaf 5)) (Leaf 7)))
tSearch 2 (NE (Branch (5, 9) (Leaf 5) (Branch (7, 9) (Leaf 7) (Leaf 9)))) == False
tSearch 2 (NE (Branch (5, 7) (Branch (4, 5) (Branch (2, 4) (Leaf 2) (Leaf 4)) (Leaf 5)) (Leaf 7)))
tSearch 8 (NE (Branch (5, 9) (Leaf 5) (Branch (7, 9) (Leaf 7) (Branch (8, 9) (Leaf 8) (Leaf 9)))))

tMax (Leaf 5) == 5
tMax (Branch (5, 7) (Leaf 5) (Leaf 7)) == 7
tMax (Branch (5, 7) (Branch (4, 5) (Leaf 4) (Leaf 5)) (Leaf 7)) == 7
tMax (Branch (5, 7) (Leaf 5) (Branch (6, 7) (Leaf 6) (Leaf 7))) == 7
tMax (Branch (5, 9) (Leaf 5) (Branch (7, 9) (Leaf 7) (Leaf 9))) == 9
tMax (Branch (5, 7) (Branch (4, 5) (Branch (2, 4) (Leaf 2) (Leaf 4)) (Leaf 5)) (Leaf 7)) == 7
tMax (Branch (5, 9) (Leaf 5) (Branch (7, 9) (Leaf 7) (Branch (8, 9) (Leaf 8) (Leaf 9)))) == 9
tMax (Branch (5, 9) (Branch (4, 5) (Leaf 4) (Leaf 5)) (Branch (7, 9) (Leaf 7) (Leaf 9))) == 9

checkInorder :: Tree -> Bool
checkInorder t = inorder t == inorder_ t
  where
    inorder_ :: Tree -> [Int]
    inorder_ E = []
    inorder_ (NE t) = inorder__ t
    inorder__ :: T -> [Int]
    inorder__ (Leaf x) = [x]
    inorder__ (Branch _ tl tr) = inorder__ tl ++ inorder__ tr

checkInorder E
checkInorder (NE (Leaf 5))
checkInorder (NE (Branch (5, 7) (Leaf 5) (Leaf 7)))
checkInorder (NE (Branch (5, 7) (Branch (4, 5) (Leaf 4) (Leaf 5)) (Leaf 7)))
checkInorder (NE (Branch (5, 7) (Leaf 5) (Branch (6, 7) (Leaf 6) (Leaf 7))))
checkInorder (NE (Branch (5, 9) (Leaf 5) (Branch (7, 9) (Leaf 7) (Leaf 9))))
checkInorder (NE (Branch (5, 7) (Branch (4, 5) (Branch (2, 4) (Leaf 2) (Leaf 4)) (Leaf 5)) (Leaf 7)))
checkInorder (NE (Branch (5, 9) (Leaf 5) (Branch (7, 9) (Leaf 7) (Branch (8, 9) (Leaf 8) (Leaf 9)))))
