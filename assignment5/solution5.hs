-- Problem 1
vanadurga :: (Int, Int, Int) -> ([Int], [Int])
vanadurga (m, r, n) = undefined

-- Problem 2
data Tree = E | NE T

data T = Leaf Int | Branch (Int, Int) T T

instance Show Tree where
  show :: Tree -> String
  show = drawTree

drawTree :: Tree -> String
drawTree E = "|\n*"
drawTree (NE t) = unlines lines
  where
    (lines, _, _) = go t
    go :: T -> ([String], Int, Int)
    go (Leaf x) = ([spike, sx], p, q)
      where
        (sx, lenx, p, q) = (show x, length sx, lenx `div` 2, lenx - p - 1)
        spike = replicate p ' ' ++ "|" ++ replicate q ' '
    go (Branch _ tl tr) = (line1 : line2 : rest, m, n)
      where
        (linesl, ml, nl) = go tl
        (linesr, mr, nr) = go tr
        (m, n) = (ml + nl + 2, mr + nr + 2)
        line1 = replicate (ml + nl + 2) ' ' ++ "|" ++ replicate (mr + nr + 2) ' '
        line2 =
          replicate ml ' '
            ++ "+"
            ++ replicate (nl + 1) '-'
            ++ "+"
            ++ replicate (mr + 1) '-'
            ++ "+"
            ++ replicate nr ' '
        rest = join linesl linesr
        join xss yss
          | null xss = map (replicate (ml + nl + 4) ' ' ++) yss
          | null yss = map (++ replicate (mr + nr + 4) ' ') xss
          | otherwise = (xs ++ replicate 3 ' ' ++ ys) : join xss' yss'
          where
            (xs : xss') = xss
            (ys : yss') = yss

tMax :: T -> Int
tMax (Leaf x) = x
tMax (Branch (_, mr) _ _) = mr

inorder :: Tree -> [Int]
inorder E = []
inorder (NE t) = inorder_ t
  where
    inorder_ :: T -> [Int]
    inorder_ (Leaf x) = [x]
    inorder_ (Branch _ tl tr) = inorder_ tl ++ inorder_ tr

tSearch :: Int -> Tree -> Bool
tSearch _ E = False
tSearch x (NE t) = tSearch_ x t
  where
    tSearch_ :: Int -> T -> Bool
    tSearch_ x (Leaf y) = x == y
    tSearch_ x (Branch (ml, mr) tl tr)
      | x == ml || x == mr = True
      | x > mr = False
      | x > ml = tSearch_ x tr
      | otherwise = tSearch_ x tl

tInsert :: Int -> Tree -> Tree
tInsert x E = NE (Leaf x)
tInsert x (NE t) = NE (tInsert_ x t)
  where
    tInsert_ :: Int -> T -> T
    tInsert_ x ly@(Leaf y) = if x < y then Branch (x, y) lx ly else Branch (y, x) ly lx
      where
        lx = Leaf x
    tInsert_ x t@(Branch (ml, mr) tl tr)
      | x == ml || x == mr = t
      | x > ml = Branch (ml, max x mr) tl (tInsert_ x tr)
      | otherwise = Branch (max x ml, mr) (tInsert_ x tl) tr

tDelete :: Int -> Tree -> Tree
tDelete _ E = E
tDelete x (NE t) = unWrap $ tDelete_ x t
  where
    unWrap :: Maybe T -> Tree
    unWrap Nothing = E
    unWrap (Just t) = NE t
    combine :: Maybe T -> Maybe T -> T
    combine Nothing (Just t) = t
    combine (Just t) Nothing = t
    combine (Just tl@(Leaf a)) (Just tr@(Leaf b)) = Branch (a, b) tl tr
    combine (Just tl@(Leaf a)) (Just tr@(Branch (_, mr) _ _)) = Branch (a, mr) tl tr
    combine (Just tl@(Branch (_, ml) _ _)) (Just tr@(Leaf b)) = Branch (ml, b) tl tr
    combine (Just tl@(Branch (_, ml) _ _)) (Just tr@(Branch (_, mr) _ _)) = Branch (ml, mr) tl tr
    tDelete_ :: Int -> T -> Maybe T
    tDelete_ x t@(Leaf y) = if x == y then Nothing else Just t
    tDelete_ x t@(Branch (ml, mr) tl tr)
      | x > mr = Just t
      | x > ml = Just $ combine (Just tl) (tDelete_ x tr)
      | otherwise = Just $ combine (tDelete_ x tl) (Just tr)

fromList :: [Int] -> Tree
fromList = foldr tInsert E
