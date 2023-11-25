-- Problem 1.
-------------
vanadurga :: (Int, Int, Int) -> ([Int], [Int])
vanadurga (m, r, n) = undefined

-- Problem 2.
-------------

data Tree = E | NE T

data T = Leaf Int | Branch (Int, Int) T T

instance Show Tree where
  show = drawTree

tMax :: T -> Int
tMax = undefined

inorder :: Tree -> [Int]
inorder = undefined

tSearch :: Int -> Tree -> Bool
tSearch = undefined

tInsert :: Int -> Tree -> Tree
tInsert = undefined

tDelete :: Int -> Tree -> Tree
tDelete = undefined

fromList :: [Int] -> Tree
fromList = foldr tInsert E

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