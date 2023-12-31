import Data.Array (listArray, (!))

-- Problem 1
hamming "" "" == Just 0
isNothing $ hamming "lorem ipsum dolor set" ""
hamming (replicate 984 'A') (replicate 984 'A') == Just 0
hamming "Jordan was an all-time great" "LeBron was even greater goat" == Just 19
hamming "WXAAJJALZRLGLSXDPUPURULYINBFGXURUHJELLHCMOAPXWUAWKWIPOUXPPKZHEIKEOAGGCETJEJMOUZIKOINCMRKNIRCQYAOJXSBOPRHVKXEEDAOHZHGHRHOVRREWQBFNISIOCWUOIJLXYVODZZFTCWFNIHUMPCJZDKFDWWLALRUCXZZBDCTZURDHLMPOVAMMVBBRJXEEEYSFKQFPVQYXYOQBAQQAFWFFLRRUGWYPWCZPGDAALRJAEELTPQAPLYORBNOYQENLMLREAQRFVQZAJJMYEKBPRBDNHBHKETOEFCWPRINOXRPLCBTTZGQEWBARZZQDFBWXVVCBYDKODNVFSWLQUZBNNZDCNOIGOHXPHGXBQZZDUOBGRIYEMKMJXKUFMRCSKDNMCRSNIXOPRTYQNBXZERPBQGULYNWVUIITVIDZHLGVSQXVZEGZKHZRWBQTXUZTUCIDVOUIVTRBCJBAUEGUNYHCOSOEXXGOTVBGNNQMOPYMTELMLHDMSNLWHGEVEGQTAPVAUNNYVZIJUWUORMRFZSSJCWRQPHENRBGSRVTVZFHGAWJIQ" "IYEHXKFSFYCESPLFTDTUQCOKBBWKFZAEGWKWPWTKQIYHEBSHJLBIEKHBGWOXSHUMVWKHZMOAHFZLGOEGNIWYDCZCAFPIANYHNKKEKTKCUJPCTWACHZHXPHCKCWKMCIMUUIXBXYALAQCJFPXORKFDBNZFIMPAQQCDLUGNVNKYUQYBDTPYWEDMVNIRGVKXZCGPOMTAULLDNKDWOIOUKMVCIVSAFUKDGFVGMFEYUFRMNSJQTHLDBPYLMRERRLCNIDVFHOTFZLZGCHPESIRKZAWTWDPTFJEFMMECQYNFJWSYOTJAQKKEOKLAENTTUGVDVGZUKUTUSKSFSZSWXQMHYXNAJSQHAJXXXIVVCTRDWNLWKLNXFWZDADIPZSLEUXQAJGXSNWTRIHQMKPHRRMANUOGATQSUVIOCXYIYSFHDJHOKGRUUXHOGFAYBQFTZEVNFSZUTMBQYAGLLHJINGCCMLAHRLYOIXVSJVNPSOVAJYYLRBCTGQBQKIGAZESLOOITEPKCNKXSPTUBHNMZOPQPBHAFINXMSRXFJKSFUJAPBOXZFXYOFQRPMZJNTRJ" == Just 541

-- Problem 2
parity 0
not $ parity 1
not $ parity (2 ^ 25 - 1)
parity (2 ^ 26 - 1)
not $ parity (2 ^ 50)

-- Problem 3
knightMove (4, 3) 0 == [(4, 3)]
knightMove (7, 0) 1 == [(5, 1), (6, 2)]
knightMove (7, 7) 4 == [(0, 2), (0, 4), (0, 6), (1, 1), (1, 3), (1, 5), (1, 7), (2, 0), (2, 2), (2, 4), (2, 6), (3, 1), (3, 3), (3, 5), (3, 7), (4, 0), (4, 2), (4, 4), (4, 6), (5, 1), (5, 3), (5, 5), (5, 7), (6, 0), (6, 2), (6, 4), (6, 6), (7, 1), (7, 3), (7, 5), (7, 7)]
knightMove (3, 6) 163 == [(0, 0), (0, 2), (0, 4), (0, 6), (1, 1), (1, 3), (1, 5), (1, 7), (2, 0), (2, 2), (2, 4), (2, 6), (3, 1), (3, 3), (3, 5), (3, 7), (4, 0), (4, 2), (4, 4), (4, 6), (5, 1), (5, 3), (5, 5), (5, 7), (6, 0), (6, 2), (6, 4), (6, 6), (7, 1), (7, 3), (7, 5), (7, 7)]
knightMove (5, 2) 191 == [(0, 0), (0, 2), (0, 4), (0, 6), (1, 1), (1, 3), (1, 5), (1, 7), (2, 0), (2, 2), (2, 4), (2, 6), (3, 1), (3, 3), (3, 5), (3, 7), (4, 0), (4, 2), (4, 4), (4, 6), (5, 1), (5, 3), (5, 5), (5, 7), (6, 0), (6, 2), (6, 4), (6, 6), (7, 1), (7, 3), (7, 5), (7, 7)]

-- Problem 4
checkmss :: [Integer] -> Bool
checkmss l_in = kadane l_in 0 0 == s_out && sum (map (^ 3) l_out) == s_out && segOf l_out l_in
  where
    (s_out, l_out) = mss l_in
    kadane :: [Integer] -> Integer -> Integer -> Integer
    kadane [] _ maxSum = maxSum
    kadane (x : xs) currentSum maxSum = kadane xs newSum $ max maxSum newSum
      where
        newSum = max 0 (currentSum + x ^ 3)
    segOf :: [Integer] -> [Integer] -> Bool
    segOf [] _ = True
    segOf _ [] = False
    segOf (x : xs) (y : ys) = segOf (if x == y then xs else x : xs) ys

checkmss [4, 1, -5, -5, -1, 0, 12]
checkmss [-5, 14, -3, 11, 1, 19, 0, 14]
checkmss [-5, 13, 0, 21, 9, -6, 12, 21, 5]
checkmss [20, -2, -2, 1, -7, -2, 2, -6]
checkmss [22, 0, -4, 24, -5, 14, 22, 11]

-- Problem 5
checkED :: String -> String -> Bool
checkED as bs = checkEdit d s (0, 0)
  where
    (d, s) = editDistance as bs
    (la, lb) = (length as, length bs)
    aArr = listArray (0, la) as
    bArr = listArray (0, lb) bs
    arr = listArray ((0, 0), (la, lb)) [ed i j | i <- [0 .. la], j <- [0 .. lb]]
    ed :: Int -> Int -> Int
    ed i j
      | i == la = 2 * (lb - j)
      | j == lb = 2 * (la - i)
      | aArr ! i == bArr ! j = arr ! (i + 1, j + 1)
      | otherwise = min (arr ! (i + 1, j + 1) + 1) $ min (arr ! (i + 1, j) + 2) (arr ! (i, j + 1) + 2)
    checkEdit :: Int -> String -> (Int, Int) -> Bool
    checkEdit n "" (i, j) = n == 0 && i == la && j == lb
    checkEdit n (x : xs) (i, j)
      | i > la || j > lb || n /= arr ! (i, j) = False
      | x == '-' = checkEdit n xs (i + 1, j + 1)
      | x == 'm' = checkEdit (n - 1) xs (i + 1, j + 1)
      | x == 'i' = checkEdit (n - 2) xs (i, j + 1)
      | x == 'd' = checkEdit (n - 2) xs (i + 1, j)
      | otherwise = False

checkED "" ""
checkED "lorem ipsum dolor set" ""
checkED (replicate 984 'A') (replicate 984 'A')
checkED "Jordan was an all-time great" "LeBron is even greater"
checkED "WXAAJJALZRLGLSXDPUPURULYINBFGXURUHJELLHCMOAPXWUAWKWIPOUXPPKZHEIKEOAGGCETJEJMOUZIKOINCMRKNIRCQYAOJXSBOPRHVKXEEDAOHZHGHRHOVRREWQBFNISIOCWUOIJLXYVODZZFTCWFNIHUMPCJZDKFDWWLALRUCXZZBDCTZURDHLMPOVAMMVBBRJXEEEYSFKQFPVQYXYOQBAQQAFWFFLRRUGWYPWCZPGDAALRJAEELTPQAPLYORBNOYQENLMLREAQRFVQZAJJMYEKBPRBDNHBHKETOEFCWPRINOXRPLCBTTZGQEWBARZZQDFBWXVVCBYDKODNVFSWLQUZBNNZDCNOIGOHXPHGXBQZZDUOBGRIYEMKMJXKUFMRCSKDNMCRSNIXOPRTYQNBXZERPBQGULYNWVUIITVIDZHLGVSQXVZEGZKHZRWBQTXUZTUCIDVOUIVTRBCJBAUEGUNYHCOSOEXXGOTVBGNNQMOPYMTELMLHDMSNLWHGEVEGQTAPVAUNNYVZIJUWUORMRFZSSJCWRQPHENRBGSRVTVZFHGAWJIQ" "IYEHXKFSFYCESPLFTDTUQCOKBBWKFZAEGWKWPWTKQIYHEBSHJLBIEKHBGWOXSHUMVWKHZMOAHFZLGOEGNIWYDCZCAFPIANYHNKKEKTKCUJPCTWACHZHXPHCKCWKMCIMUUIXBXYALAQCJFPXORKFDBNZFIMPAQQCDLUGNVNKYUQYBDTPYWEDMVNIRGVKXZCGPOMTAULLDNKDWOIOUKMVCIVSAFUKDGFVGMFEYUFRMNSJQTHLDBPYLMRERRLCNIDVFHOTFZLZGCHPESIRKZAWTWDPTFJEFMMECQYNFJWSYOTJAQKKEOKLAENTTUGVDVGZUKUTUSKSFSZSWXQMHYXNAJSQHAJXXXIVVCTRDWNLWKLNXFWZDADIPZSLEUXQAJGXSNWTRIHQMKPHRRMANUOGATQSUVIOCXYIYSFHDJHOKGRUUXHOGFAYBQFTZEVNFSZUTMBQYAGLLHJINGCCMLAHRLYOIXVSJVNPSOVAJYYLRBCTGQBQKIGAZESLOOITEPKCNKXSPTUBHNMZOPQPBHAFINXMSRXFJKSFUJAPBOXZFXYOFQRPMZJNTRJ"
