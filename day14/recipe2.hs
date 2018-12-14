
main :: IO ()
main = interact (show . solve . read)


solve :: Int -> Int
solve n = helper 0 $ recipeScores
  where
    -- not really correct when n starts with 0s
    digits = getDigits n
    ndigits = length digits
    helper k scores
      | take ndigits scores == digits = k
    helper k (s:rest) = helper (k+1) rest


recipeScores = 3:7:helper 2 0 1
  where
    helper recipelength e1 e2 = digits ++ helper recipelength' e1' e2'
      where
        e1score = recipeScores !! e1
        e2score = recipeScores !! e2

        digits = getDigits (e1score + e2score)
        
        recipelength' = recipelength + length digits
        e1' = (e1 + 1 + e1score) `rem` recipelength'
        e2' = (e2 + 1 + e2score) `rem` recipelength'


getDigits :: Int -> [Int]
getDigits x = reverse . helper $ x
  where
    helper x
      | (0,r) <- qr = [r]
      | (q,r) <- qr = r:helper q
      where
        qr = x `quotRem` 10
