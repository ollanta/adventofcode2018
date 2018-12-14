
main :: IO ()
main = interact (show . solve . read)


solve :: Int -> [Int]
solve n = take 10 . drop n $ recipeScores


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
