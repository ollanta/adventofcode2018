import Data.List as L
import Data.Map.Strict as M

main :: IO ()
main = interact (show . solve . read)


--solve :: Int -> [Int]
--solve n = L.take 10 . L.drop n $ recipeScores

solve :: Int -> Int
solve n = helper 0 $ recipeScores
  where
    -- not really correct when n starts with 0s
    digits = getDigits n
    ndigits = length digits
    helper k scores
      | L.take ndigits scores == digits = k
    helper k (s:rest) = helper (k+1) rest


initMap :: M.Map Int Int
initMap = M.fromList [(0,3),(1,7)]


recipeScores :: [Int]
recipeScores = 3:7:helper initMap 0 1
  where
    helper rmap e1 e2 = digits ++ helper rmap' e1' e2'
      where
        e1score = rmap ! e1
        e2score = rmap ! e2

        digits = getDigits (e1score + e2score)

        size = M.size rmap
        rmap' = L.foldr (\(k,a) m -> M.insert k a m) rmap $ zip [size..] digits

        size' = size + length digits
        e1' = (e1 + 1 + e1score) `rem` size'
        e2' = (e2 + 1 + e2score) `rem` size'


getDigits :: Int -> [Int]
getDigits x = reverse . helper $ x
  where
    helper x
      | (0,r) <- qr = [r]
      | (q,r) <- qr = r:helper q
      where
        qr = x `quotRem` 10
