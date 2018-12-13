import qualified Data.List as List
import qualified Data.Maybe as Maybe



main :: IO ()
main = interact (show . solve . lines)


solve :: [String] -> String
solve list = map fst . filter (uncurry (==)) $ zip s1 s2
  where
    (d, s1, s2) = findMin list


findMin :: [String] -> (Int, String, String)
findMin list = Maybe.fromJust . List.find (\(d, s1, s2) -> d == 1) $ comparisons list
  where
    comparisons (l:rest) = zip3 (map (distance l) rest) (repeat l) rest ++ comparisons rest
    comparisons []       = []
    


type Class = (Bool, Bool)


distance :: String -> String -> Int
distance s1 s2 = length . filter id . map (uncurry (/=)) $ zip s1 s2
