import qualified Data.Set as Set

main :: IO ()
main = interact sumLines
     where sumLines = show . findFirstDupe . partialSums . cycle . map readPlusMinus . lines

readPlusMinus :: String -> Integer
readPlusMinus ('+':rest) = read rest
readPlusMinus all = read all


partialSums :: (Num a) => [a] -> [a]
partialSums = scanl (+) 0


findFirstDupe :: (Ord a) => [a] -> a
findFirstDupe list = helper Set.empty list
    where helper s (x:rest) = if (Set.member x s) then x else helper (Set.insert x s) rest
          helper s [] = error "What then?"