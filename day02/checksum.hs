import qualified Data.List as List


main :: IO ()
main = interact (show . solve . lines)


solve :: [String] -> Int
solve = checksum . map classify


type Class = (Bool, Bool)


classify :: String -> Class
classify string = (2 `elem` groupSizes, 3 `elem` groupSizes)
  where
    grouped = List.groupBy (==) . List.sort $ string
    groupSizes = map length grouped


checksum :: [Class] -> Int
checksum list = twos * threes
  where
    twos = length . filter id . map fst $ list
    threes = length . filter id . map snd $ list
