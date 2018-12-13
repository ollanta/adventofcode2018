import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe


main :: IO ()
main = interact (show . solve . readInput)


data Tree = Node {
  children :: [Tree],
  metadata :: [Int]
} deriving (Show, Ord, Eq)


readInput :: String -> Tree
readInput = fst . read1 . map read . words


read1 :: [Int] -> (Tree, [Int])
read1 (nchildren:nmetadata:rest) = (Node children metadata, rest'')
  where
    (children, rest')  = readN nchildren rest
    (metadata, rest'') = splitAt nmetadata rest'

    readN :: Int -> [Int] -> ([Tree], [Int])
    readN 0 list = ([], list)
    readN n list = (node:nodes, rest'')
      where
        (node,  rest')  = read1 list
        (nodes, rest'') = readN (n-1) rest'


valueOf :: Tree -> Int
valueOf (Node [] metadata) = sum metadata
valueOf (Node children metadata) = sum $ values2
  where
    childMap = Map.fromList $ zip [1..] children
    values :: [Int]
    values = map (valueOf . Maybe.fromJust) . filter (/=Nothing) . map (\m -> Map.lookup m childMap) $ metadata
    values2 :: [Int]
    values2 = Maybe.mapMaybe (\m -> Map.lookup m childMap >>= Just . valueOf) $ metadata

solve :: Tree -> Int
solve t = valueOf t
