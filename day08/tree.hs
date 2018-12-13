

main :: IO ()
main = interact (show . solve . readInput)


data Tree = Node {
  children :: [Tree],
  metadata :: [Int]
} deriving (Show)


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


foldMetadata :: (Int -> a -> a) -> a -> Tree -> a
foldMetadata f acc (Node children metadata) = foldr (\t r -> foldMetadata f r t) acc' children
  where
    acc' = foldr f acc metadata


solve :: Tree -> Int
solve t = foldMetadata (+) 0 t
