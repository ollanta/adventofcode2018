

main :: IO ()
main = interact (show . solve . parseInput)


type Cut = (Int, Int, Int, Int)

type Coord = (Int, Int)


parse1 :: String -> Cut
parse1 s = (x, y, width, height)
  where
    (id:at:topleft:size:_) = words s
    x = read . takeWhile (/=',') $ topleft
    y = read . filter (/=':') . drop 1 . dropWhile (/=',') $ topleft
    width = read . takeWhile (/='x') $ size
    height = read . drop 1 . dropWhile (/='x') $ size


parseInput :: String -> [Cut]
parseInput = map parse1 . lines


inCut :: Coord -> Cut -> Bool
inCut (x, y) (cx, cy, cw, ch)
  | x < cx       = False
  | x >= cx + cw = False
  | y < cy       = False
  | y >= cy + ch = False
  | otherwise    = True


overlaps :: Cut -> Cut -> Bool
overlaps cut1@(x1,y1,w1,h1) cut2@(x2,y2,w2,h2) =
  x1+w1 > x2 && x1 < x2+w2 && y1+h1 > y2 && y1 < y2+h2


nonoverlapping :: [Cut] -> [Cut]
nonoverlapping cuts = filter (not . overlapsAny) cuts
  where
    overlapsAny :: Cut -> Bool
    overlapsAny cut = any (overlaps cut) (filter (/=cut) cuts)


solve :: [Cut] -> [Cut]
solve = nonoverlapping
