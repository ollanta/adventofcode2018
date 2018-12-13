

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


getMinX (x, y, w, h) = x
getMinY (x, y, w, h) = y
getMaxX (x, y, w ,h) = x + w
getMaxY (x, y, w ,h) = y + h


boundingSquare :: [Cut] -> (Coord, Coord)
boundingSquare cuts = ((minX, minY), (maxX, maxY))
  where
    minX = minimum . map getMinX $ cuts
    minY = minimum . map getMinY $ cuts
    maxX = maximum . map getMaxX $ cuts
    maxY = maximum . map getMaxY $ cuts


solve :: [Cut] -> Int
solve cuts = length . filter (inNCuts cuts 2) $ points
  where
    points = [(x,y) | x <- [minX..maxX], y <- [minY..maxY]]
    ((minX, minY), (maxX, maxY)) = boundingSquare cuts


inNCuts :: [Cut] -> Int -> Coord -> Bool
inNCuts cuts n coord = (==n) . length . take n . filter (inCut coord) $ cuts
