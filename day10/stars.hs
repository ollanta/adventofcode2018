
main :: IO ()
main = interact (showData . solve . readData)


data Star = Star {
  getPosition :: (Int, Int),
  getVelocity :: (Int, Int)
} deriving (Show, Eq, Ord)


read1 :: String -> Star
read1 s = Star (x, y) (vx, vy)
  where
    s' = drop 1 . dropWhile (/='<') $ s
    posS = takeWhile (/='>') $ s'

    s'' = drop 1 . dropWhile (/='<') $ s'
    velS = takeWhile (/='>') $ s''

    x:y:_ = map read . words . filter (/=',') $ posS
    vx:vy:_ = map read . words . filter (/=',') $ velS


readData :: String -> [Star]
readData = map read1 . lines


data Box = Box {
  minX :: Int,
  maxX :: Int,
  minY :: Int,
  maxY :: Int
} deriving (Show, Eq, Ord)


boundingBox :: [Star] -> Box
boundingBox stars = Box minX maxX minY maxY
  where
    maxX = maximum . map (fst . getPosition) $ stars
    minX = minimum . map (fst . getPosition) $ stars
    maxY = maximum . map (snd . getPosition) $ stars
    minY = minimum . map (snd . getPosition) $ stars


areaOf :: Box -> Int
areaOf (Box minX maxX minY maxY) = (maxX - minX) * (maxY - minY)


move :: Star -> Star
move (Star (x, y) (vx, vy)) = Star (x+vx, y+vy) (vx, vy)


solve :: [Star] -> [Star]
solve stars = turningpoint (zip steps areas)
  where
    steps = iterate (map move) stars
    areas = map (areaOf . boundingBox) steps

    turningpoint ((s,a):(s',a'):rest)
      | a < a'    = s
      | otherwise = turningpoint ((s',a'):rest)


showData :: [Star] -> String
showData stars = unlines starlines
  where
    positions = map getPosition $ stars
    Box minX maxX minY maxY = boundingBox stars

    starlines = [[if (x,y) `elem` positions then '*' else '.' | x <- [minX..maxX]]
                 | y <- [minY..maxY]]
