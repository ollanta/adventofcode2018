
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
boundingBox stars = getMaxMin positions
  where
    positions = map getPosition stars


getMaxMin :: [(Int, Int)] -> Box
getMaxMin ((x,y):rest) = helper x x y y rest
  where
    helper minX maxX minY maxY [] = Box minX maxX minY maxY
    helper minX maxX minY maxY ((x,y):rest) = helper minX' maxX' minY' maxY' rest
      where
        minX' = min minX x
        maxX' = max maxX x
        minY' = min minY y
        maxY' = max maxY y


areaOf :: Box -> Int
areaOf (Box minX maxX minY maxY) = (maxX - minX) * (maxY - minY)


move :: Star -> Star
move (Star (x, y) (vx, vy)) = Star (x+vx, y+vy) (vx, vy)


solve :: [Star] -> (Int, [Star])
solve stars = turningpoint (zip3 times steps areas)
  where
    times = [0..]
    steps = iterate (map move) stars
    areas = map (areaOf . boundingBox) steps

    turningpoint ((t,s,a):rest@((_,s',a'):_))
      | a < a'    = (t, s)
      | otherwise = turningpoint rest


showData :: (Int, [Star]) -> String
showData (time, stars) = unlines $ [show time] ++ starlines
  where
    positions = map getPosition $ stars
    Box minX maxX minY maxY = boundingBox stars

    starlines = [[if (x,y) `elem` positions then '*' else '.' | x <- [minX..maxX]]
                 | y <- [minY..maxY]]
