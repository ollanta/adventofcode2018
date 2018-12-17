import Data.Array as A


main :: IO ()
main = interact (showGround . solve . readD)


data GroundType = Sand | Clay | SWater | RWater
  deriving (Eq)


type Ground = A.Array (Int,Int) GroundType


data Range = RangeX Int Int Int | RangeY Int Int Int
  deriving (Show)


-- (x,y)
type Coord = (Int, Int)


showGround :: Ground -> String
showGround ground = unlines $ groundlines ++ bonuslines
  where
    ((minX,minY),(maxX,maxY)) = A.bounds ground
    firstRow   = [if x==500 then '+' else '.' | x <- [minX..maxX]]
    groundRows = [[showGT (ground A.! (x,y)) | x <- [minX..maxX]] | y <- [minY..maxY]]

    showGT Sand = '.'
    showGT Clay = '#'
    showGT SWater = '~'
    showGT RWater = '|'

    groundlines = firstRow:groundRows
    rwatercount = length . filter (== RWater) $ A.elems ground
    swatercount = length . filter (== SWater) $ A.elems ground
    bonuslines  = map show [rwatercount + swatercount, swatercount]


toPoints :: Range -> [Coord]
toPoints (RangeX x y1 y2) = [(x,y) | y <- [y1..y2]]
toPoints (RangeY y x1 x2) = [(x,y) | x <- [x1..x2]]


readD :: String -> [Range]
readD s = ranges
  where
    lined = lines s
    ranges = map readRange lined 


readRange :: String -> Range
readRange s
  | firstChar == "x" = RangeX (read int1) (read int2) (read int3)
  | firstChar == "y" = RangeY (read int1) (read int2) (read int3)
  where
    firstChar:int1:_:int2:int3:[] = splitWith s "=, ."

    
splitWith :: String -> [Char] -> [String]
splitWith "" chars = []
splitWith s  chars = s' : splitWith rest chars
  where
    s'   = takeWhile shouldKeep s
    rest = dropWhile (not . shouldKeep) $ dropWhile shouldKeep s
    shouldKeep = not . (`elem` chars)


--solve :: [Range] -> 
solve ranges = fill ground
  where
    ground = groundFromRanges ranges


groundFromRanges :: [Range] -> Ground
groundFromRanges ranges = ground
  where
    points = concatMap toPoints ranges
    minX   = minimum . map fst $ points
    maxX   = maximum . map fst $ points
    minY   = minimum . map snd $ points
    maxY   = maximum . map snd $ points

    initGr = A.listArray ((minX-1,minY),(maxX+1,maxY)) . repeat $ Sand
    ground = (A.//) initGr . zip points . repeat $ Clay


fill :: Ground -> Ground
fill ground = helper 0 ground [(500,minY-1)]
  where
    ((minX,minY),(maxX,maxY)) = A.bounds ground

    --helper 150 ground _ = ground
    helper i ground (coord:rest)
      | null newcoords = helper (i+1) ground' rest
      | otherwise      = helper (i+1) ground'' (rest ++ newcoords')
      where
        (ground', newcoords)  = spreadDownwards ground coord
        (ground'', newcoords') = spreadSideways ground' (head newcoords)

    helper i ground [] = ground


spreadSideways :: Ground -> Coord -> (Ground, [Coord])
spreadSideways ground (cx,cy)
  | not (canSpread (cx,cy)) = (ground, [])
  | canStay                 = (sground, scoords)
  | otherwise               = (rground, rcoords)
  where
    canStop c = point == Clay || point == SWater
      where
        point = ground A.! c

    canFlow c = point == Sand || point == RWater
      where
        point = ground A.! c

    canSit (x,y) = canStop (x,y+1)
         
    canSpread c = canFlow c && canSit c

    goLeft = takeWhile canSpread [(cx-k,cy) | k <- [1..]]
    goRight = takeWhile canSpread [(cx+k,cy) | k <- [1..]]
    spread = goLeft ++ [(cx,cy)] ++ goRight

    (lx,_) = minimum spread
    (rx,_) = maximum spread

    canStay = canStop (lx-1,cy) && canStop (rx+1,cy)

    rcoords = filter canFlow [(lx-1,cy), (rx+1,cy)]
    rground = ground A.// (zip (rcoords ++ spread) (repeat RWater))

    scoords = filter ((==RWater) . (ground A.!)) [(x,y-1) | (x,y) <- spread]
    sground = ground A.// (zip spread (repeat SWater))


isSand :: Ground -> Coord -> Bool
isSand ground coord = ground A.! coord == Sand


spreadDownwards :: Ground -> Coord -> (Ground, [Coord])
spreadDownwards ground (cx,cy)
  | null freeDownwards = (ground,  [(cx, cy)])
  | ny == maxY         = (ground', [])
  | otherwise          = (ground', [(nx, ny)])
  where
    ((minX,minY),(maxX,maxY)) = A.bounds ground
    freeDownwards = takeWhile (isSand ground) [(cx,cy') | cy' <- [cy+1..maxY]]
    ground' = ground A.// (zip freeDownwards $ repeat RWater)
    (nx,ny) = last freeDownwards
