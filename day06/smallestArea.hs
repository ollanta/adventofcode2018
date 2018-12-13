import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe

main :: IO ()
main = interact (show . solve . parseInput)


type Coordinate = (Integer, Integer)


parseInput :: String -> [Coordinate]
parseInput = map readCoordinate . lines
  where
    readCoordinate = readList . words . filter (/=',')
    readList (a:b:[]) = (read a, read b)


distance :: Coordinate -> Coordinate -> Integer
distance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)


data Area = Finite Integer | Infinite
  deriving (Eq, Ord, Show)


solve :: [Coordinate] -> Area
solve coords = maximum . filter (/= Infinite) . map solve' $ coords
  where
    solve' = getArea coords


getArea :: [Coordinate] -> Coordinate -> Area
getArea allCoords c
  | c `Set.member` infiniteAreas allCoords = Infinite
  | otherwise                          = fillArea (filter (/= c) allCoords) c


-- Area is infinite if it's closest to any element on the square hull
infiniteAreas :: [Coordinate] -> Set.Set Coordinate
infiniteAreas coords = Set.fromList . map Maybe.fromJust . List.filter (/= Nothing) . map (\hc -> closestCoord coords hc) $ hullCoords coords


hullCoords :: [Coordinate] -> [Coordinate]
hullCoords coords = zip xrange (repeat minY) ++ zip xrange (repeat maxY) ++
                    zip (repeat minX) yrange ++ zip (repeat maxX) yrange
  where
    xcords = map fst coords
    minX   = minimum xcords
    maxX   = maximum xcords
    xrange = [minX..maxX]
    
    ycords = map snd coords
    minY   = minimum ycords
    maxY   = maximum ycords
    yrange = [minY..maxY]


closestCoord :: [Coordinate] -> Coordinate -> Maybe Coordinate
closestCoord coords target
  | minCoord:[] <- coordsAtMinDistance = Just minCoord
  | otherwise                          = Nothing
  where
    distances = map (distance target) coords
    coordDistancePairs = zip coords distances
    minDistance = minimum distances
    coordsAtMinDistance = map fst . filter ((==) minDistance . snd) $ coordDistancePairs


fillArea :: [Coordinate] -> Coordinate -> Area
fillArea otherCoords c = Finite . toInteger . length $ fill' (Set.fromList [c]) (getAdjacent c)
  where fill' filled (next:rest)
          | next `Set.member` filled = fill' filled rest
          | shouldFill next          = fill' (Set.insert next filled) (getAdjacent next ++ rest)
          | otherwise                = fill' filled rest
        fill' filled []                  = filled
        shouldFill target = (minimum . map (distance target) $ otherCoords) > distance target c
        getAdjacent (x,y) = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
