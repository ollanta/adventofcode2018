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


solve :: [Coordinate] -> Integer
solve coords = fillArea coords center
  where
    center = (centerX, centerY)
    n      = toInteger $ length coords
    centerX = (`div` n) . sum . map fst $ coords
    centerY = (`div` n) . sum . map snd $ coords


getTotalDist :: [Coordinate] -> Coordinate -> Integer
getTotalDist coords target = sum . map (distance target) $ coords


maxTotalDist :: Integer
maxTotalDist = 10000


fillArea :: [Coordinate] -> Coordinate -> Integer
fillArea coords c = toInteger . length $ fill' (Set.fromList [c]) (getAdjacent c)
  where fill' filled (next:rest)
          | next `Set.member` filled = fill' filled rest
          | shouldFill next          = fill' (Set.insert next filled) (getAdjacent next ++ rest)
          | otherwise                = fill' filled rest
        fill' filled []              = filled
        shouldFill target = getTotalDist coords target < maxTotalDist
        getAdjacent (x,y) = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
