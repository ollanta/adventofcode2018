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


solve :: [Coordinate] -> Int
solve coords = length . filter (< maxTotalDist) . map (getTotalDist coords) $ getCandidates coords


getTotalDist :: [Coordinate] -> Coordinate -> Integer
getTotalDist coords target = sum . map (distance target) $ coords


maxTotalDist :: Integer
maxTotalDist = 10000


getCandidates :: [Coordinate] -> [Coordinate]
getCandidates coords = [(x,y) |
                        x <- [minX-maxDist..maxX+maxDist],
                        y <- [minY-maxDist..maxY+maxDist]]
  where
    maxDist = div maxTotalDist (toInteger $ length coords)
    minX = minimum . map fst $ coords
    minY = minimum . map snd $ coords
    maxX = maximum . map fst $ coords
    maxY = maximum . map snd $ coords
