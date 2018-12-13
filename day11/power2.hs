import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord

main :: IO ()
main = interact (show . solve . read)


grid :: [(Int,Int)]
grid = [(x,y) | x <- [1..300], y <- [1..300]]


solve :: Int -> ((Int, Int), Int, Int)
solve serialNo = (coord, size, area)
  where
    maxpersize' = maxpersize serialNo
    (size, (coord, area)) = List.maximumBy (Ord.comparing $ snd . snd) $ zip [0..] maxpersize'


maxpersize :: Int -> [((Int,Int), Int)]
maxpersize serialNo = map (List.maximumBy (Ord.comparing snd)) $ listed
  where
    listed :: [[((Int, Int), Int)]]
    listed = map Map.toList . takeWhile (not . Map.null) $ foldPower serialNo


type CoordMap = Map.Map (Int,Int) Int


glookup :: (Ord k) => k -> Map.Map k a -> a
glookup k m = Maybe.fromJust . Map.lookup k $ m


foldPower :: Int -> [CoordMap]
foldPower serialNo = coordmaps
  where
    size1map = Map.fromAscList $ zip grid (map power1s grid)
    zeromap = Map.map (const 0) size1map
    coordmaps = zeromap : size1map : map combinePower (zip3 [2..] (drop 1 coordmaps) coordmaps)

    power1s = power1 serialNo

    combinePower :: (Int, CoordMap, CoordMap) -> CoordMap
    combinePower (size, pmneg1, pmneg2) = newEntries
      where
        grid' :: [(Int,Int)]
        grid' = [(x,y) | x <- [1..300-size+1], y<-[1..300-size+1]]

        newEntries :: CoordMap
        newEntries = Map.fromAscList $ zip (grid') (map newPower grid')

        newPower :: (Int,Int) -> Int
        newPower (x,y) = sums + corners - center
          where
            sums :: Int
            sums = sum . map (`glookup` pmneg1) $ [(x,y),(x+1,y+1)]

            center :: Int
            center = glookup (x+1,y+1) pmneg2

            corners :: Int
            corners = sum . map power1s $ [(x,y+size-1),(x+size-1,y)]


power1 :: Int -> (Int, Int) -> Int
power1 serialNo (x,y) = step2 . step1 $ (x,y)
  where
    rackId      = (x+10)
    step1 (x,y) = (rackId*y+serialNo)*rackId
    step2 s     = ((s `quot` 100) `rem` 10) - 5
