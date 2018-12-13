import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord

main :: IO ()
main = interact (show . solve . read)


grid :: Grid Coord
grid = [[(x,y) | x <- [1..300]] | y <- [1..300]]


solve :: Int -> ((Int, Int), Int, Int)
solve serialNo = (coord, size, area)
  where
    maxpersize' = maxpersize serialNo
    (size, (coord, area)) = List.maximumBy (Ord.comparing $ snd . snd) $ zip [0..] maxpersize'


maxpersize :: Int -> [(Coord, Int)]
maxpersize serialNo = map (List.maximumBy (Ord.comparing snd)) $ coordPowers
  where
    relevantPowers :: [Grid Int]
    relevantPowers = takeWhile (not . all List.null) $ foldPower serialNo
    
    withCoord :: Grid Int -> Grid (Coord, Int)
    withCoord pgrid = map (\(a,b) -> zip a b) $ zip grid pgrid

    coordPowers = map (concat. withCoord) relevantPowers


type Coord = (Int, Int)


type Grid a = [[a]]


foldPower :: Int -> [Grid Int]
foldPower serialNo = coordgridlist
  where
    size1grid :: Grid Int
    size1grid = map (map power1s) grid
    zerogrid :: Grid Int
    zerogrid = replicate 300 (replicate 300 0)

    coordgridlist = zerogrid : size1grid : map combinePower (zip3 [2..] (drop 1 coordgridlist) coordgridlist)

    power1s = power1 serialNo

    combinePower :: (Int, Grid Int, Grid Int) -> Grid Int
    combinePower (size, pmneg1, pmneg2) = combinePower' size grid pmneg1 pmneg1downleft pmneg2downleft
      where
        pmneg1downleft :: Grid Int
        pmneg1downleft = map (drop 1) $ drop 1 pmneg1

        pmneg2downleft :: Grid Int
        pmneg2downleft = map (drop 1) $ drop 1 pmneg2

    combinePower' :: Int -> Grid Coord -> Grid Int -> Grid Int -> Grid Int -> Grid Int
    combinePower' s gr pmneg1 pmneg1dl pmneg2dl = map (map newPower) mega
      where
        mega :: Grid (Coord, Int, Int, Int)
        mega = map (\(a,b,c,d) -> List.zip4 a b c d) $ List.zip4 gr pmneg1 pmneg1dl pmneg2dl
        
        newPower ((x,y), pmneg1, pmneg1dl, pmneg2dl) = sums + corners - center
          where
            sums :: Int
            sums = pmneg1 + pmneg1dl

            center :: Int
            center = pmneg2dl

            corners :: Int
            corners = power1s (x,y+s-1) + power1s (x+s-1,y)


power1 :: Int -> (Int, Int) -> Int
power1 serialNo (x,y) = step2 . step1 $ (x,y)
  where
    rackId      = (x+10)
    step1 (x,y) = (rackId*y+serialNo)*rackId
    step2 s     = ((s `quot` 100) `rem` 10) - 5
