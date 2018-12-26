import qualified Data.List as L
import qualified Data.Ord as O

main :: IO ()
main = interact (show . solve . readD)


type Coord = (Int, Int, Int)

type Bot = (Coord, Int)


readD :: String -> [Bot]
readD = map readBot . lines
  where
    readBot s = ((x,y,z),r)
      where
        s' = filter (`elem` "-0123456789,") s
        (x:y:z:r:_) = map read $ splitWith ',' s'

    splitWith _ "" = []
    splitWith s cs = before : splitWith s (dropWhile (==s) after)
      where
        (before, after) = span (/=s) cs


distance :: Coord -> Coord -> Int
distance (x1,y1,z1) (x2,y2,z2) = abs (x1-x2) + abs (y1-y2) + abs (z1-z2)


solve bots = minimum radii
  where
    subsets = mostOverlappingSubsets bots

    radii   = map shortestDistanceToO subsets
    

mostOverlappingSubsets :: [Bot] -> [[Bot]]
mostOverlappingSubsets l = helper 0 0 l
  where
    helper m k [] = []
    helper m k [b] = [[b]]
    helper m k (b:bs)
      | hl with > hl without = with
      | hl with < hl without = without
      | otherwise            = with ++ without
      where
        hl [] = 0
        hl l  = length . head $ l

        overlapping = filter (overlaps b) bs
        withr   = map (b:) (helper m (k+1) overlapping)
        with    = if (m < k+1+length overlapping) then withr else []
        m'      = max m (hl with)
        without = if (m' < length bs) then helper m' k bs else []


shortestDistanceToO :: [Bot] -> Int
shortestDistanceToO bots = maximum . map (distanceTo (0,0,0)) $ bots


overlaps :: Bot -> Bot -> Bool
overlaps (c1,r1) (c2,r2) = distance c1 c2 <= r1+r2


distanceTo :: Coord -> Bot -> Int
distanceTo c (bc,r) = max 0 (distance c bc - r)
