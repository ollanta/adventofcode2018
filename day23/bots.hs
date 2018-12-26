import qualified Data.List as L
import qualified Data.Ord as O

main :: IO ()
main = interact (showD . solve . readD)


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


solve :: [Bot] -> Int
solve bots = length inrange
  where
    strongest = L.maximumBy (O.comparing snd) bots
    (scoord, srad) = strongest
    inrange   = filter ((<=srad) . distance scoord . fst) bots


showD = show
