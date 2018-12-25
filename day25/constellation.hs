import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L


main :: IO ()
main = interact (showD . solve . readD)


type Coord = (Int, Int, Int, Int)


type Constellation = [Coord]


readD = map readCoord . lines
  where
    readCoord :: String -> Coord
    readCoord s = read $ "(" ++ s ++ ")"


distance (x1,y1,z1,t1) (x2,y2,z2,t2) = abs (x1-x2) + abs (y1-y2) + abs (z1-z2) + abs (t1-t2)


solve coords = loop coords
  where
    loop []     = []
    loop (c:cs) = constellation : loop cs'
      where
        (constellation, cs') = expandConstellation [] [c] cs

    expandConstellation con []     other = (con, other)
    expandConstellation con (n:ns) other = expandConstellation (n:con) (ns++close) far
      where
        (close, far) = L.partition ((<=3) . distance n) other


showD = (++"\n") . show . length
