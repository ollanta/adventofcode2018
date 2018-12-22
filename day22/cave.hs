import qualified Data.Array as A
import qualified Data.Map as M
import qualified Data.Ord as O

main :: IO ()
main = interact (show . solve . readD)

type Coord = (Int, Int)

data Type = Rocky | Wet | Narrow
  deriving (Eq, Show)

readD :: String -> (Int, Coord)
readD s = (depth, target)
  where
    line1:line2:_ = lines s
    depth = read $ words line1 !! 1

    target = (read x, read y)
    (x,',':y)  = span (/=',') $ words line2 !! 1

type CMapper = M.Map Coord Int

geoIndex :: Int -> CMapper -> Coord -> Int
geoIndex d lut (0,0) = 0
geoIndex d lut (x,0) = x*16807
geoIndex d lut (0,y) = y*48271
geoIndex d lut (x,y) = erosionLevel d (lut M.! (x-1,y)) * erosionLevel d (lut M.! (x,y-1))


erosionLevel :: Int -> Int -> Int
erosionLevel d gi = (gi + d) `rem` 20183


getType :: Int -> Type
getType elvl
  | mod3 == 0 = Rocky
  | mod3 == 1 = Wet
  | mod3 == 2 = Narrow
  where
    mod3 = elvl `rem` 3


riskLevel :: Type -> Int
riskLevel Rocky = 0
riskLevel Wet   = 1
riskLevel Narrow = 2


generateMapper :: Int -> Coord -> CMapper
generateMapper depth (x,y) = M.adjust (const 0) (x,y) mapper 
  where
    coordlist = [(x',y') | x'<-[0..x], y'<-[0..y]]

    mapper = foldl updateMapper M.empty coordlist

    updateMapper mapper coord = M.insert coord (geoIndex depth mapper coord) mapper


solve (depth, target) = sum . map riskLevel $ types
  where
    mapper = generateMapper depth target
    types  = map (getType . erosionLevel depth) (M.elems mapper)
