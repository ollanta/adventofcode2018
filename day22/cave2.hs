import qualified Data.Array as A
import qualified Data.Map as M
import qualified Data.Ord as O
import qualified Data.List as L
import qualified Data.Heap as H
import qualified Data.Set as S

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

type GMapper = M.Map Coord Int

type TMapper = M.Map Coord Type

geoIndex :: Int -> Coord -> GMapper -> Coord -> Int
geoIndex d t lut (0,0) = 0
geoIndex _ (tx,ty) _ (x,y)
  | tx == x && ty == y = 0
geoIndex d t lut (x,0) = x*16807
geoIndex d t lut (0,y) = y*48271
geoIndex d t lut (x,y) = erosionLevel d (lut M.! (x-1,y)) * erosionLevel d (lut M.! (x,y-1))


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


geoMapper :: Int -> Coord -> GMapper
geoMapper depth target@(x,y) = mapper
  where
    {-this should be a memoized function to allow any coords >0
      instead we just hope to generate a "big enough" map-}
    coordlist = [(x',y') | x'<-[0..(max x y)*2], y'<-[0..(max x y)*2]]

    mapper = foldl updateMapper M.empty coordlist

    updateMapper mapper coord = M.insert coord (geoIndex depth target mapper coord) mapper


geoToTypeMapper :: Int -> GMapper -> TMapper
geoToTypeMapper depth = M.map (getType . erosionLevel depth)


solve (depth, target@(tx,ty)) = searchTarget tmapper target
  where
    gmapper = geoMapper depth (tx,ty)
    tmapper = geoToTypeMapper depth gmapper


data Gear = Gear | NoGear
  deriving (Show, Eq, Ord)

data Torch = Torch | NoTorch
  deriving (Show, Eq, Ord)

type Tools = (Gear, Torch)

type State = (Tools, Coord)


searchTarget :: TMapper -> Coord -> Int
searchTarget lut target@(tx,ty) = helper initSet initHeap
  where
    initHeap :: H.MinPrioHeap Int State
    initState = ((NoGear, Torch), (0,0))
    initHeap = H.fromList [(minimumLeft initState, initState)]

    -- already-visited-set
    initSet = S.empty

    minimumLeft ((_,t),(x,y))
      | t /= Torch = 7 + distance
      | otherwise  = distance
      where
        distance = abs (tx-x) + abs (ty-y)

    helper :: S.Set State -> H.MinPrioHeap Int State -> Int
    helper vset heap
      | ((_,Torch),coord) <- state, coord == target = msofar
      | otherwise = helper vset' heap''
      where
        ([next], heap')  = H.splitAt 1 heap
        (minutes, state) = next

        msofar = minutes - minimumLeft state

        newstates = zip (repeat $ msofar+7) (toolChange lut state) ++
                    zip (repeat $ msofar+1) (moves lut state)
        newstates' = filter ((`S.notMember` vset) . snd) newstates
        newstates'' = map (\(m, st) -> (m+minimumLeft st, st)) newstates'

        stateheap :: H.MinPrioHeap Int State
        stateheap = H.fromList newstates''

        heap'' = H.union heap' stateheap

        vset' = S.insert state vset


toolChange :: TMapper -> State -> [State]
toolChange lut (tool, c) = [(tool', c) | g <- [Gear, NoGear], t <- [Torch, NoTorch], let tool' = (g,t), tool' /= tool, canPass lut tool' c]

moves :: TMapper -> State -> [State]
moves mapper (tool, (x,y)) = [(tool, c') | c' <- allowedMoves]
  where
    allowedMoves = filter (canPass mapper tool) [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]


canPass :: TMapper -> Tools -> Coord -> Bool
canPass mapper _ c
  | M.notMember c mapper = False
canPass mapper (gear, torch) c
  | ctype == Rocky  = gear == Gear || torch == Torch
  | ctype == Wet    = torch == NoTorch
  | ctype == Narrow = gear == NoGear
  where
    ctype = mapper M.! c
    

showType Rocky  = '.'
showType Wet    = '='
showType Narrow = '|'


showMap :: TMapper -> String
showMap mapper = unlines . splitEvery (maxX+1) . map (showType . snd) $ kvlist
  where
    kvlist = L.sortBy (O.comparing ((\(x,y) -> (y,x)) . fst)) (M.assocs mapper)
    maxX = maximum . map (fst . fst) $ kvlist

    splitEvery :: Int -> [a] -> [[a]]
    splitEvery _ [] = []
    splitEvery n s  = take n s : splitEvery n (drop n s)
