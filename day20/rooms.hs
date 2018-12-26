import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Ord as O


main :: IO ()
main = interact (showD . solve . readD)


data Direction = North | East | South | West
  deriving (Show, Eq)

data RMatcher = RDir Direction | RBranch [Regex]
  deriving (Show, Eq)

type Regex = [RMatcher]


readD :: String -> Regex
readD ('^':s) = r
  where
    s' = takeWhile (/='$') s
    (r, "") = readRegex s'


readDir :: Char -> Direction
readDir 'N' = North
readDir 'E' = East
readDir 'S' = South
readDir 'W' = West


readRegex :: String -> (Regex, String)
readRegex "" = ([], "")
readRegex (c:s)
  | c `elem` "NESW" = (dir:r1, s')
  where
    dir      = RDir $ readDir c
    (r1, s') = readRegex s
readRegex ('(':s) = (RBranch branches:r1, s'')
  where
    (branches, s') = loop s
    (r1, s'') = readRegex s'

    loop :: String -> ([Regex], String)
    loop str
      | c == ')' = ([r], s')
      | c == '|' = (r:rs, s'')
      where
        (r, s) = readRegex str
        (c:s') = s
        (rs, s'') = loop s'
readRegex s@(c:_)
  | isstopper = ([], s)
  where
    isstopper = c `elem` "|)"


type Coord = (Int, Int)

data Feature = Room | Door
  deriving (Eq, Show)

type RMap = M.Map Coord Feature


move :: Direction -> Coord -> Coord
move North (x,y) = (x,y-1)
move South (x,y) = (x,y+1)
move West  (x,y) = (x-1,y)
move East  (x,y) = (x+1,y)


generateMap :: Regex -> RMap
generateMap rx = rmap
  where
    initMap   = M.singleton (0,0) Room
    (rmap, _) = helper initMap [(0,0)] rx

    helper :: RMap -> [Coord] -> Regex -> (RMap, [Coord])
    helper m cs [] = (m, cs)
    helper m cs (RDir dir:rms) = helper m'' cs'' rms
      where
        cs' = map (move dir) cs
        inserter v m c = M.insert c v m
        m' = L.foldl' (inserter Door) m cs'

        cs'' = map (move dir) cs'
        m'' = L.foldl' (inserter Room) m' cs''
    helper m cs (RBranch brs:rms) = helper m' cs'' rms
      where
        folder (fm, csacc) br = let (fm', cs') = helper fm cs br in (fm', L.nub (csacc++cs'))
        (m', cs'') = L.foldl' folder (m, cs) brs


showMap :: RMap -> String
showMap rmap = unlines design
  where
    coords = M.keys rmap
    minX   = minimum . map fst $ coords
    maxX   = maximum . map fst $ coords
    minY   = minimum . map snd $ coords
    maxY   = maximum . map snd $ coords

    showF (0,0) = 'X'
    showF c@(x,y)
      | f == Nothing = ' '
      | f == Just Room = '.'
      | f == Just Door = if even x then '-' else '|'
      where
        f = M.lookup c rmap
        evenX = x `rem` 2 == 0
    
    design = [[showF (x,y) | x <- [minX..maxX]] | y <- [minY..maxY]]


solve regex = (rmap, findFurthest rmap)
  where
    rmap = generateMap regex


findFurthest :: RMap -> (Coord, Int)
findFurthest rmap = L.maximumBy (O.comparing snd) $ M.assocs distMap
  where
    initMap = M.singleton (0,0) 0

    distMap :: M.Map Coord Int
    distMap = helper initMap [(0,0)]

    helper m []     = m
    helper m (c:rest) = helper m' (rest ++ unseenRooms)
      where
        directions  = filter hasDoor [North, South, East, West]
        hasDoor dir = M.lookup (move dir c) rmap == Just Door

        adjRooms    = map (\dir -> move dir . move dir $ c) directions
        unseenRooms = filter (`M.notMember` m) adjRooms

        distance = m M.! c

        m' = M.union m (M.fromList . zip unseenRooms . repeat $ distance+1)


showD (rmap, ans) = unlines [showMap rmap, show ans]
