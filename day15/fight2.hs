{-# LANGUAGE BangPatterns #-}

import qualified Data.Array as A
import qualified Data.Set as S
import qualified Data.Maybe as M
import qualified Data.Ord as O
import qualified Data.List as L

main :: IO ()
main = interact (showCaves . solve . readD)


data Feature = Open | Wall
  deriving (Eq, Show)

data ActorType = Goblin | Elf
  deriving (Eq, Show)

-- use initial coord as id
data Actor = Actor ActorType Int Coord
  deriving (Show, Eq)

type Coord = (Int, Int)

data CaveSquare = CaveA Actor | CaveF Feature
  deriving (Show, Eq)

type Cave = A.Array Coord CaveSquare


showCaves (cs, p) = unlines . (++["",show p]) . map showCave $ zip cs [0..]

showCave :: (Cave, Int) -> String
showCave (c,i) = unlines . (++[show i, show (totalHp c)]) . torows . map showCS $ A.elems c
  where
    (_, (h,w)) = A.bounds c

    torows [] = []
    torows l  = take (w) l : torows (drop (w) l)

    showCS (CaveF Wall) = '#'
    showCS (CaveF Open) = '.'
    showCS (CaveA (Actor Goblin _ _)) = 'G'
    showCS (CaveA (Actor Elf _ _))    = 'E'


readD :: String -> Cave
readD s = A.array ((1,1), (height, width)) cavelist
  where
    lined = takeWhile (not . null) . lines . filter (`elem` "#.GE\n") $ s
    height = length lined
    width  = length . head $ lined

    readCS '#' c = CaveF Wall
    readCS '.' c = CaveF Open
    readCS 'G' c = CaveA (Actor Goblin 200 c)
    readCS 'E' c = CaveA (Actor Elf    200 c)

    cavelist = [((y,x), readCS c (y,x)) |
                (y,row) <- zip [1..] lined,
                (x,c)   <- zip [1..] row]


type Path = (Coord, Coord)


getType :: Actor -> ActorType
getType (Actor atype _ _) = atype

getHp :: Actor -> Int
getHp (Actor _ hp _) = hp

getId :: Actor -> Coord
getId (Actor _ _ ic) = ic

adjacent :: Coord -> [Coord]
adjacent (y,x) = [(y-1,x), (y,x-1), (y,x+1), (y+1,x)] -- (sorted)

totalHp cave = sum [getHp actor | CaveA actor <- A.elems cave]

isOpen :: CaveSquare -> Bool
isOpen (CaveF Open) = True
isOpen _            = False


adjacentOpponents :: (Coord, Actor) -> Cave -> [(Coord, Actor)]
adjacentOpponents (coord, actor) cave = [(c, opp) | (c, CaveA opp) <- adjacentSquares, isOpponent opp]
  where
    isOpponent someone = getType actor /= getType someone
    adjacentSquares = [(c, cave A.! c) | c <- adjacent coord]


getMoveFor :: (Coord, Actor) -> Cave -> Path
getMoveFor (!coord, !actor) !cave
  | hasAdjacentOpponents coord = (coord, coord)
  | otherwise                  = helper initcset initpaths []
  where
    initcset = S.fromList $ [coord] ++ adjacent coord
    initpaths = [(c,c) | c <- adjacent coord, isOpen (cave A.! c)]

    hasAdjacentOpponents c = not . L.null $ adjacentOpponents (c, actor) cave

    helper :: S.Set Coord -> [Path] -> [Path] -> Path
    helper cset (path@(fc,cc):paths) nextpaths
      | hasAdjacentOpponents cc = path
      | otherwise               = helper cset' paths nextpaths'
        where
          cset'      = L.foldr S.insert cset adjOpen
          nextpaths' = nextpaths ++ [(fc, c') | c' <- adjOpen] -- already in order

          adjSquares = [(c, cave A.! c) | c <- adjacent cc, not (S.member c cset)]

          neighbourOpp = not . L.null $ [() | (_, CaveA opp) <- adjSquares, getType opp /= getType actor]
          adjOpen = [c | (c, CaveF Open) <- adjSquares]
          

    helper cset [] []        = (coord, coord)
    helper cset [] nextpaths = helper cset nextpaths []


damage :: Int -> Actor -> CaveSquare
damage elfpower (Actor otype !hp id)
  | hp <= power = CaveF Open
  | otherwise   = CaveA (Actor otype (hp-power) id)
  where
    power = if otype == Goblin then elfpower else 3


takeTurn :: Int -> Cave -> (Coord, Actor) -> Cave
takeTurn power cave (coord, actor)
  | alreadyDead     = cave
  | otherwise       = cave''
  where
    alreadyDead = case cave A.! coord of
      CaveA someone -> getId actor /= getId someone
      _             -> True

    (target, _) = getMoveFor (coord, actor) cave

    --move
    (coord', cave') = moveInCave cave coord target

    --attack
    cave'' = attackInCave power actor cave' coord'


moveInCave :: Cave -> Coord -> Coord -> (Coord, Cave)
moveInCave cave coord target
  | target == coord               = (coord, cave) -- no move
  | CaveF Open <- cave A.! target = (target, cave')
  | otherwise                     = (coord, cave) -- target is an actor
  where
    CaveA actor = cave A.! coord
    cave' = cave A.// [(coord, CaveF Open), (target, CaveA actor)]


attackInCave :: Int -> Actor -> Cave -> Coord -> Cave
attackInCave power actor cave coord
  | (tc, opp):_ <- sortedTargets = cave A.// [(tc, damage power opp)]
  | otherwise                    = cave
  where
    attackTargets = adjacentOpponents (coord, actor) cave
    sortedTargets = L.sortBy (O.comparing (\(c,o) -> (getHp o, c))) attackTargets


takeRound :: Int -> Cave -> (Cave, Bool)
takeRound p cave = (cave'', someoneWon cave'')
  where
    actors = [(coord, actor) | (coord, CaveA actor) <- A.assocs cave]
    sortedActors = (L.sortBy (O.comparing fst) actors)
    n_actors = length actors
    (firstActors, (lastActor:[])) = L.splitAt (n_actors-1) sortedActors

    cave' = L.foldl' (takeTurn p) cave firstActors
    cave'' = takeTurn p cave' lastActor


someoneWon :: Cave -> Bool
someoneWon cave = all (==head atypes) atypes
  where
    atypes = [getType actor | CaveA actor <- A.elems cave]

countElfs cave = length [() | CaveA (Actor Elf _ _) <- A.elems cave]


solve :: Cave -> ([Cave], Int)
solve cave = loop 4
  where
    originalElfs = countElfs cave
    loop p
      | elfCount == originalElfs && someoneWon finalcave = (cave:caves', p)
      | otherwise = loop (p+1)
      where
        loop' c
          | isDone    = [c']
          | otherwise = c':loop' c'
          where
            (c', isDone) = takeRound p c

        caves' = takeWhile ((==originalElfs) . countElfs) $ loop' cave
        finalcave = last caves'
        elfCount = countElfs finalcave
