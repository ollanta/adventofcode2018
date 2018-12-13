import Data.Array as Arr
import Data.List  as L
import Data.Maybe as M
import Data.Function (on)
import Data.Ord (comparing)

main :: IO ()
main = interact (show . solve . readD)


type Coord = (Int,Int)

type Track = Arr.Array Coord Char

data Direction = DUp | DRight | DDown | DLeft
  deriving (Show, Eq)

data Turn = TLeft | TStraight | TRight
  deriving (Show, Eq)

data Cart = Cart Coord Direction [Turn]

instance Show Cart where
  show (Cart coord direction turns) = show "Cart " ++
                                      show coord ++ " " ++
                                      show direction ++ " " ++
                                      show (head turns)

toCart :: (Coord, Char) -> Cart
toCart (coord, c) = Cart coord dir turns
  where
    dir   = case c of
      '<' -> DLeft
      '>' -> DRight
      '^' -> DUp
      'v' -> DDown
    turns = cycle [TLeft, TStraight, TRight]


solve :: (Array Coord Char, [(Coord, Char)]) -> Maybe Cart
solve (track, rawcarts) = loop firstcarts
  where
    firstcarts = map toCart rawcarts

    loop :: [Cart] -> Maybe Cart
    loop []    = Nothing
    loop [c]   = Just c
    loop carts = loop . helper [] . sortBy (comparing getCoord) $ carts

    helper moved (c:rest) 
      | Just m <- find (coordEq c') moved = helper (remcrash m moved) rest
      | Just r <- find (coordEq c') rest  = helper moved (remcrash r rest)
      | otherwise = helper (c':moved) rest
      where
        c' = step1 track c
    helper moved [] = moved

    coordEq = (==) `on` getCoord
    remcrash c l = deleteBy coordEq c l


getCoord :: Cart -> Coord
getCoord (Cart coord _ _) = coord


move :: Cart -> Cart
move (Cart (x,y) dir turns)
  | dir == DUp    = Cart (x,y-1) dir turns
  | dir == DDown  = Cart (x,y+1) dir turns
  | dir == DLeft  = Cart (x-1,y) dir turns
  | dir == DRight = Cart (x+1,y) dir turns


turnDir :: Turn -> Direction -> Direction
turnDir TStraight dir = dir
turnDir TLeft dir
  | dir == DUp   = DLeft
  | dir == DLeft = DDown
  | dir == DDown = DRight
  | dir == DRight = DUp
turnDir TRight dir
  | dir == DUp   = DRight
  | dir == DLeft = DUp
  | dir == DDown = DLeft
  | dir == DRight = DDown


applyTurn :: Turn -> Cart -> Cart
applyTurn turn (Cart coord dir turns) = Cart coord (turnDir turn dir) turns


turn :: Track -> Cart -> Cart
turn track cart@(Cart (x,y) dir (turn:turns))
  | spot `elem` "|-"              = cart
  | spot == '+'                   = applyTurn turn $ Cart (x,y) dir turns
  | spot == '/' && dirVertical    = applyTurn TRight cart
  | spot == '/' && dirHorizontal  = applyTurn TLeft cart
  | spot == '\\' && dirVertical   = applyTurn TLeft cart
  | spot == '\\' && dirHorizontal = applyTurn TRight cart
    where
      spot = track ! (x,y)
      dirVertical = dir == DUp || dir == DDown
      dirHorizontal = not dirVertical


step1 :: Track -> Cart -> Cart
step1 track cart = turn track . move $ cart


readD :: String -> (Array Coord Char, [(Coord, Char)])
readD s = (track, carts)
  where
    rows = lines s

    indexed = [((x,y),c) | (y,row) <- zip [0..] rows, (x,c) <- zip [0..] row]
    maxcoord = foldl1 helper . map fst $ indexed
      where
        helper (x,y) (x',y') = (max x x', max y y')

    tracklist = [(coord, removeCart c) | (coord,c) <- indexed]
      where
        removeCart c
          | c `elem` "<>" = '-'
          | c `elem` "^v" = '|'
          | otherwise = c

    track = Arr.array ((0,0),maxcoord) tracklist

    carts = [(coord, c) | (coord, c) <- indexed, c `elem` "<>^v"]
