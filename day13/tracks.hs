import Data.Array as Arr
import Data.List as L
import Data.Maybe as M

main :: IO ()
main = interact (show . solve . readD)

type Coord = (Int,Int)

type Track = Arr.Array Coord Char

data Direction = DUp | DDown | DLeft | DRight
  deriving (Show, Eq)

data Turn = TLeft | TStraight | TRight
  deriving (Show, Eq)

data Cart = Cart Coord Direction [Turn]
  deriving (Eq)

instance Show Cart where
  show (Cart coord direction turns) = show "Cart " ++
                                      show coord ++ " " ++
                                      show direction ++ " " ++
                                      show (take 3 turns)


toCart :: (Coord, Char) -> Cart
toCart (coord, c)
  | c == '<' = Cart coord DLeft turns
  | c == '>' = Cart coord DRight turns
  | c == '^' = Cart coord DUp turns
  | c == 'v' = Cart coord DDown turns
  where
    turns = cycle [TLeft, TStraight, TRight]


--solve :: (Array Coord Char, [(Coord, Char)])
solve (track, rawcarts) = firstCollision
  where
    carts = map toCart rawcarts

    steps = iterate (step1 track) carts

    firstCollision = M.fromJust . head . dropWhile M.isNothing . map hasCollision $ steps


hasCollision :: [Cart] -> Maybe Coord
hasCollision carts = helper . sort . map getcoord $ carts
  where
    getcoord (Cart coord _ _) = coord
    helper (c1:rest@(c2:_))
      | c1 == c2  = Just c1
      | otherwise = helper rest
    helper _ = Nothing


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
  | spot == '|'               = cart
  | spot == '-'               = cart
  | spot == '/' && (dir == DUp || dir == DDown) = applyTurn TRight cart
  | spot == '/' && (dir == DRight || dir == DLeft) = applyTurn TLeft cart
  | spot == '\\' && (dir == DUp || dir == DDown) = applyTurn TLeft cart
  | spot == '\\' && (dir == DRight || dir == DLeft) = applyTurn TRight cart
  | spot == '+'              = applyTurn turn $ Cart (x,y) dir turns
    where spot = track ! (x,y)


step1 :: Track -> [Cart] -> [Cart]
step1 track carts = map (turn track . move) carts


-- get track and carts
readD :: String -> (Array Coord Char, [(Coord, Char)])
readD s = (track, carts)
  where
    rows = lines s

    indexed = [((x,y),c) | (y,row) <- zip [0..] rows, (x,c) <- zip [0..] row]
    maxcoord = L.foldl1' helper . map fst $ indexed
      where
        helper (x,y) (x',y') = (max x x', max y y')

    tracklist = [(coord, removeCart c) | (coord,c) <- indexed]
      where
        removeCart c
          | c == '<' = '-'
          | c == '>' = '-'
          | c == '^' = '|'
          | c == 'v' = '|'
          | otherwise = c
    track = Arr.array ((0,0),maxcoord) tracklist

    carts = [(coord, c) | (coord, c) <- indexed, isCart c]
      where
        isCart c
          | c == '<' = True
          | c == '>' = True
          | c == 'v' = True
          | c == '^' = True
          | otherwise = False
