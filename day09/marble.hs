import qualified Data.Map.Strict as Map


main :: IO ()
main = interact (showInput . solve . readInput)


showInput = show


readInput :: String -> (Int, Int)
readInput s = (players, marbles)
  where
    players = read . (!!0) $ words s
    marbles = (+1) . read . (!!6) $ words s


data Circle = Circle {
  previous :: [Int],
  next     :: [Int]
} deriving (Show)


data Direction = DLeft | DRight


rotate :: Direction -> Circle -> Circle
rotate DRight (Circle [] [])         = Circle [] []
rotate DRight (Circle prev [])       = rotate DRight (Circle [] $ reverse prev)
rotate DRight (Circle prev (n:rest)) = Circle (n:prev) rest
rotate DLeft (Circle prev next)      = Circle prev' next'
  where
    Circle next' prev' = rotate DRight (Circle next prev)


rotateN :: Int -> Direction -> Circle -> Circle
rotateN 0 d c = c
rotateN n d c = rotateN (n-1) d $ rotate d c


takeCurrentMarble :: Circle -> (Int, Circle)
takeCurrentMarble (Circle [] []) = undefined
takeCurrentMarble (Circle (m:prev) next) = (m, Circle prev next)
takeCurrentMarble (Circle [] next) = takeCurrentMarble (Circle (reverse next) [])


place :: Int -> Circle -> (Int, Circle)
place m circle
  | m `mod` 23 == 0 = (m + removedmarble, circle''')
  where
    circle' = rotateN 7 DLeft circle
    (removedmarble, circle'') = takeCurrentMarble circle'
    circle''' = rotate DRight circle''

place m circle = (0, circle')
  where
    circle' = let Circle prev next = rotateN 1 DRight circle
              in Circle (m:prev) next


solve :: (Int, Int) -> Int
solve (players, lastmarble) = maximum . playerscores players $ movescores lastmarble


movescores :: Int -> [Int]
movescores lastmarble = play marbles startcircle
  where
    marbles    = [1..lastmarble]
    startcircle = Circle [0] []
    play [] c = []
    play (m:rest) c = let (score, c') = place m c in
                        score:play rest c'


playerscores :: Int -> [Int] -> [Int]
playerscores players movescores = sumplayer [] (replicate players 0) movescores
  where
    sumplayer psl psr [] = (reverse psl) ++ psr
    sumplayer psl []  ms = sumplayer [] (reverse psl) ms
    sumplayer psl (p:psr) (m:ms) = sumplayer (p+m:psl) psr ms
