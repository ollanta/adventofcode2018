import qualified Data.List as List

main :: IO ()
main = interact (show . solve . readData)


readData :: String -> Int
readData = read


grid = [(x,y) | x <- [1..300], y <- [1..300]]


getSquareHulls :: (Int,Int) -> [[(Int,Int)]]
getSquareHulls (x,y) = map getSquareHull [0..maxd]
  where
    maxd = 300 - max x y
    getSquareHull :: Int -> [(Int,Int)]
    getSquareHull d = [(x',y+d) | x' <- [x..x+d]] ++
                      [(x+d,y') | y' <- [y..y+d-1]]


solve :: Int -> (Int, (Int, Int), Int)
solve serialNo = maximum . concatMap (powerSq serialNo) $ grid


powerSq :: Int -> (Int, Int) -> [(Int, (Int, Int), Int)]
powerSq serialNo coord = zip3 powers (repeat coord) [1..]
  where
    squareHulls = getSquareHulls coord
    hullPowers :: [Int]
    hullPowers  = map (sum . map (power1 serialNo)) squareHulls
    powers      = drop 1 $ List.scanl' (+) 0 hullPowers


power1 :: Int -> (Int, Int) -> Int
power1 serialNo (x,y) = step2 . step1 $ (x,y)
  where
    rackId      = (x+10)
    step1 (x,y) = (rackId*y+serialNo)*rackId
    step2 s     = ((s `div` 100) `mod` 10) - 5
