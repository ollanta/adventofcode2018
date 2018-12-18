import Data.Array as A

main :: IO ()
main = interact (showD . solve . readD)


data Feature = Lumberyard | Open | Tree
  deriving (Show, Eq)

-- (y,x)
type Coord = (Int, Int)

type TMap = A.Array Coord Feature


--readD :: String -> TMap
readD s = A.array ((1,1),(height,width)) [((y,x), readF f) |
                                          (y,row) <- zip [1..] lined,
                                          (x,f)   <- zip [1..] row]
  where
    lined = map (filter (`elem` ".|#")) . lines $ s

    readF '.' = Open
    readF '|' = Tree
    readF '#' = Lumberyard

    height = length lined
    width  = length . head $ lined
    

neighbourhood :: Coord -> TMap -> [Feature]
neighbourhood (x,y) tmap = map (tmap A.!) neighbours
  where
    ((minY,minX),(maxY,maxX)) = A.bounds tmap
    neighbours = [(x',y') | x' <- [x-1..x+1], y' <- [y-1..y+1],
                  x'>=minX, x'<=maxX, y'>=minY, y'<=maxY, not (x==x' && y==y')]


updateMap :: TMap -> TMap
updateMap tmap = A.array abounds [(c, update c f) | (c,f) <- A.assocs tmap]
  where
    abounds = A.bounds tmap
    ((minY,minX),(maxY,maxX)) = abounds

    update :: Coord -> Feature -> Feature
    update c Open
      | nadjacent c Tree >= 3 = Tree
      | otherwise          = Open
    update c Tree
      | nadjacent c Lumberyard >= 3 = Lumberyard
      | otherwise           = Tree
    update c Lumberyard
      | nadjacent c Tree >= 1 && nadjacent c Lumberyard >= 1 = Lumberyard
      | otherwise           = Open

    nadjacent :: Coord -> Feature -> Int
    nadjacent c t = length [() | t' <- neighbourhood c tmap, t==t']


solve tmap = iterate updateMap tmap


showF :: Feature -> Char
showF Open = '.'
showF Tree = '|'
showF Lumberyard = '#'


showD tmaps = unlines . map showMap $ zip [0..] (take 1301 $ tmaps)
  where
    showMap (i,tmap) = show (i, trees, yards)
      where
        ((minY,minX),(height,width)) = A.bounds tmap
        structure "" = []
        structure s = take width s : structure (drop width s)

        mapstrings = structure . map showF . A.elems $ tmap

        trees = length [() | Tree <- A.elems tmap]
        yards = length [() | Lumberyard <- A.elems tmap]
