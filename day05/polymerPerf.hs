import Data.Char
import Data.List

main :: IO ()
main = interact $ show . solveString

solveString :: String -> Int
solveString = solvePolymer . map toPolymer . filter isLetter


solvePolymer :: [Polymer] -> Int
solvePolymer = minimum . map (length . dissolve) . getReducedPolymers . dissolve


getReducedPolymers :: [Polymer] -> [[Polymer]]
getReducedPolymers plist = map (\c -> filter (not . isType c) plist) ['a'..'z']

data Polymer = Small Char | Big Char


toPolymer :: Char -> Polymer
toPolymer c = if (isUpper c) then (Big $ toLower c) else Small c


isType :: Char -> Polymer -> Bool
isType c2 (Small c1) = c1 == toLower c2
isType c2 (Big   c1) = c1 == toLower c2


dissolve1 :: Polymer -> Polymer -> Bool
dissolve1 (Small c1) (Big c2)   = c1 == c2
dissolve1 (Big c1)   (Small c2) = c1 == c2
dissolve1 l r = False


data Direction = DLeft | DRight

dissolve :: [Polymer] -> [Polymer]
dissolve list = dissolve' [] list DRight
  where
    dissolve' :: [Polymer] -> [Polymer] -> Direction -> [Polymer]
    dissolve' old (x:y:rest) DRight
      | dissolve1 x y = dissolve' old rest DLeft
      | otherwise     = dissolve' (x:old) (y:rest) DRight
    dissolve' old@(x:oldRest) new@(y:newRest) DLeft
      | dissolve1 x y = dissolve' oldRest newRest DLeft
      | otherwise     = dissolve' old new DRight
    dissolve' old [x] DRight = x:old
    dissolve' old []  _      = old
    dissolve' []  new DLeft  = dissolve' [] new DRight
