import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map


main :: IO ()
main = interact (show . solve . readSteps)


type StepCondition = (Char, Char)


read1 :: String -> StepCondition
read1 s = (head $ stepwords !! 7, head $ stepwords !! 1)
  where
    stepwords = words s

readSteps :: String -> [StepCondition]
readSteps = map read1 . lines


data Step = Step {
  name :: Char,
  prerequisites :: String
} deriving (Show)


toSteps :: [StepCondition] -> [Step]
toSteps conds = Map.foldrWithKey (\k p ss -> Step k p : ss) [] prereqMap
  where
    stepNames = List.nub $ concatMap (\(a,b) -> [a,b]) conds
    prereqMap :: Map.Map Char String
    prereqMap = Map.fromListWith (++) (zip stepNames (repeat []) ++ [(n,p:[]) | (n,p) <- conds])


solve :: [StepCondition] -> [Char]
solve conds = loop prereqToName stepNames
  where
    stepNames = Set.fromList . List.nub . concatMap (\(a,b) -> [a,b]) $ conds
    prereqToName = Map.fromListWith (++) [(p,n:[]) | (n,p) <- conds]

loop :: Map.Map Char String -> Set.Set Char -> [Char]
loop _   s
  | null s = []
loop lockmap stepsleft = newUnlock : loop (Map.delete newUnlock lockmap) (Set.delete newUnlock stepsleft)
  where
    stepsinmap :: Set.Set Char
    stepsinmap = Set.fromList . foldr (++) [] $ lockmap
    unlockedsteps :: Set.Set Char
    unlockedsteps = Set.difference stepsleft stepsinmap
    newUnlock = Set.findMin unlockedsteps
