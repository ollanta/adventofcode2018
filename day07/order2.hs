import qualified Data.Char as Char
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


solve :: [StepCondition] -> [(Int,Char)]
solve conds = loop workerset prereqToName stepNames
  where
    workerset = (Set.fromList [(0,'.')])
    stepNames = Set.fromList . List.nub . concatMap (\(a,b) -> [a,b]) $ conds
    prereqToName = Map.fromListWith (++) [(p,n:[]) | (n,p) <- conds]


loop :: Set.Set (Int, Char) -> Map.Map Char String -> Set.Set Char -> [(Int, Char)]
loop workerset _ stepsleft
  | null stepsleft = Set.toAscList workerset
loop workerset lockmap stepsleft = workersDone ++ loop newWorkerset newLockMap newStepsLeft
  where
    stepsinmap :: Set.Set Char
    stepsinmap = Set.fromList . foldr (++) [] $ newLockMap

    unlockedsteps :: Set.Set Char
    unlockedsteps = Set.difference stepsleft stepsinmap

    currentTime = fst . Set.findMin $ workerset

    workers = Set.toAscList workerset
    workersDone = filter ((==currentTime) . fst) $ workers
    workersLeft = drop (length workersDone) workers
    unlockedNow = map snd workersDone

    newWorkerset = Set.fromList . take nWorkers $ workersLeft ++ queuedWorkers

    newLockMap = foldr Map.delete lockmap unlockedNow

    beingWorkedOn = map snd . Set.toList $ newWorkerset
    newStepsLeft = foldr Set.delete stepsleft beingWorkedOn
    

    queuedWorkers = map (\c -> (currentTime + toSeconds c, c)) (Set.toAscList unlockedsteps)

toSeconds :: Char -> Int
toSeconds c = 60 + Char.ord c - Char.ord 'A' + 1


nWorkers = 5
