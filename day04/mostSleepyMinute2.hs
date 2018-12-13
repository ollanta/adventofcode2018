import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Ord as Ord


main :: IO ()
main = interact (showData . solve . readData)


showData :: (Guard, (Int, Int)) -> String
showData all@(Guard gid, (minute, freq)) = unlines $ [resString, sumString]
  where
    resString = show all
    sumString = show (gid * minute)

data Timestamp = Timestamp {
  getYear :: Int,
  getMonth :: Int,
  getDay :: Int,
  getHour :: Int,
  getMinute :: Int
} deriving (Show, Eq, Ord)

data Action = Waking | Sleeping
  deriving (Show, Eq, Ord)

data Guard = Guard Int
  deriving (Show, Eq, Ord)
  
data LogType = GuardLog Guard | ActionLog Action
  deriving (Show, Eq, Ord)

readTimestamp :: String -> Timestamp
readTimestamp s = Timestamp year month day hour minute
  where
    date:time:[] = words s
    splitsAt c s = map (takeWhile (/=c)) . iterate (drop 1.dropWhile (/=c)) $ s
    year:month:day:_ = map read . splitsAt '-' $ date
    hour:minute:_ = map read . splitsAt ':' $ time


readData = map readLine . lines

readLine :: String -> (Timestamp, LogType)
readLine s = (readTimestamp timestampString, parseLogType logtypeString)
  where
    timestampString = drop 1 . takeWhile (/=']') $ s
    logtypeString = drop 2 . dropWhile (/=']') $ s

    parseLogType "falls asleep" = ActionLog Sleeping
    parseLogType "wakes up"     = ActionLog Waking
    parseLogType s              = GuardLog . Guard . read . drop 1 . flip (!!) 1 . words $ s


mostCommon :: (Eq a, Ord a) => [a] -> a
mostCommon = head . List.maximumBy (Ord.comparing length) . List.group . List.sort

mostCommonCount :: (Eq a, Ord a) => [a] -> (a, Int)
mostCommonCount l = (head commonlist, length commonlist)
  where
    commonlist = List.maximumBy (Ord.comparing length) . List.group . List.sort $ l


solve :: [(Timestamp, LogType)] -> (Guard, (Int, Int))
solve loglist = (winningGuard, mostFrequentMinute winningGuard)
  where
    groupedLogs :: [(Guard, [(Timestamp, Action)])]
    groupedLogs = groupLogs . List.sort $ loglist
    minutesAsleep g = concatMap (generateMinutes . snd) . filter ((==g) . fst) $ groupedLogs

    mostFrequentMinute g = mostCommonCount $ minutesAsleep g

    guards = List.nub . map fst $ groupedLogs
    sleepyGuards = filter (not . List.null . minutesAsleep) guards

    winningGuard = List.maximumBy (Ord.comparing (snd . mostFrequentMinute)) sleepyGuards


generateMinutes :: [(Timestamp, Action)] -> [Int]
generateMinutes ((ts1, Sleeping):(ts2, Waking):rest) = [getMinute ts1..getMinute ts2 - 1]
                                                       ++ generateMinutes rest
generateMinutes [] = []


groupLogs :: [(Timestamp, LogType)] -> [(Guard, [(Timestamp, Action)])]
groupLogs []                        = []
groupLogs ((ts, GuardLog g):rest) = (g, map toAction actions):groupLogs rest'
  where
    (actions, rest') = span (not . isGuardLog . snd) rest

    toAction (ts, ActionLog a) = (ts, a)

    isGuardLog (GuardLog _) = True
    isGuardLog _            = False
