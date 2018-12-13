import qualified Data.List as L

main :: IO ()
main = interact (showD . solve . readD)


readD :: String -> (String, [(String, Char)])
readD s = (initState, rules)
  where
    ls = lines s
    initState = drop 15 $ head ls

    ruless = drop 2 ls
    rules = map (\s -> (take 5 s, last s)) ruless



generation :: [(String, Char)] -> (String,Int) -> (String,Int)
generation rules (s,i0) = trim . helper $ "...."++s++"...."
  where
    trim s = (s', i0-2+c)
      where
        s' = reverse . dropWhile (=='.') . reverse . dropWhile (=='.') $ s
        c  = length . takeWhile (=='.') $ s

    helper (a:rest@(b:c:d:e:_)) = ruling (a:b:c:d:e:[]) : helper rest
    helper _ = []

    ruling s
      | Just (s', c) <- rule = c
      | otherwise            = '.'
      where
        rule = L.find ((==s).fst) rules
        

maxG :: Int
maxG = 50000000000


solve :: (String, [(String, Char)]) -> (String, Int)
solve (initState, rules) = (news, ms)
  where
    states = iterate (generation rules) (initState,0)

    (news, newi0) = helper' 0 $ (take (maxG+1) states)
    --(news, newi0) = helper' $ take 21 states

    helper' g ((s,c):rest@((s',c'):_))
      | s == s'   = (s,mc)
      | otherwise = helper' (g+1) rest
      where
        mc = c + (c'-c) * (maxG-g)
    helper' g ((s,c):[]) = (s,c)
      

    ms = sum . map fst . filter ((=='#').snd) $ zip [newi0..] news
    


showD (s, int) = unlines [s, show int]
