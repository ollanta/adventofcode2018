import qualified Data.Map as M
import qualified Data.Ord as O
import qualified Data.List as L
import Debug.Trace

main :: IO ()
main = interact (showD . solve . readD)


type AType = String

type Group = (Int, Int, [AType], [AType], Int, AType, Int)

type Army = [Group]


readD s = (map readGroup immuneGroups, map readGroup infectionGroups)
  where
    lined = lines s
    immuneGroups = takeWhile (not . null) . drop 1 $ lined

    infectionGroups = drop 2 . dropWhile (not . null) $ lined


readGroup :: String -> Group
readGroup s = (n, hp, immunities, weaknesses, attack, attacktype, initiative)
  where
    worded = words s
    n = read $ worded !! 0
    hp = read $ worded !! 4

    (immunities, weaknesses) = readImmunAndWeak . drop 1 . takeWhile (/=')') . dropWhile (/='(') $ s

    nwords = length worded
    attack     = read $ worded !! (nwords-6)
    attacktype = worded !! (nwords-5)
    initiative = read $ worded !! (nwords-1)


readImmunAndWeak :: String -> ([AType], [AType])
readImmunAndWeak "" = ([], [])
readImmunAndWeak s
  | elem ';' s = (max immun immun', max weak weak')
  | otherwise  = (immun, weak)
  where
    worded = words . filter (/=',') . takeWhile (/=';') $ s

    atypes = drop 2 worded
    immun  = if worded !! 0 == "immune" then atypes else []
    weak   = if worded !! 0 == "weak" then atypes else []

    (immun', weak') = readImmunAndWeak . drop 1 . dropWhile (/=';') $ s


--group (n, hp, immunities, weaknesses, attack, attacktype, initiative)
effectivePower :: Group -> Int
effectivePower (n, _, _, _, a, _, _) = n*a

getUnits      (n, _, _, _, _, _, _) = n

getHp         (_, hp, _, _, _, _, _) = hp

getImmunities (_, _, imm, _, _, _, _) = imm

getWeaknesses (_, _, _, weak, _, _, _) = weak

getAttackType (_, _, _, _, _, atype, _) = atype

getInitiative (_, _, _, _, _, _, init) = init


getDamage attacker defender
  | atype `elem` weaknesses = 2 * power
  | atype `elem` immunities = 0
  | otherwise                 = power
  where
    power = effectivePower attacker
    atype = getAttackType attacker
    weaknesses = getWeaknesses defender
    immunities = getImmunities defender


dealDamage :: Group -> Group -> Group
dealDamage attacker defender@(n, hp, imm, weak, att, atype, init) = defender'
  where
    defender' = (n-losses, hp, imm, weak, att, atype, init)
    losses    = damage `quot` hp
    damage    = getDamage attacker defender


solve (imm, inf) = loop 0
  where
    loop b
      | null inf' = [(b,imm',inf')]
      | otherwise = (b,imm',inf'):loop (b+1)
      where
        (imm', inf') = last $ fight (boost b imm, inf)

    boost b = map (\(n, hp, imm, weak, att, atype, init) -> (n, hp, imm, weak, att+b, atype, init))


fight (immuneSystem, infection) = loop immuneSystem infection
  where
    loop imm inf
      | imm == imm' && inf == inf' = [(imm, inf)]
      | otherwise = (imm, inf):loop imm' inf'
      where
        (imm', inf') = takeRound (imm, inf)


takeRound :: ([Group], [Group]) -> ([Group], [Group])
takeRound (immuneSystem, infection) = (imm', inf')
  where
    immIds = zip [0..] immuneSystem
    infIds = zip [length immuneSystem..] infection

    immTargets = selectTargets immIds infIds
    infTargets = selectTargets infIds immIds

    groupMap = M.fromList $ immIds ++ infIds
    targetMap = M.union immTargets infTargets

    groupMap' = attackTargets groupMap targetMap

    getAliveGroups = filter ((>0) . getUnits) . map ((groupMap' M.!) . fst)
    imm' = getAliveGroups $ immIds
    inf' = getAliveGroups $ infIds


selectTargets :: [(Int, Group)] -> [(Int, Group)] -> M.Map Int Int
selectTargets att def = helper sorted def M.empty
  where
    attOrderer (id, group) = (-effectivePower group, -getInitiative group)
    sorted = L.sortBy (O.comparing attOrderer) att

    defOrderer attGr (id, group) = (-getDamage attGr group, -effectivePower group, -getInitiative group)

    helper :: [(Int, Group)] -> [(Int, Group)] -> M.Map Int Int -> M.Map Int Int
    helper [] _ tmap = tmap
    helper ((id,gr):grs) targets tmap
      | null filteredTargets = helper grs targets tmap
      | otherwise            = helper grs targets'' tmap'
      where
        targets'            = L.sortBy (O.comparing (defOrderer gr)) targets
        filteredTargets     = filter ((>0) . getDamage gr . snd) targets'
        ((tid,_):targets'') = targets'
        tmap'               = M.insert id tid tmap


attackTargets :: M.Map Int Group -> M.Map Int Int -> M.Map Int Group
attackTargets groupMap targetMap = helper sortedIds groupMap
  where
    attOrderer (id, att) = -getInitiative att
    sortedIds = map fst . L.sortBy (O.comparing attOrderer) $ M.toList groupMap

    helper [] grMap = grMap
    helper (id:ids) grMap
      | M.notMember id targetMap = helper ids grMap
      | getUnits attacker <= 0   = helper ids grMap
      | otherwise                = helper ids grMap'
      where
        attacker = grMap M.! id
        targetId = targetMap M.! id
        grMap'   = M.adjust (dealDamage attacker) targetId grMap


showD :: [(Int,Army,Army)] -> String
showD = unlines . map showR
  where
    showR (boost, imm, inf) = show (boost, countUnits imm, countUnits inf)
    countUnits = sum . map getUnits


every n list = every' n list
  where
    every' _ [l]    = [l]
    every' 0 (l:ls) = l : every' n ls
    every' k (l:ls) = every' (k-1) ls
