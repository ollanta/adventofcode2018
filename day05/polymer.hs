import Data.Char

main :: IO ()
main = interact $ show . length . dissolve . map toPolymer . filter isLetter


data Polymer = Small Char | Big Char


toPolymer :: Char -> Polymer
toPolymer c = if (isUpper c) then (Big $ toLower c) else Small c


dissolve1 :: Polymer -> Polymer -> Bool
dissolve1 (Small c1) (Big c2)   = c1 == c2
dissolve1 (Big c1)   (Small c2) = c1 == c2
dissolve1 l r = False


dissolve :: [Polymer] -> [Polymer]
dissolve list = loop True list
  where
    loop False list = list
    loop True  list = let (b, list') = dissolve' (False, list) in loop b list'
    dissolve' (b, x:y:rest) = let (b', rest') = dissolve' (b, y:rest) in
      if (dissolve1 x y) then (True, snd $ dissolve' (False, rest)) else (b', x:rest')
    dissolve' (b, rest)     = (b, rest)
