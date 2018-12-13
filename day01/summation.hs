

main :: IO ()
main = interact sumLines
     where sumLines = show . sum . map readPlusMinus . lines

readPlusMinus :: String -> Integer
readPlusMinus ('+':rest) = read rest
readPlusMinus all = read all