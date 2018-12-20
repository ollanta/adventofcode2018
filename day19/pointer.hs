import Data.Bits as B
import qualified Data.Map as M
import qualified Data.Array as A

main :: IO ()
main = interact (showD . solve . readD)

type Registers = (Int, Int, Int, Int, Int, Int)

type Opcode = (String, Int, Int, Int)

type Program = A.Array Int Opcode

listTo4tuple (a:b:c:d:_) = (a,b,c,d)

readD :: String -> (Int, Program)
readD s = (ipointer, program)
  where
    lined = lines s

    ipointer = read . drop 3 . head $ lined

    readOpcode :: String -> Opcode
    readOpcode s = (op, read a, read b, read c)
      where
        (op,a,b,c) = listTo4tuple . words $ s

    programlines = tail lined
    program = A.listArray (0, length programlines - 1) (map readOpcode programlines)
        
          
readReg :: Int -> Registers -> Int
readReg 0 (i,_,_,_,_,_) = i
readReg 1 (_,i,_,_,_,_) = i
readReg 2 (_,_,i,_,_,_) = i
readReg 3 (_,_,_,i,_,_) = i
readReg 4 (_,_,_,_,i,_) = i
readReg 5 (_,_,_,_,_,i) = i


writeReg :: Int -> Int -> Registers -> Registers
writeReg 0 v (a,b,c,d,e,f) = (v,b,c,d,e,f)
writeReg 1 v (a,b,c,d,e,f) = (a,v,c,d,e,f)
writeReg 2 v (a,b,c,d,e,f) = (a,b,v,d,e,f)
writeReg 3 v (a,b,c,d,e,f) = (a,b,c,v,e,f)
writeReg 4 v (a,b,c,d,e,f) = (a,b,c,d,v,f)
writeReg 5 v (a,b,c,d,e,f) = (a,b,c,d,e,v)

type Operation = (Int, Int, Int) -> Registers -> Registers

funcToOperation :: Func -> Operation
funcToOperation func = operation
  where
    operation (a, b, c) regs = writeReg c result regs
      where
        result = func a b regs

type Func = (Int -> Int -> Registers -> Int)

addr :: Func
addr a b r = readReg a r + readReg b r

addi :: Func
addi a b r = readReg a r + b

mulr a b r = readReg a r * readReg b r

muli a b r = readReg a r * b

banr a b r = readReg a r .&. readReg b r

bani a b r = readReg a r .&. b

borr a b r = readReg a r .|. readReg b r

bori a b r = readReg a r .|. b

setr a b r = readReg a r

seti a b r = a

gtir a b r = if a > readReg b r then 1 else 0

gtii a b r = if readReg a r > b then 1 else 0

gtrr a b r = if readReg a r > readReg b r then 1 else 0

eqir a b r = if a == readReg b r then 1 else 0

eqii a b r = if readReg a r == b then 1 else 0

eqrr a b r = if readReg a r == readReg b r then 1 else 0


operations = map funcToOperation [addr, addi, mulr, muli, banr, bani, borr, bori ,setr, seti, gtir, gtii, gtrr, eqir, eqii, eqrr]

namedOperations = M.fromList $ zip ["addr", "addi", "mulr", "muli", "banr", "bani", "borr", "bori", "setr", "seti", "gtir", "gtii", "gtrr", "eqir", "eqii", "eqrr"] operations

runOp :: (String, Int, Int, Int) -> Registers -> Registers
runOp (op, a, b, c) regs = operation (a,b,c) regs
  where
    operation = namedOperations M.! op

solve :: (Int, Program) -> [Registers]
solve (ipointer, program) = helper initregisters
  where
    --initregisters = (0,10551343,0,11,10551343,10551342)
    initregisters = (0,0,0,0,0,0)
    (minI, maxI) = A.bounds program

    helper regs
      | instr < minI = [regs]
      | instr > maxI = [regs]
      | otherwise    = regs : helper regs''
      where
        instr  = readReg ipointer regs
        regs'  = runOp (program A.! instr) regs
        instr' = readReg ipointer regs' + 1
        regs'' = writeReg ipointer instr' regs' 


showD = unlines . map show . skippy n --unlines . map show
  where
    n = 0
    skippy _ [x]    = [x]
    skippy 0 (x:xs) = x:skippy n xs
    skippy i (x:xs) = skippy (i-1) xs
