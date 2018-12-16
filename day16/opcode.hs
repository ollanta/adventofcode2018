import Data.Bits as B

main :: IO ()
main = interact (show . solve . readD)


type Registers = (Int, Int, Int, Int)

type Opcode = (Int, Int, Int, Int)

type Testcase = (Opcode, Registers, Registers)

type Program = [Opcode]


listTo4tuple (a:b:c:d:_) = (a,b,c,d)

readD :: String -> ([Testcase], Program)
readD s = (tests, program)
  where
    lined = lines s

    tests = looptestcase lined

    looptestcase (before:op:after:empty:rest)
      | hasRead   = testcase:looptestcase rest
      | otherwise = []
      where
        hasRead = "Before:" == take 7 before
  
        testcase = (readOpcode op, readRegs before, readRegs after)

    readRegs :: String -> Registers
    readRegs s = listTo4tuple . read $ drop 8 s
    readOpcode :: String -> Opcode
    readOpcode = listTo4tuple . map read . words

    program = map readOpcode programlines
      where
        programlines = dropWhile null . drop (4*length tests) $ lined
        
          
readReg :: Int -> Registers -> Int
readReg 0 (i,_,_,_) = i
readReg 1 (_,i,_,_) = i
readReg 2 (_,_,i,_) = i
readReg 3 (_,_,_,i) = i


writeReg :: Int -> Int -> Registers -> Registers
writeReg 0 v (a,b,c,d) = (v,b,c,d)
writeReg 1 v (a,b,c,d) = (a,v,c,d)
writeReg 2 v (a,b,c,d) = (a,b,v,d)
writeReg 3 v (a,b,c,d) = (a,b,c,v)


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

namedOperations = zip ["addr", "addi", "mulr", "muli", "banr", "bani", "borr", "bori", "setr", "seti", "gtir", "gtii", "gtrr", "eqir", "eqii", "eqrr"] operations


--solve :: ([Testcase], Program) -> Int
solve (cases, program) = length . filter ((>=3) . length) $ matching
  where
    matching = map matchingOperations cases

matchingOperations :: Testcase -> [String]
matchingOperations (opcode@(o,a,b,c), before, after) = map fst matching
  where
    matching :: [(String, Operation)]
    matching = filter ((==after) . apply) namedOperations

    apply :: (String, Operation) -> Registers
    apply (opname, operation) = operation (a,b,c) before
