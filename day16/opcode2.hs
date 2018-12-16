import Data.Bits as B
import qualified Data.Map.Strict as M
import qualified Data.Set as S

main :: IO ()
main = interact (unlines . map show . solve . readD)


newtype Registers = Reg (Int, Int, Int, Int)
  deriving (Eq, Show)

newtype Opcode = Opc (Int, Int, Int, Int)

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
    readRegs s = Reg . listTo4tuple . read $ drop 8 s
    readOpcode :: String -> Opcode
    readOpcode = Opc . listTo4tuple . map read . words

    program = map readOpcode programlines
      where
        programlines = dropWhile null . drop (4*length tests) $ lined
        
          
readReg :: Int -> Registers -> Int
readReg 0 (Reg (i,_,_,_)) = i
readReg 1 (Reg (_,i,_,_)) = i
readReg 2 (Reg (_,_,i,_)) = i
readReg 3 (Reg (_,_,_,i)) = i


writeReg :: Int -> Int -> Registers -> Registers
writeReg 0 v (Reg (a,b,c,d)) = Reg (v,b,c,d)
writeReg 1 v (Reg (a,b,c,d)) = Reg (a,v,c,d)
writeReg 2 v (Reg (a,b,c,d)) = Reg (a,b,v,d)
writeReg 3 v (Reg (a,b,c,d)) = Reg (a,b,c,v)


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

operationNames = ["addr", "addi", "mulr", "muli", "banr", "bani", "borr", "bori", "setr", "seti", "gtir", "gtii", "gtrr", "eqir", "eqii", "eqrr"]

namedOperations = zip operationNames operations


getOpid :: Testcase -> Int
getOpid (Opc (o,_,_,_),_,_) = o

--solve :: ([Testcase], Program) -> [Registers]
solve (cases, program) = scanl runOp initRegisters program
  where
    opmap = getOpmap cases
    initRegisters = Reg (0,0,0,0)

    runOp :: Registers -> Opcode -> Registers
    runOp regs (Opc (o,a,b,c)) = operation (a,b,c) regs
      where
        operation = opmap M.! o


getOpmap :: [Testcase] -> M.Map Int Operation
getOpmap cases = M.map (oplookup M.!) idmap
  where
    idmap :: M.Map Int String
    idmap = M.map (S.elemAt 0) $ remDefined matchingAll
    oplookup :: M.Map String Operation
    oplookup = M.fromList $ namedOperations
    
    matching = map matchingOperations cases
    opids  = map getOpid cases
    initMap = M.fromAscList $ zip [0..15] (repeat $ S.fromList operationNames)

    intersect (opid, matching) opmap = M.adjust (S.intersection (S.fromList matching)) opid opmap

    matchingAll = foldr intersect initMap (zip opids matching)

    remDefined opmap
      | length singles == M.size opmap = opmap
      | otherwise                      = remDefined opmap'
      where
        singles = filter ((==1) . length . snd) $ M.assocs opmap
        opmap' = foldr (\(opid, matchset) accmap -> M.mapWithKey (remSingle opid matchset) accmap) opmap singles
        remSingle opid matchset otherop othermatchset
          | opid == otherop = matchset
          | otherwise       = S.difference othermatchset matchset
    

matchingOperations :: Testcase -> [String]
matchingOperations (Opc (o,a,b,c), before, after) = map fst matching
  where
    matching :: [(String, Operation)]
    matching = filter ((==after) . apply) namedOperations

    apply :: (String, Operation) -> Registers
    apply (opname, operation) = operation (a,b,c) before
