module D21
    ( Memory(..), Parameters(..), Instruction(..), Execution
    , ProgramCounter(..), Program(..)
    , readProgram, registerIP, runProgram, endProgram, initialMemory
    , hackProgram1, setR, hackProgram2
    ) where

import Data.List
import Data.Bits
import Text.Regex
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import Data.Tuple

{- ########### device memory ########### -}

data Memory = Memory  { ip :: Int
                      , r0 :: Int
                      , r1 :: Int
                      , r2 :: Int
                      , r3 :: Int
                      , r4 :: Int
                      , r5 :: Int
                      , trace :: M.Map Int Int } deriving (Show,Eq)

getR :: Int -> Memory -> Int
getR 0 (Memory _ r0 _  _  _  _  _  _) = r0
getR 1 (Memory _ _  r1 _  _  _  _  _) = r1
getR 2 (Memory _ _  _  r2 _  _  _  _) = r2
getR 3 (Memory _ _  _  _  r3 _  _  _) = r3
getR 4 (Memory _ _  _  _  _  r4 _  _) = r4
getR 5 (Memory _ _  _  _  _  _  r5 _) = r5

setR :: Int -> Int -> Memory -> Memory
setR 0 v m = m { r0 = v }
setR 1 v m = m { r1 = v }
setR 2 v m = m { r2 = v }
setR 3 v m = m { r3 = v }
setR 4 v m = m { r4 = v }
setR 5 v m = m { r5 = v }

setIP :: Int -> Memory -> Memory
setIP v m = m { ip = v }

addR :: Int -> Int -> Memory -> Memory
addR r v m = setR r ((getR r m)+v) m

{- ########### device instructions ########### -}

data Parameters = Parameters  { a :: Int
                              , b :: Int
                              , c :: Int } deriving (Show, Eq)

type Operator = Int -> Int -> Int
type Execution = Parameters -> Memory -> Memory
   
data Instruction = Instruction  { execution :: Execution
                                , params :: Parameters }

runOprr :: Operator -> Execution
runOprr f (Parameters a b c) m = setR c (f (getR a m) (getR b m)) m

runOpri :: Operator -> Execution
runOpri f (Parameters a b c) m = setR c (f (getR a m) b) m

runOpir :: Operator -> Execution
runOpir f (Parameters a b c) m = setR c (f a (getR b m)) m

runOp_rr_ri_ir :: Operator -> (Execution,Execution,Execution)
runOp_rr_ri_ir f = (runOprr f, runOpri f, runOpir f)

runEnum :: (Enum a) => (Int -> Int -> a) -> Operator
runEnum f x y = fromEnum $ f x y

(runAddr,runAddi,_) = runOp_rr_ri_ir (+)
(runMulr,runMuli,_) = runOp_rr_ri_ir (*)
(runBanr,runBani,_) = runOp_rr_ri_ir (.&.)
(runBorr,runBori,_) = runOp_rr_ri_ir (.|.)
(runSetr,_,runSeti) = runOp_rr_ri_ir const
(runGtrr,runGtri,runGtir) = runOp_rr_ri_ir $ runEnum (>)
(runEqrr,runEqri,runEqir) = runOp_rr_ri_ir $ runEnum (==)

allExecutions = M.fromList  [("addr",runAddr)
                            ,("addi",runAddi)
                            ,("mulr",runMulr)
                            ,("muli",runMuli)
                            ,("banr",runBanr)
                            ,("bani",runBani)
                            ,("borr",runBorr)
                            ,("bori",runBori)
                            ,("setr",runSetr)
                            ,("seti",runSeti)
                            ,("gtrr",runGtrr)
                            ,("gtri",runGtri)
                            ,("gtir",runGtir)
                            ,("eqrr",runEqrr)
                            ,("eqri",runEqri)
                            ,("eqir",runEqir)] :: M.Map String Execution

data ProgramCounter = ProgramCounter { writeToRegister :: Memory -> Memory
                                     , readFromRegister :: Memory -> Memory }

neutralIP :: ProgramCounter
neutralIP = ProgramCounter id (\m -> m { ip = (ip m)+1 })

registerIP :: Int -> ProgramCounter
registerIP r = ProgramCounter wtr rfr
  where wtr m = setR r (ip m) m
        rfr m = let m' = addR r 1 m in setIP (getR r m') m'
                                                                      
data Program = Program { ipAction :: ProgramCounter
                       , instructions :: V.Vector Instruction }

{- ########### Run programs ########### -}

initialMemory :: Memory
initialMemory = Memory 0 0 0 0 0 0 0 M.empty

endProgram :: Memory -> Program -> Memory
endProgram memory program@(Program (ProgramCounter wtr rfr) instructions) = finalMemory
  where finalMemory = head $ dropWhile keepRunning $ runProgram memory program
        keepRunning memory = (ip memory) < (length instructions)

runProgram :: Memory -> Program -> [Memory]
runProgram memory (Program (ProgramCounter wtr rfr) instructions) = iterate execute memory
  where execute memory = rfr $ (runInstruction instructions) $ wtr memory
            
runInstruction :: V.Vector Instruction -> Memory -> Memory
runInstruction instructions memory = execute params memory
  where (Instruction execute params) = instructions V.! (ip memory)

{- ########### factories ########### -}

readProgram :: String -> Program
readProgram s = Program (registerIP ipRegister) $ V.fromList $ map readInstruction instructionLines
  where instructionLines = drop 1 $ lines s
        ipRegister = read $ drop 4 $ head $ lines s

readInstruction :: String -> Instruction
readInstruction = readParams . extractFromRegex "([a-z]+) ([0-9]+) ([0-9]+) ([0-9]+)"
  where readParams (inst:a:b:c:_) = Instruction (allExecutions M.! inst) (Parameters (read a) (read b) (read c))
      
extractFromRegex :: String -> String -> [String]
extractFromRegex regex def = (\(Just (_,_,_,subs)) -> subs) $ matchRegexAll (mkRegex regex) def

{- ########### Hack part 1 ########### -}

hackProgram1 :: Program -> Program
hackProgram1 (Program ipa instructions) = (Program ipa newInstructions)
  where newInstructions = V.take ((V.length instructions)-1) instructions

{- ########### Hack part 2 ########### -}

hackProgram2 :: Program -> Program
hackProgram2 (Program ipa instructions) = (Program ipa newInstructions)
  where l = V.length instructions
        newInstructions = (V.take (l-1) instructions) V.++ addedInstructions
        addedInstructions = V.fromList  [ Instruction sniffer $ Parameters 0 0 0
                                        , instructions V.! (l-1) ]

sniffer :: Parameters -> Memory -> Memory
sniffer p m = dejaVu $ M.lookup (r4 m) ctrace
  where ctrace = trace m
        dejaVu Nothing = m { trace = M.insert (r4 m) (M.size ctrace) ctrace }
        dejaVu (Just n) = Memory n n n n n n n $ M.fromList [highest]
        highest = last $ sortOn snd $ M.toAscList ctrace
        n = fst highest
