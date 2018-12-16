module D16
    ( Memory(..), Parameters(..), Instruction(..), Execution, Sample(..)
    , countMatchingExecutions, countSamplesWithMatchingExecutions, readSamples
    , findOpcodeCandidates, reduceOpcodeCandidates
    , createDevice, readProgram, runProgram
    ) where

import Data.List
import Data.Bits
import Text.Regex

{- ########### device memory ########### -}

data Memory = Memory  { r0 :: Int
                      , r1 :: Int
                      , r2 :: Int
                      , r3 :: Int } deriving (Show, Eq)

getR :: Int -> Memory -> Int
getR 0 (Memory r0 _  _  _ ) = r0
getR 1 (Memory _  r1 _  _ ) = r1
getR 2 (Memory _  _  r2 _ ) = r2
getR 3 (Memory _  _  _  r3) = r3

setR :: Int -> Int -> Memory -> Memory
setR 0 v m = m { r0 = v }
setR 1 v m = m { r1 = v }
setR 2 v m = m { r2 = v }
setR 3 v m = m { r3 = v }

{- ########### device instructions ########### -}

data Parameters = Parameters  { a :: Int
                              , b :: Int
                              , c :: Int } deriving (Show, Eq)

type Operator = Int -> Int -> Int
type Execution = Parameters -> Memory -> Memory
   
data Instruction = Instruction  { opcodeValue :: Int
                                , params :: Parameters } deriving (Show, Eq)

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

allExecutions = [runAddr,runAddi
                ,runMulr,runMuli
                ,runBanr,runBani
                ,runBorr,runBori
                ,runSetr,runSeti
                ,runGtrr,runGtri,runGtir
                ,runEqrr,runEqri,runEqir] :: [Execution]

{- ########### part 1 ########### -}

data Sample = Sample  { before :: Memory
                      , instruction :: Instruction
                      , after :: Memory }

countMatchingExecutions :: Sample -> Int
countMatchingExecutions (Sample m1 (Instruction _ p) m2) = length $ filter ((==) True) $ map tryout allExecutions
  where tryout ex = (ex p m1) == m2

countSamplesWithMatchingExecutions :: (Int -> Bool) -> [Sample] -> Int
countSamplesWithMatchingExecutions f xs = length $ filter f $ map countMatchingExecutions xs

{- ########### part 2 ########### -}

findOpcodeCandidates :: [Sample] -> [[Int]]
findOpcodeCandidates samples = map findOpcodesFor allExecutions
  where findOpcodesFor run = filter (isValidFor run) allOpcodes
        isValidFor run = and . map (trySample run) . samplesForOpcode
        trySample run (Sample m1 (Instruction _ p) m2) = (run p m1) == m2
        samplesForOpcode opc = filter (((==)opc).opcodeValue.instruction) samples
        allOpcodes = nub $ map (opcodeValue.instruction) samples

reduceOpcodeCandidates :: [[Int]] -> [Int]
reduceOpcodeCandidates cs = concat $ head $ dropWhile remainUnknowns $ iterate removeKnownsForAll cs
  where knowns xs = concat $ filter (((==)1).length) xs :: [Int]
        removeKnownsForAll xs = map (removeKnownFor $ knowns xs) xs :: [[Int]]
        removeKnownFor ks (x:[]) = [x]
        removeKnownFor ks xs = xs \\ ks
        remainUnknowns xs = (length $ concat xs) > (length xs)

type Device = [Execution]

createDevice :: [Sample] -> Device
createDevice samples = map snd $ sortOn fst $ zip allOpcodes allExecutions
  where allOpcodes = reduceOpcodeCandidates $ findOpcodeCandidates samples

runProgram :: Device -> [Instruction] -> Memory
runProgram device instructions = foldl execute initialMemory instructions
  where initialMemory = Memory 0 0 0 0
        execute memory (Instruction opc params) = (device !! opc) params memory

{- ########### factories ########### -}

readProgram :: String -> [Instruction]
readProgram = map readInstruction . lines

readInstruction :: String -> Instruction
readInstruction = readParams . extractFromRegex "([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+)"
  where readParams (opc:a:b:c:_) = Instruction opc (Parameters a b c)

readSamples :: String -> [Sample]
readSamples = map readSample . splitFromRegex "\n\n"

readSample :: String -> Sample
readSample = readParams . extractFromRegex "Before: \\[([0-9]+), ([0-9]+), ([0-9]+), ([0-9]+)\\]\\s([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+)\\sAfter:  \\[([0-9]+), ([0-9]+), ([0-9]+), ([0-9]+)\\]"
  where readParams ps = Sample (readMemory ps) (readInstruction $ drop 4 ps) (readMemory $ drop 8 ps)
        readMemory (a:b:c:d:_) = Memory a b c d
        readInstruction (opc:a:b:c:_) = Instruction opc (Parameters a b c)
        
extractFromRegex :: (Read a) => String -> String -> [a]
extractFromRegex regex def = (\(Just (_,_,_,subs)) -> map read subs) $ matchRegexAll (mkRegex regex) def

splitFromRegex :: String -> String -> [String]
splitFromRegex regex = splitRegex (mkRegex regex)
