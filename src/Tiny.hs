-- Tiny.hs

{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Tiny (
    Computer(Computer)
  , Program(Program)
  , initialize
  , printOut
  , run
  , showInput
  , showOutput
) where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.State
import Data.Array
import Data.Bits
import Data.Bool
import Data.Int
import Data.List
import Numeric (showHex)

class Category obj mor | mor -> obj where
    dom :: mor -> obj
    cod :: mor -> obj
    idy :: obj -> mor
    cmp :: mor -> mor -> Maybe mor

-- TinyWord is the basic data type of the TINY computer
-- A 4-bit word (one hex character) backed by an integer since Haskell provides no native 4-bit type
newtype TinyWord = TW Integer deriving (Eq)

instance Show TinyWord where
    show (TW a) = showHex a ""

-- Mask off the lowest 4-bit part of the Integer
mkTinyWord :: Integer -> TinyWord
mkTinyWord b = TW . toInteger $ 0xF .&. b

-- Some useful operations on TinyWords, with carry and overflow flags
flipTW :: TinyWord -> TinyWord
flipTW (TW a) = mkTinyWord . xor 0xF $ a

-- arithmetic operations produce carry and overflow flags
data ArithOut = ArithOut { value :: TinyWord
                         , carry :: Bool
                         , overflow :: Bool
                         }

rolTW :: Bool -> TinyWord -> ArithOut
rolTW carryIn (TW a) = let
    res = shiftL a 1 .|. if carryIn then bit 0 else zeroBits
    carryOut = testBit res 4
    value = mkTinyWord res
    overflow = xor carryOut $ testBit res 3
    in ArithOut { value = value, carry = carryOut, overflow = overflow }

rorTW :: Bool -> TinyWord -> ArithOut
rorTW carryIn (TW a) = let
    res = a .|. if carryIn then bit 4 else zeroBits
    carryOut = testBit res 0
    value = mkTinyWord $ shiftR res 1
    overflow = xor carryIn $ testBit res 3
    in ArithOut { value = value, carry = carryOut, overflow = overflow }
    
addTW :: Bool -> TinyWord -> TinyWord -> ArithOut
addTW carryIn (TW a) (TW b) = let
    sum =  a + b + if carryIn then 1 else 0
    carryOut = testBit sum 4
    value = mkTinyWord sum
    overflow = testBit ((a `xor` sum) .&. (b `xor` sum)) 3
    in ArithOut { value = value, carry = carryOut, overflow = overflow }

incTW :: TinyWord -> TinyWord
incTW = value . addTW False (TW 0x1)

decTW :: TinyWord -> TinyWord
decTW = value . addTW False (TW 0xF)

-- The RAM of the TINY computer, with 16 addressable values
newtype Memory = Memory (Array Integer TinyWord)

valueAt :: TinyWord -> Lens' Memory TinyWord
valueAt (TW addr) = let
    readMem (Memory mem) = mem!addr
    writeMem (Memory mem) value = Memory $ mem // [(addr, value)]
    in lens readMem writeMem

memoryDefault :: Memory
memoryDefault = Memory . listArray (0x0, 0xF) . replicate 0x10 . TW $ 0x0

instance Show Memory where
    show (Memory mem) = concatMap show . elems $ mem

-- The Flag register is backed by another TinyWord, but we want to access it bit-by-bit
newtype Flags = Flags TinyWord

flag :: Int -> Lens' Flags Bool
flag i = let
    setOrClear = bool clearBit setBit
    getB (Flags (TW x)) = testBit x i
    setB (Flags (TW x)) b = Flags . TW $ setOrClear b x i
    in lens getB setB

fH :: Lens' Flags Bool
fH = flag 3
fO :: Lens' Flags Bool
fO = flag 2
fZ :: Lens' Flags Bool
fZ = flag 1
fC :: Lens' Flags Bool
fC = flag 0

instance Show Flags where
    show f = let readFlag (lens, symbol) = if view lens f then symbol else " " in
        concatMap readFlag [(fH, "h"), (fO, "o"), (fZ, "z"), (fC, "c")]

flagsDefault :: Flags
flagsDefault = Flags (TW 0x0)

-- The registers of the TINY computer
data Registers = Registers { _fr :: Flags
                           , _ip :: TinyWord
                           , _li :: TinyWord
                           , _ac :: TinyWord
                           }

fr :: Lens' Registers Flags
fr = lens _fr $ \r fr -> r { _fr = fr }

ip :: Lens' Registers TinyWord
ip = lens _ip $ \r ip -> r { _ip = ip }

li :: Lens' Registers TinyWord
li = lens _li $ \r li -> r { _li = li }

ac :: Lens' Registers TinyWord
ac = lens _ac $ \r ac -> r { _ac = ac }

registersDefault :: Registers
registersDefault = Registers { _fr = flagsDefault
                             , _ip = TW 0x0
                             , _li = TW 0x0
                             , _ac = TW 0x0
                             }

instance Show Registers where
    show r = "{" ++
             " fr: " ++ (show . _fr $ r) ++
             " ip: " ++ (show . _ip $ r) ++
             " li: " ++ (show . _li $ r) ++
             " ac: " ++ (show . _ac $ r) ++
             " }"

-- Finally, the whole TINY computer
data Computer = Computer { _registers :: Registers
                         , _memory :: Memory
                         , _input :: [TinyWord]
                         , _output :: [TinyWord]
                         }

registers :: Lens' Computer Registers
registers = lens _registers $ \c r -> c { _registers = r }

memory :: Lens' Computer Memory
memory = lens _memory $ \c m -> c { _memory = m }

input :: Lens' Computer [TinyWord]
input = lens _input $ \c i -> c { _input = i }

output :: Lens' Computer [TinyWord]
output = lens _output $ \c o -> c { _output = o }

newtype Program = Program Int64

loadProgram :: Program -> Computer -> Computer
loadProgram (Program p) = let
    gen seed = Just (tw, newSeed) where
        newSeed = rotate seed 4
        tw = mkTinyWord . toInteger $ newSeed
    newMemory = Memory . listArray (0x0, 0xF) . take 0x10 . unfoldr gen $ p
    in set memory newMemory

computerDefault :: Computer
computerDefault = Computer { _registers = registersDefault
                           , _memory = memoryDefault
                           , _input = []
                           , _output = []
                           }

showInput :: Computer -> String
showInput = concatMap show . _input

showOutput :: Computer -> String
showOutput = concatMap show . reverse . _output

instance Show Computer where
    show c =
        (show . _registers $ c) ++ 
        " RAM: " ++ (show . _memory $ c) ++ 
        " Input: " ++ (concatMap show . _input $ c) ++ 
        " Output: " ++ (concatMap show . reverse . _output $ c)

-- The instruction set of the TINY computer; some instructions take addresses
data Instruction = HLT
                 | JMP TinyWord
                 | JZE TinyWord
                 | JNZ TinyWord
                 | LDA TinyWord
                 | STA TinyWord
                 | GET
                 | PUT
                 | ROL
                 | ROR
                 | ADC TinyWord
                 | CCF
                 | SCF
                 | DEL
                 | LDL TinyWord
                 | FLA
                 deriving (Show)

takesOperand :: TinyWord -> Bool
takesOperand (TW opcode) = opcode `elem` [0x1, 0x2, 0x3, 0x4, 0x5, 0xA, 0xE]

mkInstruction :: TinyWord -> TinyWord -> Instruction
mkInstruction (TW opcode) operand = case opcode of
    0x0 -> HLT
    0x1 -> JMP operand
    0x2 -> JZE operand
    0x3 -> JNZ operand
    0x4 -> LDA operand
    0x5 -> STA operand
    0x6 -> GET
    0x7 -> PUT
    0x8 -> ROL
    0x9 -> ROR
    0xA -> ADC operand
    0xB -> CCF
    0xC -> SCF
    0xD -> DEL
    0xE -> LDL operand
    0xF -> FLA

-- Read an instruction (including address if necessary) from memory
-- Also note the updated IP
readInstruction :: TinyWord -> Memory -> (Instruction, TinyWord)
readInstruction ip ram = let
    TW opcode = view (valueAt ip) ram
    nextIP = incTW ip
    addr = view (valueAt nextIP) ram   
    in case opcode of
        0x0 -> (HLT, nextIP)
        0x1 -> (JMP addr, incTW nextIP)
        0x2 -> (JZE addr, incTW nextIP)
        0x3 -> (JNZ addr, incTW nextIP)
        0x4 -> (LDA addr, incTW nextIP)
        0x5 -> (STA addr, incTW nextIP)
        0x6 -> (GET, nextIP)
        0x7 -> (PUT, nextIP)
        0x8 -> (ROL, nextIP)
        0x9 -> (ROR, nextIP)
        0xA -> (ADC addr, incTW nextIP)
        0xB -> (CCF, nextIP)
        0xC -> (SCF, nextIP)
        0xD -> (DEL, nextIP)
        0xE -> (LDL addr, incTW nextIP)
        0xF -> (FLA, nextIP)

-- Now we start to get to the meat of the emulation
-- First, some utilities to make lenses and states work nicely together
viewState :: Getting a s a -> State s a
viewState = gets . view

setState :: ASetter' s a -> a -> State s ()
setState = (modify .) . set

overState :: ASetter' s a -> (a -> a) -> State s ()
overState = (modify .) . over

setStateIf :: ASetter' s a -> a -> Bool -> State s ()
setStateIf l a b = let
    replaceIf a' = if b then a else a'
    in overState l replaceIf

-- Next the basic state interactions for flags, registers, memory
getHF :: State Computer Bool
getHF = viewState (registers.fr.fH)

setHF :: Bool -> State Computer ()
setHF = setState (registers.fr.fH)

getOF :: State Computer Bool
getOF = viewState (registers.fr.fO)

setOF :: Bool -> State Computer ()
setOF = setState (registers.fr.fO)

getZF :: State Computer Bool
getZF = viewState (registers.fr.fZ)

-- the zero flag is only set based on a given word value, never directly
setZF :: TinyWord -> State Computer ()
setZF = setState (registers.fr.fZ) . (TW 0 ==)

getCF :: State Computer Bool
getCF = viewState (registers.fr.fC)

setCF :: Bool -> State Computer ()
setCF = setState (registers.fr.fC)

getIP :: State Computer TinyWord
getIP = viewState (registers.ip)

setIP :: TinyWord -> State Computer ()
setIP = setState (registers.ip)

getLI :: State Computer TinyWord
getLI = viewState (registers.li)

-- whenever we set LI, also set or clear the zero flag
setLI :: TinyWord -> State Computer ()
setLI = liftM2 (>>) (setState (registers.li)) setZF

getAC :: State Computer TinyWord
getAC = viewState (registers.ac)

-- whenever we set AC, also set or clear the zero flag
setAC :: TinyWord -> State Computer ()
setAC = liftM2 (>>) (setState (registers.ac)) setZF

getMem :: TinyWord -> State Computer TinyWord
getMem addr = viewState (memory.valueAt addr)

setMem :: TinyWord -> TinyWord -> State Computer ()
setMem addr = setState (memory.valueAt addr)

-- Some useful Computer-specific state interactions
getInstruction :: State Computer Instruction
getInstruction = let
    fetchByIP = liftM2 (>>) (setIP . incTW) getMem =<< getIP
    fetchOperand = bool (return $ TW 0x0) fetchByIP . takesOperand
    in liftM2 (<$>) mkInstruction fetchOperand =<< fetchByIP

pullInput :: State Computer TinyWord
pullInput = state $ \c -> case view input c of
    []       -> (TW 0x0, c)
    (i:rest) -> (i, set input rest c)

-- put the output in the AC register and set carry, overflow, and zero flags appropriately
handleArithmeticOutput :: ArithOut -> State Computer ()
handleArithmeticOutput = (last <$>) . sequence . ([setAC . value, setCF . carry, setOF . overflow] ??)

-- Now the encodings of each instruction as a State inteaction
doHLT :: State Computer ()
doHLT = setHF True

doJMP :: TinyWord -> State Computer ()
doJMP = setIP

doJZE :: TinyWord -> State Computer ()
doJZE = (getZF >>=) . setStateIf (registers.ip)

doJNZ :: TinyWord -> State Computer ()
doJNZ = (getZF >>=) . (. not) . setStateIf (registers.ip)

doLDA :: TinyWord -> State Computer ()
doLDA = setAC <=< getMem

doSTA :: TinyWord -> State Computer ()
doSTA = (getAC >>=) . setMem

doGET :: State Computer ()
doGET = setAC =<< pullInput

doPUT :: State Computer ()
doPUT = getAC >>= overState output . (:)

doROL :: State Computer ()
doROL = handleArithmeticOutput =<< liftM2 rolTW getCF getAC

doROR :: State Computer ()
doROR = handleArithmeticOutput =<< liftM2 rorTW getCF getAC

doADC :: TinyWord -> State Computer ()
doADC = handleArithmeticOutput <=< liftM3 addTW getCF getAC . getMem

doCCF :: State Computer ()
doCCF = setCF False

doSCF :: State Computer ()
doSCF = setCF True

doDEL :: State Computer ()
doDEL = setLI . decTW =<< getLI

doLDL :: TinyWord -> State Computer ()
doLDL = setLI <=< getMem

doFLA :: State Computer ()
doFLA = setAC . flipTW =<< getAC

-- Wiring each instruction to its corresponding implementation
execute :: Instruction -> State Computer ()
execute HLT        = doHLT
execute (JMP addr) = doJMP addr
execute (JZE addr) = doJZE addr
execute (JNZ addr) = doJNZ addr
execute (LDA addr) = doLDA addr
execute (STA addr) = doSTA addr
execute GET        = doGET
execute PUT        = doPUT
execute ROL        = doROL
execute ROR        = doROR
execute (ADC addr) = doADC addr
execute CCF        = doCCF
execute SCF        = doSCF
execute DEL        = doDEL
execute (LDL addr) = doLDL addr
execute FLA        = doFLA

-- At last, the core of the emulation: what happens on a clock tick?
tick :: State Computer ()
tick = getInstruction >>= execute

-- a couple looping combinators
whileNot :: State a () -> State a Bool -> State a ()
whileNot update test = let
    stepAndRecurse = update >> update `whileNot` test
    stop = return ()
    in test >>= bool stepAndRecurse stop

traceUntil :: State a () -> State a Bool -> State a [a]
traceUntil update test = let
    addAndRecurse = get >>= (<$> (update >> update `traceUntil` test)) . (:)
    stop = return <$> get
    in test >>= bool addAndRecurse stop

halted :: State Computer Bool
halted = getHF

-- Initialize a computer with a program and input stream
initialize :: Program -> [Integer] -> Computer
initialize program inputStream =
    loadProgram program .
    set input (map mkTinyWord inputStream) $
    computerDefault

-- Trace the execution of the emulation one tick at a time until it halts
trace :: State Computer [Computer]
trace = tick `traceUntil` halted

-- Let the computer run until it halts
run :: Computer -> Computer
run = execState $ tick `whileNot` halted

-- Print all the output from a given program
printOut :: Computer -> IO ()
printOut = mapM_ print . evalState trace
