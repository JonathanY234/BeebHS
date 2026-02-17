{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module HarteCpuTests where

import Data.Aeson ( eitherDecode, withArray, FromJSON(parseJSON), fieldLabelModifier, genericParseJSON, defaultOptions )
import GHC.Generics ( Generic )
import qualified Data.ByteString.Lazy as B
import qualified Data.Vector as IBVector
import Data.Word (Word16, Word8)
import Data.IORef (readIORef)
import Data.List (intercalate)
import Control.Monad (unless, forM, forM_, when)
import qualified Data.Map as M


import CPU6502 ( opcodeTable)
import MemoryRegisters ( Memory (cycleCount), initMemory, writeMemory, readMemory, CPURegs(pc, x, y, stackP, accumulator, statusReg), initRegisters)
import Debug (opcodeNames, showStatusReg, printRegs)
import Utilities (showHexF, showHexX)
import Numeric (showHex)


adcAndSbc :: [Word8]
adcAndSbc = [0x61, 0x65, 0x69, 0x6D, 0x71, 0x75, 0x79, 0x7D, 0xE1, 0xE5, 0xE9, 0xED, 0xF1, 0xF5, 0xF9, 0xFD]

type RamEntry = (Word16, Word8)

showRam :: [RamEntry] -> String
showRam entries = intercalate ", " $ map (\(addr, val) ->
    showHexX addr ++ "=" ++ showHexF val) entries

data CycleEntry = CycleEntry
    { address :: Int
    , value   :: Int
    , action  :: String
    } deriving (Generic)

showCycles :: [CycleEntry] -> String
showCycles entries = show (length entries)

instance FromJSON CycleEntry where
    parseJSON = withArray "CycleEntry" $ \arr ->
        if IBVector.length arr == 3
            then CycleEntry <$> parseJSON (arr IBVector.! 0)
                            <*> parseJSON (arr IBVector.! 1)
                            <*> parseJSON (arr IBVector.! 2)
            else fail "CycleEntry must be [Int, Int, String]"


data CpuState = CpuState
    { pcState           :: Word16
    , stackPState       :: Word8
    , accumulatorState  :: Word8
    , xState            :: Word8
    , yState            :: Word8
    , statusRegState    :: Word8
    , ram               :: [RamEntry]
    } deriving (Generic)

instance Show CpuState where
    show cpu =
        "PC=" ++ showHexF (pcState cpu) ++
        " A="  ++ showHexF (accumulatorState cpu) ++
        " X="  ++ showHexF (xState cpu) ++
        " Y="  ++ showHexF (yState cpu) ++
        " SP=" ++ showHexF (stackPState cpu) ++
        " SR=" ++ showStatusReg (statusRegState cpu) ++
        " Memory= " ++ showRam (ram cpu)

instance FromJSON CpuState where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = \case
            "pcState"           -> "pc"
            "stackPState"       -> "s"
            "accumulatorState"  -> "a"
            "xState"            -> "x"
            "yState"            -> "y"
            "statusRegState"    -> "p"
            other               -> other
        }

-- The top-level structure
data ATest = ATest
    { name   :: String
    , initial :: CpuState
    , final   :: CpuState
    , cycles  :: [CycleEntry]
    } deriving (Generic)

instance FromJSON ATest

showMemoryNotZeroFormat :: Memory -> IO String
showMemoryNotZeroFormat mem =

    let go :: Word16 -> String -> IO String
        go 65535 output = return output
        go pointer output = do
            memVal <- readMemory mem pointer
            let newAcc = if memVal == 0
                            then output
                            else showHexF pointer ++ "=" ++ showHexF memVal ++ ", " ++ output
            go (pointer + 1) newAcc

    in go 0 ""

loadInitialRegistersAndMem :: CpuState -> IO (CPURegs, Memory)
loadInitialRegistersAndMem CpuState{pcState=iPc, accumulatorState=iA, xState=iX, yState=iY, stackPState=iSp, statusRegState=iSr, ram=memoryVals} = do

    regs <- initRegisters iPc iA iX iY iSp iSr
    mem <- initMemory 0x00
    mapM_ (uncurry (writeMemory mem)) memoryVals

    return (regs, mem)

isFinalRegsMemCyclesAsExpected :: CPURegs -> Memory -> CpuState -> Int -> IO Bool
isFinalRegsMemCyclesAsExpected regs mem CpuState{pcState=fPc, accumulatorState=fA, xState=fX, yState=fY, stackPState=fSp, statusRegState=fSr, ram=memoryVals} expectedCycles = do
    pcVal <- readIORef (pc regs)
    aVal  <- readIORef (accumulator regs)
    xVal  <- readIORef (x regs)
    yVal  <- readIORef (y regs)
    spVal <- readIORef (stackP regs)
    srVal <- readIORef (statusReg regs)

    let regsMatch = and [ fPc == pcVal, fA  == aVal, fX  == xVal, fY  == yVal, fSp == spVal, fSr == srVal ]

    finalMem <- initMemory 0x00
    mapM_ (uncurry (writeMemory finalMem)) memoryVals

    let allEqual i
            | i == (64*1024)  = return True
            | otherwise = do
                x <- readMemory mem i
                y <- readMemory finalMem i
                if x /= y then return False else allEqual (i + 1)

    memMatch <- allEqual 0

    cyclesPassed <- readIORef (cycleCount mem)
    let cyclesMatch = cyclesPassed == fromIntegral expectedCycles

    return (regsMatch && memMatch && cyclesMatch)

run1Test :: ATest -> IO Bool
run1Test ATest{name=name, initial=initial, final=final, cycles=cycles} = do

    (regs, mem) <- loadInitialRegistersAndMem initial

    -- run one instruction just like normal
    pcVal <- readIORef (pc regs)
    currentInstructionOpcode <- readMemory mem pcVal
    let instr = opcodeTable IBVector.! fromIntegral currentInstructionOpcode
    instr mem regs

    result <- isFinalRegsMemCyclesAsExpected regs mem final (length cycles)

    let verbose = False
    let dontShowAdcSbc = True

    let isAdcSbc = currentInstructionOpcode `elem` adcAndSbc

    when (not result && verbose && not (dontShowAdcSbc && isAdcSbc)) $ do
        actualCycles <- readIORef (cycleCount mem)
        putStrLn $ "Failed Test: " ++ name
        putStrLn $ "Initial : " ++ show initial
        putStrLn $ "Expected: " ++ show final ++ "   Cycles: " ++ showCycles cycles

        actualMemoryValues <- showMemoryNotZeroFormat mem
        putStr "Actual  : "
        printRegs regs
        putStrLn $ "Memory= " ++ actualMemoryValues ++ "   Cycles: " ++ show actualCycles

    return result

runTests :: IO ()
runTests = do
    results <- forM (M.toList opcodeNames) $ \(opcode, name) -> do
        putStrLn $ "Test: " ++ name
        let fileCode = if length (showHex opcode "") == 2
            then showHex opcode ""
            else "0" ++ showHex opcode ""

        passFail <- runTestsFromFile fileCode
        return (name, passFail)

    let passes = length $ filter snd results
        total  = length results

    putStrLn "_________________"
    forM_ results $ \(name, passed) ->
        unless passed $ putStrLn $ name ++ " Failed"

    putStrLn $ "Passed " ++ show passes ++ " out of " ++ show total ++ " test files"

    -- result <- runTestsFromFile "91"
    -- print result
        

runTestsFromFile :: String -> IO Bool
runTestsFromFile fileVal = do
    jsonData <- B.readFile $ "harteCpuTests/v1/" ++ fileVal ++ ".json"

      -- Parse as a list of ATest
    let input = eitherDecode jsonData :: Either String [ATest]

    case input of
        Left err       -> putStrLn ("Error parsing JSON: " ++ err) >> return False
        Right testCases -> do

            results <- forM testCases run1Test
            if and results
                then putStrLn "Passed" >> return True
                else putStrLn "Failed" >> return False

