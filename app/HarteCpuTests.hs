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
import Control.Monad (unless, forM, forM_)
import qualified Data.Map as M


import CPU6502 ( CPURegs, initRegisters, pc, accumulator, x, y, stackP, statusReg, opcodeTable, printRegs, showStatusReg )
import Memory ( Memory, initMemory, writeMemory, readMemory )
import Debug (showMemoryPage, opcodeNames, showHexF)
import GHC.Integer (popCountInteger)
-- import Numeric (showHex)



type RamEntry = (Word16, Word8)

showRam :: [RamEntry] -> String
showRam entries = intercalate ", " $ map (\(addr, val) ->
    showHexF addr ++ "=" ++ showHexF val) entries

data CycleEntry = CycleEntry
    { address :: Int
    , value   :: Int
    , action  :: String
    } deriving (Show, Generic)

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
    } deriving (Show, Generic)

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
    mem <- initMemory
    mapM_ (uncurry (writeMemory mem)) memoryVals

    return (regs, mem)

isFinalRegsAndMemSameAsExpected :: CPURegs -> Memory -> CpuState -> IO Bool
isFinalRegsAndMemSameAsExpected regs mem CpuState{pcState=fPc, accumulatorState=fA, xState=fX, yState=fY, stackPState=fSp, statusRegState=fSr, ram=memoryVals} = do
    pcVal <- readIORef (pc regs)
    aVal  <- readIORef (accumulator regs)
    xVal  <- readIORef (x regs)
    yVal  <- readIORef (y regs)
    spVal <- readIORef (stackP regs)
    srVal <- readIORef (statusReg regs)

    let regsMatch = and [ fPc == pcVal, fA  == aVal, fX  == xVal, fY  == yVal, fSp == spVal, fSr == srVal ]

    finalMem <- initMemory
    mapM_ (uncurry (writeMemory finalMem)) memoryVals

    let allEqual i
            | i == (64*1024)  = return True
            | otherwise = do
                x <- readMemory mem i
                y <- readMemory finalMem i
                if x /= y then return False else allEqual (i + 1)

    memMatch <- allEqual 0

    return (regsMatch && memMatch)

run1Test :: ATest -> IO Bool
run1Test ATest{name=name, initial=initial, final=final, cycles=cycles} = do

    (regs, mem) <- loadInitialRegistersAndMem initial

    -- run one instruction just like normal
    pcVal <- readIORef (pc regs)
    currentInstructionOpcode <- readMemory mem pcVal
    let instr = opcodeTable IBVector.! fromIntegral currentInstructionOpcode
    instr mem regs

    result <- isFinalRegsAndMemSameAsExpected regs mem final

    unless result $ do
        putStrLn $ "Failed Test: " ++ name
        putStrLn $ "Initial : " ++ show initial
        putStrLn $ "Expected: " ++ show final

        actualMemoryValues <- showMemoryNotZeroFormat mem
        putStr "Actual  : "
        printRegs regs
        putStrLn $ "Memory= " ++ actualMemoryValues

    return result

runTests :: IO ()
runTests = do
    -- results <- forM (M.toList opcodeNames) $ \(opcode, name) -> do
    --     putStrLn $ "Test: " ++ name
    --     let fileCode = if length (showHex opcode "") == 2
    --         then showHex opcode ""
    --         else "0" ++ showHex opcode ""

    --     passFail <- runTestsFromFile fileCode
    --     return (name, passFail)

    -- let passes = length $ filter snd results
    --     total  = length results

    -- putStrLn "_________________"
    -- forM_ results $ \(name, passed) ->
    --     unless passed $ putStrLn $ name ++ " Failed"


    -- putStrLn $ "Passed " ++ show passes ++ " out of " ++ show total ++ " test files"

    result <- runTestsFromFile "20"
    print result
        

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

