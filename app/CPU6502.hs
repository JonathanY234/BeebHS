module CPU6502 where

import CPUInstructions
import MemoryRegisters (initMemory, initRegisters, readMemory, Memory, CPURegs(pc), sysvia, readRegs, writeRegs)
import LoadRom (loadRom)
import Debug ( DebugState(..), handleInput, debuggerLineOutput, manageSimulatedKeyPress)
import Sysvia ( sysviaPoll, Sysvia (irqPendingFlag), doInterruptCheck )
import FunctionIntercepts (checkForIntercept)

import Data.Word (Word16)
import Data.IORef (readIORef, writeIORef)
import Control.Monad (when, unless, replicateM_)
import qualified Data.Vector as IBVector

cpuInit :: IO Memory
cpuInit = do
    regs <- initRegisters 0 0 0 0 0xFF 0x20
    mem <- initMemory 0xFF regs

    -- Load the 'machine operating system' in the upper quarter of address space
    loadRom "roms/os12.rom" 0xC000 mem
    -- Load the correct basic rom to the sideways rom area
    loadRom "roms/basic2.rom" 0x8000 mem

    initialPC <- getInitialPC mem
    writeRegs pc mem initialPC
    srWriteInterruptDisable mem True

    return mem

getInitialPC :: Memory -> IO Word16
getInitialPC mem = do
    let resetVector :: Word16
        resetVector = 0xFFFC
    low <- readMemory mem resetVector
    high <- readMemory mem (resetVector+1)

    return $ combineTwoBytes low high

doIRQfromSysvia :: Memory -> IO ()
doIRQfromSysvia mem = do
    let svia = sysvia mem

    irqPendingFlagVal <- readIORef (irqPendingFlag svia)
    when irqPendingFlagVal $ do

        irq mem
        writeIORef (irqPendingFlag svia) False

runInstructions :: Memory -> Int -> IO ()
runInstructions mem count =
    replicateM_ count $ do
        pcVal <- readRegs pc mem

        wasIntercepted <- checkForIntercept mem

        unless wasIntercepted $ do
            currentInstructionOpcode <- readMemory mem pcVal
            let instr = opcodeTable IBVector.! fromIntegral currentInstructionOpcode
            instr mem

            sysviaPoll (sysvia mem) 2
            doInterruptCheck (sysvia mem)
            doIRQfromSysvia mem

debuggerStart :: Memory -> IO ()
debuggerStart mem = do
    pcVal <- readRegs pc mem
    operand1 <- readMemory mem (pcVal+1)
    operand2 <- readMemory mem (pcVal+2)
    currentInstructionOpcode <- readMemory mem pcVal

    putStr $ debuggerLineOutput pcVal currentInstructionOpcode operand1 operand2

runInstructionsDebug :: Memory -> Int -> DebugState -> IO DebugState
runInstructionsDebug mem count = loop 0 --dbs
    where
        loop :: Int -> DebugState -> IO DebugState
        loop n debugState@(DebugState _ _ pause _)
            | n >= count = return debugState
            | otherwise = do

                -- get user input
                newDebugState <- if pause
                    then do
                        handleInput mem debugState

                    else return debugState

                let simKP = simulatedKeyPress newDebugState

                when (simKP == 2) $ manageSimulatedKeyPress mem
                when (simKP == 1) $ do
                    manageSimulatedKeyPress mem

                -- run current instruction
                pcVal <- readRegs pc mem
                currentInstructionOpcode <- readMemory mem pcVal
                let instr = opcodeTable IBVector.! fromIntegral currentInstructionOpcode

                instr mem
                sysviaPoll (sysvia mem) 2
                doInterruptCheck (sysvia mem)
                doIRQfromSysvia mem

                -- next instruction
                nextPcVal <- readRegs pc mem
                operand1 <- readMemory mem (nextPcVal+1)
                operand2 <- readMemory mem (nextPcVal+2)
                nextInstructionOpcode <- readMemory mem nextPcVal

                -- decide if need to stop
                let bps      = breakpoints newDebugState
                    stepsRem = stepsRemaining newDebugState
                    newStepsRem = if stepsRem == -1 -- treat -1 as meaning continueMode -- if stepsRem == 0 this will be caught earlier
                                            then stepsRem
                                            else stepsRem -1

                --handle simuated keypress
                    newSimKP = if simKP == 0
                        then 0
                        else 2

                if newStepsRem == 0 || (nextPcVal `elem` bps) then do
                    -- show instruction details
                    putStr $ debuggerLineOutput nextPcVal nextInstructionOpcode operand1 operand2
                    return newDebugState { pause = True, stepsRemaining = newStepsRem, simulatedKeyPress = newSimKP } -- yield to mainLoop
                else loop (n+1) newDebugState { pause = False, stepsRemaining = newStepsRem, simulatedKeyPress = newSimKP }
