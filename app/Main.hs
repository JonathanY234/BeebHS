module Main where

import CPU6502 (cpuInit, runInstructionsDebug, runInstructions, debuggerStart)
import LoadRom (loadMode7Font, loadSpaceInvaders)
import SDLVideoOutput (initVideo, endVideo, eventLoop, renderMode7Frame)
import HarteCpuTests (runTests)
import Debug (DebugState, DebugState(..))

import System.Clock ( getTime, toNanoSecs, Clock(Monotonic) )
import Control.Concurrent (threadDelay)
import Control.Monad (unless, when)
import System.Environment (getArgs)
import System.Exit (exitSuccess)

import Data.Word (Word16)
import MemoryRegisters (Memory (cycleCount), readMemory)
import KeyboardInput (updateKeyboardMatrix)
import Data.Bits (Bits((.|.), shiftL))
import Data.IORef (readIORef)

main :: IO ()
main = do
    args <- getArgs
    when ("-cpuTest" `elem` args) $ do
            runTests
            exitSuccess
    let isDebug = "-debug" `elem` args

    -- initialisation
    --m7Font <- loadMode7Font "roms/original.fnt" 24
    m7Font <- loadMode7Font "roms/basicsdl.fnt" 28
    --let (targetHz :: Int) = 50
    
    (mem, regs) <- cpuInit
    sdlCtxt <- initVideo

    clockStart <- getTime Monotonic

    let mainLoop :: DebugState -> IO ()
        mainLoop debugState = do
            (quit, qPressed) <- eventLoop
            
            unless quit $ do

                when qPressed $ loadSpaceInvaders mem

                updateKeyboardMatrix mem

                newDebugState <- if isDebug
                    then runInstructionsDebug mem regs 10000 debugState
                    else do
                        runInstructions mem regs 10000
                        return debugState -- value of debugState is not relevent here as not debugging

                scrollAmount <- getScrollingAmount mem
                renderMode7Frame sdlCtxt m7Font mem scrollAmount-- + 959)

                -- slow down
                cyclesPassed <- readIORef (cycleCount mem)
                let idealElapsedNs = cyclesPassed * nanosecondsPerCycle

                clockCurrent <- getTime Monotonic
                let elapsedNs = toNanoSecs (clockCurrent - clockStart)
                    timeCorrection = if idealElapsedNs > elapsedNs
                                        then idealElapsedNs - elapsedNs
                                        else 0
                threadDelay (fromIntegral timeCorrection `div` 1000)
                mainLoop newDebugState

    when isDebug $ debuggerStart mem regs
    mainLoop (DebugState [] 0 True 0)
    endVideo sdlCtxt

nanosecondsPerCycle :: Integer
nanosecondsPerCycle = 1000000000 `div` 2000000 -- = 500

getScrollingAmount :: Memory -> IO Word16
getScrollingAmount mem = do
    lowByte <- readMemory mem 0x0350
    highByte <- readMemory mem 0x0351
    let screenStart = (fromIntegral highByte `shiftL` 8) .|. fromIntegral lowByte
    return screenStart