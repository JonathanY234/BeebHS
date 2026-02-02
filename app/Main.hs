module Main where

import CPU6502 (cpuInit, runInstructionsDebug, runInstructions, debuggerStart)
import LoadRom (loadMode7Font)
import SDLVideoOutput (initVideo, endVideo, eventLoop, renderMode7Frame)
import HarteCpuTests (runTests)
import Debug (DebugState, DebugState(..))

--import Control.Concurrent (threadDelay)
import Control.Monad (unless, when, forM)
import System.Environment (getArgs)
import System.Exit (exitSuccess)

-- --temp
import Data.Word (Word16, Word8)
import MemoryRegisters (Memory, readMemory)
import KeyboardInput (updateKeyboardMatrix)
--endtemp

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
    let (targetHz :: Int) = 50
    (mem, regs) <- cpuInit
    sdlCtxt <- initVideo

    -- cpuRun mem regs False
    -- cpuRun mem regs True
    
    let mainLoop :: DebugState -> IO ()
        mainLoop debugState = do
            quit <- eventLoop
            updateKeyboardMatrix mem

            unless quit $ do
                
                newDebugState <- if isDebug
                    then runInstructionsDebug mem regs 10000 debugState
                    else do
                        runInstructions mem regs 10000
                        return debugState -- value of debugState is not relevent here as not debugging

                frame <- getMode7Frame mem
                renderMode7Frame sdlCtxt m7Font frame

                --threadDelay (1000000 `div` targetHz)
                mainLoop newDebugState

    when isDebug $ debuggerStart mem regs
    mainLoop (DebugState [] 0 True 0)
    endVideo sdlCtxt

-- temp
mode7VideoArea :: Word16
mode7VideoArea = 0x7C00
mode7ScreenSize :: Int
mode7ScreenSize = 40 * 25
getMode7Frame :: Memory -> IO [Word8]
getMode7Frame mem = do
    let addresses = [mode7VideoArea .. mode7VideoArea + fromIntegral mode7ScreenSize - 1]
    forM addresses (readMemory mem)
-- end temp 