module Main where

import CPU6502 (cpuMain)
import LoadRom (loadMode7Font)
import SDLVideoOutput (initVideo, endVideo, eventLoop, renderMode7Frame)
import HarteCpuTests (runTests)

import Control.Concurrent (threadDelay)
import Control.Monad (unless, when)
import System.Environment (getArgs)
import System.Exit (exitSuccess)

main :: IO ()
main = do
    args <- getArgs
    when ("-cpuTest" `elem` args) $ do
            runTests
            exitSuccess
    let isDebug = "-debug" `elem` args

    frame <- cpuMain isDebug

    --m7Font <- loadMode7Font "roms/original.fnt" 24
    m7Font <- loadMode7Font "roms/basicsdl.fnt" 28
    let targetHz = 50
    -- let charsToDraw :: [Word8]
    --     charsToDraw = [0..(25*40)]

    sdlCtxt <- initVideo
    
    let mainLoop :: IO ()
        mainLoop = do
            quit <- eventLoop

            unless quit $ do
                renderMode7Frame sdlCtxt m7Font frame
                threadDelay (1000000 `div` targetHz)
                mainLoop

    mainLoop
    endVideo sdlCtxt

