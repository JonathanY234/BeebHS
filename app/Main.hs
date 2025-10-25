module Main where

import CPU6502 (cpuMain)
import LoadRom (loadMode7Font)
import GraphicsSDLStuff (initVideo, endVideo, eventLoop, renderMode7Frame)
import HarteCpuTests (runTests)

import Control.Concurrent (threadDelay)
import Data.Word (Word8)
import Control.Monad (unless)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        (arg:_) -> putStrLn ("First argument: " ++ arg)
        []      -> putStrLn "No arguments provided"

    --runTests
    cpuMain

    -- theRom <- loadRom "roms/mos320.rom" 16384
    -- print theRom

    -- m7Font <- loadMode7Font "roms/saa5050.fnt" 18
    -- let charsToDraw :: [Word8]
    --     charsToDraw = [0..(25*40)]
    --     targetHz = 50

    -- sdlCtxt <- initVideo
    
    -- let mainLoop :: IO ()
    --     mainLoop = do
    --         quit <- eventLoop

    --         unless quit $ do
    --             renderMode7Frame sdlCtxt m7Font charsToDraw
    --             threadDelay (1000000 `div` targetHz)
    --             mainLoop

    -- mainLoop
    -- endVideo sdlCtxt

