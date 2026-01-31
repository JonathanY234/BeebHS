module MemoryRegisters where

import Sysvia (Sysvia (via), initSysvia, readSysvia, writeSysvia)
import Utilities (showHexF)
import Via (Via(timer1c))

import Data.Word (Word16, Word8)
import Data.IORef (IORef, newIORef, writeIORef)
import qualified Data.Vector.Unboxed.Mutable as MUVector
import Data.Bits ((.&.))



data Memory = Memory {m :: MUVector.IOVector Word8, sysvia :: Sysvia}

readMemory :: Memory -> Word16 -> IO Word8
readMemory mem address =
    if address .&. 0xFFF0 == 0xFE40 then do
        --putStrLn $ "a via address was read " ++ showHexF address
        readSysvia (sysvia mem) address
    else
        MUVector.read (m mem) (fromIntegral address)



-- keyPressed :: Memory -> Int -> IO Bool
-- keyPressed Memory{keyboardMatrix = kbM} rowCol = do
--     let row = rowCol `div` 16
--     let col = rowCol `mod` 16
--     kb <- readIORef kbM
--     return $ kb IBVector.! (col + row * kbMatrixCols)

-- printKeyboardMatrix :: Memory -> IO ()
-- printKeyboardMatrix mem = do
--     kb <- readIORef (keyboardMatrix mem)
--     let lst = IBVector.toList kb
--     forM_ (zip [1..] lst) $ \(i, val) -> do
--         putStr (if val then "1," else "0,")
--         when (i `mod` kbMatrixCols == 0) $
--             putStrLn ""

writeMemory :: Memory -> Word16 -> Word8 -> IO ()
writeMemory mem address value =
    if address .&. 0xFFF0 == 0xFE40 then do
        --putStrLn $ "a via address was written " ++ showHexF address
        writeSysvia (sysvia mem) address value
    else
        MUVector.write (m mem) (fromIntegral address) value

initMemory :: Word8 -> IO Memory
initMemory initialValue = do
    mVec   <- MUVector.replicate (64*1024) initialValue
    Memory mVec <$> initSysvia

data CPURegs = CPURegs {pc :: IORef Word16, accumulator :: IORef Word8, x :: IORef Word8, y :: IORef Word8, stackP :: IORef Word8, statusReg :: IORef Word8}

initRegisters :: Word16 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> IO CPURegs
initRegisters ipc ia ix iy isp isr = do
    pcRef <- newIORef ipc
    aRef  <- newIORef ia
    xRef  <- newIORef ix
    yRef  <- newIORef iy
    spRef <- newIORef isp
    srRef <- newIORef isr
    return (CPURegs pcRef aRef xRef yRef spRef srRef)

writeMemoryArrayOnly :: Memory -> Word16 -> Word8 -> IO ()
writeMemoryArrayOnly mem address = MUVector.write (m mem) (fromIntegral address)

changeInitialTimer1c :: Memory -> IO ()
changeInitialTimer1c mem = do
    let v = via (sysvia mem)
    writeIORef (timer1c v) 1000