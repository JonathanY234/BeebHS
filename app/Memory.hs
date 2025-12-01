module Memory where

import Data.Word (Word16, Word8)
import Data.IORef (IORef, newIORef)
import qualified Data.Vector.Unboxed.Mutable as MUVector

data Memory = Memory {m :: MUVector.IOVector Word8}

readMemory :: Memory -> Word16 -> IO Word8
readMemory memory address = MUVector.read (m memory) (fromIntegral address)

writeMemory :: Memory -> Word16 -> Word8 -> IO ()
writeMemory memory address value = do
    MUVector.write (m memory) (fromIntegral address) value --value removed at the insistence of hlint
    --putStrLn $ "Hello attempted to write memory at " ++ showHex address ""

initMemory :: IO Memory
initMemory = Memory <$> MUVector.replicate (64*1024) 0xFF



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