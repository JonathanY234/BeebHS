module MemoryRegisters where

import Sysvia (Sysvia, initSysvia, readSysvia, writeSysvia)

import Data.Word (Word16, Word8)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Vector.Unboxed.Mutable as MUVector
import Data.Bits ((.&.))

data Memory = Memory {m :: MUVector.IOVector Word8, regs :: CPURegs, cycleCount :: IORef Integer, sysvia :: Sysvia, fileTable :: FileTable}

readMemory :: Memory -> Word16 -> IO Word8
readMemory mem address =
    if address Data.Bits..&. 0xFFF0 == 0xFE40 then do
        readSysvia (sysvia mem) address
    else
        MUVector.read (m mem) (fromIntegral address)
    -- MUVector.read (m mem) (fromIntegral address)

writeMemory :: Memory -> Word16 -> Word8 -> IO ()
writeMemory mem address value = do
    if address Data.Bits..&. 0xFFF0 == 0xFE40 then do
        writeSysvia (sysvia mem) address value
    else
        MUVector.write (m mem) (fromIntegral address) value
    -- MUVector.write (m mem) (fromIntegral address) value

initMemory :: Word8 -> CPURegs -> IO Memory
initMemory initialValue initialRegs = do
    mVec   <- MUVector.replicate (64*1024) initialValue
    cycleCountRef <- newIORef 0
    sysviaa <- initSysvia
    Memory mVec initialRegs cycleCountRef sysviaa <$> initFileTable

writeMemoryArrayOnly :: Memory -> Word16 -> Word8 -> IO ()
writeMemoryArrayOnly mem address = MUVector.write (m mem) (fromIntegral address)

readMemoryArrayOnly :: Memory -> Word16 -> IO Word8
readMemoryArrayOnly mem address = MUVector.read (m mem) (fromIntegral address) --value


-- CPU Registers
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

readRegs ::  (CPURegs -> IORef a) -> Memory -> IO a
readRegs f mem = readIORef (f (regs mem))

writeRegs :: (CPURegs -> IORef a) -> Memory -> a -> IO ()
writeRegs f mem = writeIORef (f (regs mem)) --val

--File Management
data OpenFile = OpenFile
  { handle              :: Word8
  , fileName            :: String
  , mode                :: FileMode
  , readWritePosition   :: Int
  , filePointer         :: Int
  }
data FileMode = Input | Output | InputOutput

type FileTable = (IORef (Maybe OpenFile), IORef (Maybe OpenFile), IORef (Maybe OpenFile))

initFileTable :: IO FileTable
initFileTable = do
    slot1 <- newIORef Nothing
    slot2 <- newIORef Nothing
    slot3 <- newIORef Nothing
    return (slot1, slot2, slot3)