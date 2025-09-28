module CPU6502 where

import Data.Word (Word16, Word8)
import Data.Bits ( Bits((.|.), testBit, complement, (.&.), shiftL))
import Data.IORef (IORef, readIORef, modifyIORef', newIORef, writeIORef)
import qualified Data.Vector.Unboxed.Mutable as MUVector
import qualified Data.Vector as IBVector

cpuMain :: IO ()
cpuMain = do
    mem <- initMemory
    putStrLn "memtest___"
    print =<< readMemory mem 300
    writeMemory mem 300 20
    print =<< readMemory mem 300

    putStrLn "regTest___"
    regs <- initRegisters 0 0 0 0 0 0x20
    sr <- readIORef (statusReg regs)
    print $ showStatusReg sr
    printRegs regs

    putStrLn "Instrs Test___"
    runInstructions mem regs 10

runInstructions :: Memory -> CPURegs -> Int -> IO ()
runInstructions mem regs count = loop 0
    where
        loop n --hopefully looping in a way that avoids stack overflow
            | n >= count = return ()
            | otherwise = do
                pcVal <- readIORef (pc regs)
                currentInstructionOpcode <- readMemory mem pcVal
                let instr = opcodeTable IBVector.! fromIntegral currentInstructionOpcode
                instr mem regs
                loop (n+1)

-- __________Memory__________
data Memory = Memory {m :: MUVector.IOVector Word8}

readMemory :: Memory -> Word16 -> IO Word8
readMemory memory address = MUVector.read (m memory) (fromIntegral address)

writeMemory :: Memory -> Word16 -> Word8 -> IO ()
writeMemory memory address = MUVector.write (m memory) (fromIntegral address)

initMemory :: IO Memory
initMemory = Memory <$> MUVector.replicate (64*1024) 0

-- __________Registers__________
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

printRegs :: CPURegs -> IO ()
printRegs regs = do
    pc' <- readIORef (pc regs)
    a'  <- readIORef (accumulator regs)
    x'  <- readIORef (x regs)
    y'  <- readIORef (y regs)
    sp' <- readIORef (stackP regs)
    sr' <- readIORef (statusReg regs)
    putStrLn $ "PC=" ++ show pc' ++ " A=" ++ show a' ++ " X=" ++ show x' ++ " Y=" ++ show y' ++ " SP=" ++ show sp' ++ " SR=" ++ showStatusReg sr'

showStatusReg :: Word8 -> String
showStatusReg w = [if testBit w i then '1' else '0' | i <- [7,6..0]]

-- __________StatusReg Read Helpers__________
srReadBit :: Word8 -> IORef Word8 -> IO Bool
srReadBit mask statusReg = do
    val <- readIORef statusReg
    return $ (val .&. mask) /= 0
srReadCarry            :: IORef Word8 -> IO Bool; srReadCarry            = srReadBit 0x01
srReadZero             :: IORef Word8 -> IO Bool; srReadZero             = srReadBit 0x02
srReadInterruptDisable :: IORef Word8 -> IO Bool; srReadInterruptDisable = srReadBit 0x04
srReadDecimalMode      :: IORef Word8 -> IO Bool; srReadDecimalMode      = srReadBit 0x08
srReadBreak            :: IORef Word8 -> IO Bool; srReadBreak            = srReadBit 0x10
srReadUnused           ::                IO Bool; srReadUnused           = return True --haha
srReadOverflow         :: IORef Word8 -> IO Bool; srReadOverflow         = srReadBit 0x40
srReadNegative         :: IORef Word8 -> IO Bool; srReadNegative         = srReadBit 0x80

-- __________StatusReg Write Helpers__________
srWriteBit :: Word8 -> IORef Word8 -> Bool -> IO ()
srWriteBit mask statusReg val = modifyIORef' statusReg update
    where
        update sr = if val then sr .|. mask else sr .&. complement mask

srWriteOverflow         :: IORef Word8 -> Bool -> IO (); srWriteOverflow         = srWriteBit 0x40
srWriteCarry            :: IORef Word8 -> Bool -> IO (); srWriteCarry            = srWriteBit 0x01
srWriteZero             :: IORef Word8 -> Bool -> IO (); srWriteZero             = srWriteBit 0x02
srWriteInterruptDisable :: IORef Word8 -> Bool -> IO (); srWriteInterruptDisable = srWriteBit 0x04
srWriteDecimalMode      :: IORef Word8 -> Bool -> IO (); srWriteDecimalMode      = srWriteBit 0x08
srWriteBreak            :: IORef Word8 -> Bool -> IO (); srWriteBreak            = srWriteBit 0x10
--srWriteUnused         :: IORef Word8 -> Bool -> IO (); srWriteUnused           = srWriteBit 0x20
srWriteNegative         :: IORef Word8 -> Bool -> IO (); srWriteNegative         = srWriteBit 0x80

-- __________Create Opcode Table__________
opcodeTable :: IBVector.Vector (Memory -> CPURegs -> IO ())
opcodeTable = IBVector.generate 256 assign
    where
        assign :: Int -> (Memory -> CPURegs -> IO ())
        assign 0x00 = instrBRK
        assign 0x01 = instrORA
        assign _    = instrUnimplemented

-- __________Addressing Modes__________
-- immediate        value is the value right there in the instruction
immediate :: (Word16 -> Memory -> CPURegs -> Bool -> IO ()) -> Memory -> CPURegs -> IO ()
immediate instr mem regs = do
    pcVal <- readIORef (pc regs)
    operand <- readMemory mem (pcVal + 1)

    writeIORef (pc regs) (pcVal + 2)
    instr (fromIntegral operand) mem regs True

-- zeropage         value is in 0th page can be addressed with 8bits
zeropage :: (Word16 -> Memory -> CPURegs -> Bool -> IO ()) -> Memory -> CPURegs -> IO ()
zeropage instr mem regs = do
    pcVal <- readIORef (pc regs)
    operand <- readMemory mem (pcVal + 1)

    writeIORef (pc regs) (pcVal + 2)
    instr (fromIntegral operand) mem regs False

-- zeropage,X       value is at (zeropage + value of X reg) (will overflow)
zeropageX :: (Word16 -> Memory -> CPURegs -> Bool -> IO ()) -> Memory -> CPURegs -> IO ()
zeropageX instr mem regs = do
    pcVal <- readIORef (pc regs)
    operand <- readMemory mem (pcVal + 1)

    x' <- readIORef (x regs)
    let address = operand + x'

    writeIORef (pc regs) (pcVal + 2)
    instr (fromIntegral address) mem regs False

combineTwoBytes :: Word8 -> Word8 -> Word16
-- 6502 is little endian for some reason
combineTwoBytes high low = (fromIntegral high `shiftL` 8) .|. fromIntegral low

-- absolute         value is at address pointed to by the next 2 bytes (the whole memory)
absolute :: (Word16 -> Memory -> CPURegs -> Bool -> IO ()) -> Memory -> CPURegs -> IO ()
absolute instr mem regs = do
    pcVal <- readIORef (pc regs)
    operand1 <- readMemory mem (pcVal + 1)
    operand2 <- readMemory mem (pcVal + 2)
    let address = combineTwoBytes operand1 operand2

    writeIORef (pc regs) (pcVal + 3)
    instr address mem regs False

-- absolute,X       value is at Absolute address plus X
absoluteX :: (Word16 -> Memory -> CPURegs -> Bool -> IO ()) -> Memory -> CPURegs -> IO ()
absoluteX instr mem regs = do
    pcVal <- readIORef (pc regs)
    operand1 <- readMemory mem (pcVal + 1)
    operand2 <- readMemory mem (pcVal + 2)
    
    x' <- readIORef (x regs)
    let address = combineTwoBytes operand1 operand2 + fromIntegral x'

    writeIORef (pc regs) (pcVal + 3)
    instr address mem regs False

-- absolute,Y       ''
absoluteY :: (Word16 -> Memory -> CPURegs -> Bool -> IO ()) -> Memory -> CPURegs -> IO ()
absoluteY instr mem regs = do
    pcVal <- readIORef (pc regs)
    operand1 <- readMemory mem (pcVal + 1)
    operand2 <- readMemory mem (pcVal + 2)
    
    y' <- readIORef (y regs)
    let address = ((fromIntegral operand2 `shiftL` 8) .|. fromIntegral operand1) + fromIntegral y'

    writeIORef (pc regs) (pcVal + 3)
    instr address mem regs False

-- (indirect,X)     read value at operand (8 bit, only zeropage) + X (overflows), then read 2 bytes and value is memory at those 2 bytes
indirectX :: (Word16 -> Memory -> CPURegs -> Bool -> IO ()) -> Memory -> CPURegs -> IO ()
indirectX instr mem regs = do
    pcVal <- readIORef (pc regs)
    operand <- readMemory mem (pcVal + 1)
    byte1 <- readMemory mem (fromIntegral operand)
    byte2 <- readMemory mem (fromIntegral ((operand + 1) :: Word8))
    x' <- readIORef (x regs)
    let address = combineTwoBytes byte1 byte2 + fromIntegral x'

    writeIORef (pc regs) (pcVal + 2)
    instr address mem regs False

-- (indirect),Y     ''
indirectY :: (Word16 -> Memory -> CPURegs -> Bool -> IO ()) -> Memory -> CPURegs -> IO ()
indirectY instr mem regs = do
    pcVal <- readIORef (pc regs)
    operand <- readMemory mem (pcVal + 1)
    byte1 <- readMemory mem (fromIntegral operand)
    byte2 <- readMemory mem (fromIntegral ((operand + 1) :: Word8))
    y' <- readIORef (y regs)
    let address = combineTwoBytes byte1 byte2 + fromIntegral y'

    writeIORef (pc regs) (pcVal + 2)
    instr address mem regs False

instrUnimplemented :: Memory -> CPURegs -> IO ()
instrUnimplemented _ _ = putStrLn "unimplementedFunction"

instrBRK :: Memory -> CPURegs -> IO ()
instrBRK _ _ = putStrLn "BRK"

instrORA :: Memory -> CPURegs -> IO ()
instrORA _ _ = putStrLn "ORA"

-- write functions for each addressing mode, that also takes the underlying instruction. Calculate the correct address and pass to underlying instruction. Opcode table can hold partially applied functions
