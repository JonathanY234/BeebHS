module CPUInstructions where

import Data.Word (Word16, Word8)
import Data.Int (Int8)
import Data.Bits ( Bits((.|.), testBit, complement, (.&.), shiftL, shiftR, xor))
import Data.IORef (IORef, readIORef, modifyIORef', writeIORef)
import Control.Monad (when, forM)
import qualified Data.Vector as IBVector

-- temp
--import System.Exit (exitSuccess)
import qualified Data.ByteString as ByteStr
import GHC.IO.Handle.FD (withBinaryFile)
import GHC.IO.IOMode (IOMode(WriteMode))

import Utilities (showHexF)
import MemoryRegisters (readMemory, writeMemory, Memory (cycleCount), CPURegs(pc, x, y, stackP, accumulator, statusReg))

-- __________StatusReg Read Helpers__________
srReadBit :: Word8 -> IORef Word8 -> IO Bool
srReadBit mask statusReg = do
    val <- readIORef statusReg
    return $ (val .&. mask) /=0
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

srWriteCarry            :: IORef Word8 -> Bool -> IO (); srWriteCarry            = srWriteBit 0x01
srWriteZero             :: IORef Word8 -> Bool -> IO (); srWriteZero             = srWriteBit 0x02
srWriteInterruptDisable :: IORef Word8 -> Bool -> IO (); srWriteInterruptDisable = srWriteBit 0x04
srWriteDecimalMode      :: IORef Word8 -> Bool -> IO (); srWriteDecimalMode      = srWriteBit 0x08
srWriteBreak            :: IORef Word8 -> Bool -> IO (); srWriteBreak            = srWriteBit 0x10
srWriteUnused           :: IORef Word8 -> Bool -> IO (); srWriteUnused           = srWriteBit 0x20
srWriteOverflow         :: IORef Word8 -> Bool -> IO (); srWriteOverflow         = srWriteBit 0x40
srWriteNegative         :: IORef Word8 -> Bool -> IO (); srWriteNegative         = srWriteBit 0x80

opcodeTable :: IBVector.Vector (Memory -> CPURegs -> IO ())
opcodeTable = IBVector.generate 256 assign
    where
        assign :: Int -> (Memory -> CPURegs -> IO ())
        assign 0x00 = brk undefined 7
        assign 0x01 = indirectX ora 6
        assign 0x05 = zeropage ora 3
        assign 0x06 = zeropage asl 5
        assign 0x08 = implied php 3
        assign 0x09 = immediate ora 2
        assign 0x0A = useAcc asl 2
        assign 0x0C = absolute ill_0C 4
        assign 0x0D = absolute ora 4
        assign 0x0E = absolute asl 6
        assign 0x10 = relative bpl 2
        assign 0x11 = indirectY ora 5
        assign 0x15 = zeropageX ora 4
        assign 0x16 = zeropageX asl 6
        assign 0x18 = implied clc 2
        assign 0x19 = absoluteY ora 4
        assign 0x1D = absoluteX ora 4
        assign 0x1E = absoluteXRMW asl 7
        assign 0x20 = absolute jsr 6
        assign 0x21 = indirectX and_ 6
        assign 0x24 = zeropage bit 3
        assign 0x25 = zeropage and_ 3
        assign 0x26 = zeropage rol 5
        assign 0x28 = implied plp 4
        assign 0x29 = immediate and_ 2
        assign 0x2A = useAcc rol 2
        assign 0x2C = absolute bit 4
        assign 0x2D = absolute and_ 4
        assign 0x2E = absolute rol 6
        assign 0x30 = relative bmi 2
        assign 0x31 = indirectY and_ 5
        assign 0x35 = zeropageX and_ 4
        assign 0x36 = zeropageX rol 6
        assign 0x38 = implied sec 2
        assign 0x39 = absoluteY and_ 4
        assign 0x3D = absoluteX and_ 4
        assign 0x3E = absoluteXRMW rol 7
        assign 0x40 = implied rti 6
        assign 0x41 = indirectX eor 6
        assign 0x45 = zeropage eor 3
        assign 0x46 = zeropage lsr 5
        assign 0x48 = implied pha 3
        assign 0x49 = immediate eor 2
        assign 0x4A = useAcc lsr 2
        assign 0x4C = absolute jmp 3
        assign 0x4D = absolute eor 4
        assign 0x4E = absolute lsr 6
        assign 0x50 = relative bvc 2
        assign 0x51 = indirectY eor 5
        assign 0x55 = zeropageX eor 4
        assign 0x56 = zeropageX lsr 6
        assign 0x58 = implied cli 2
        assign 0x59 = absoluteY eor 4
        assign 0x5D = absoluteX eor 4
        assign 0x5E = absoluteXRMW lsr 7
        assign 0x60 = implied rts 6
        assign 0x61 = indirectX adc 6
        assign 0x65 = zeropage adc 3
        assign 0x66 = zeropage ror 5
        assign 0x68 = implied pla 4
        assign 0x69 = immediate adc 2
        assign 0x6A = useAcc ror 2
        assign 0x6C = indirect jmp 5
        assign 0x6D = absolute adc 4
        assign 0x6E = absolute ror 6
        assign 0x70 = relative bvs 2
        assign 0x71 = indirectY adc 5
        assign 0x75 = zeropageX adc 4
        assign 0x76 = zeropageX ror 6
        assign 0x78 = implied sei 2
        assign 0x79 = absoluteY adc 4
        assign 0x7D = absoluteX adc 4
        assign 0x7E = absoluteXRMW ror 7
        assign 0xB0 = relative bcs 2
        assign 0x81 = indirectX sta 6
        assign 0x84 = zeropage sty 3
        assign 0x85 = zeropage sta 3
        assign 0x86 = zeropage stx 3
        assign 0x88 = implied dey 2
        assign 0x8A = implied txa 2
        assign 0x8C = absolute sty 4
        assign 0x8D = absolute sta 4
        assign 0x8E = absolute stx 4
        assign 0x90 = relative bcc 2
        assign 0x91 = indirectYRMW sta 6
        assign 0x94 = zeropageX sty 4
        assign 0x95 = zeropageX sta 4
        assign 0x96 = zeropageY stx 4
        assign 0x98 = implied tya 2
        assign 0x99 = absoluteYRMW sta 5
        assign 0x9A = implied txs 2
        assign 0x9D = absoluteXRMW sta 5
        assign 0xA0 = immediate ldy 2
        assign 0xA1 = indirectX lda 6
        assign 0xA2 = immediate ldx 2
        assign 0xA4 = zeropage ldy 3
        assign 0xA5 = zeropage lda 3
        assign 0xA6 = zeropage ldx 3
        assign 0xA8 = implied tay 2
        assign 0xA9 = immediate lda 2
        assign 0xAA = implied tax 2
        assign 0xAC = absolute ldy 4
        assign 0xAD = absolute lda 4
        assign 0xAE = absolute ldx 4
        assign 0xB1 = indirectY lda 5
        assign 0xB4 = zeropageX ldy 4
        assign 0xB5 = zeropageX lda 4
        assign 0xB6 = zeropageY ldx 4
        assign 0xB8 = implied clv 2
        assign 0xB9 = absoluteY lda 4
        assign 0xBA = implied tsx 2
        assign 0xBC = absoluteX ldy 4
        assign 0xBD = absoluteX lda 4
        assign 0xBE = absoluteY ldx 4
        assign 0xC0 = immediate cpy 2
        assign 0xC1 = indirectX cmp 6
        assign 0xC4 = zeropage cpy 3
        assign 0xC5 = zeropage cmp 3
        assign 0xC6 = zeropage dec 5
        assign 0xC8 = implied iny 2
        assign 0xC9 = immediate cmp 2
        assign 0xCA = implied dex 2
        assign 0xCC = absolute cpy 4
        assign 0xCD = absolute cmp 4
        assign 0xCE = absolute dec 6
        assign 0xD0 = relative bne 2
        assign 0xD1 = indirectY cmp 5
        assign 0xD5 = zeropageX cmp 4
        assign 0xD6 = zeropageX dec 6
        assign 0xD8 = implied cld 2
        assign 0xD9 = absoluteY cmp 4
        assign 0xDD = absoluteX cmp 4
        assign 0xDE = absoluteXRMW dec 7
        assign 0xE0 = immediate cpx 2
        assign 0xE1 = indirectX sbc 6
        assign 0xE4 = zeropage cpx 3
        assign 0xE5 = zeropage sbc 3
        assign 0xE6 = zeropage inc 5
        assign 0xE8 = implied inx 2
        assign 0xE9 = immediate sbc 2
        assign 0xEA = implied nop 2
        assign 0xEC = absolute cpx 4
        assign 0xED = absolute sbc 4
        assign 0xEE = absolute inc 6
        assign 0xF0 = relative beq 2
        assign 0xF1 = indirectY sbc 5
        assign 0xF5 = zeropageX sbc 4
        assign 0xF6 = zeropageX inc 6
        assign 0xF8 = implied sed 2
        assign 0xF9 = absoluteY sbc 4
        assign 0xFD = absoluteX sbc 4
        assign 0xFE = absoluteXRMW inc 7
        assign _    = instrUnimplemented

dumpRAM :: Memory -> IO ()
dumpRAM mem = do
    bytes <- forM [0..0x7FFF] $ \addr -> do
        val <- readMemory mem addr
        return (fromIntegral (val .&. 0xFF) :: Word8)

    let bs = ByteStr.pack bytes
    withBinaryFile "ram_dump.bin" WriteMode $ \h -> ByteStr.hPut h bs
    putStrLn "dump done"

instrUnimplemented :: Memory -> CPURegs -> IO ()
instrUnimplemented mem regs = do
    --return ()
    pcVal <- readIORef (pc regs)
    putStrLn $ "unimplementedOpcode at " ++ showHexF pcVal
    opcode <- readMemory mem pcVal
    putStrLn $ "Value: " ++ showHexF opcode

    -- dumpRAM mem
    -- exitSuccess

-- __________Addressing Modes__________
-- immediate        value is the value right there in the instruction
immediate :: (Word16 -> Memory -> CPURegs -> Bool -> IO ()) -> Word8 -> Memory -> CPURegs -> IO ()
immediate instr baseCycles mem regs = do
    modifyIORef' (cycleCount mem) (+ fromIntegral baseCycles)

    pcVal <- readIORef (pc regs)
    operand <- readMemory mem (pcVal + 1)

    writeIORef (pc regs) (pcVal + 2)
    instr (fromIntegral operand) mem regs True

-- zeropage         value is in 0th page can be addressed with 8bits
zeropage :: (Word16 -> Memory -> CPURegs -> Bool -> IO ()) -> Word8 -> Memory -> CPURegs -> IO ()
zeropage instr baseCycles mem regs = do
    modifyIORef' (cycleCount mem) (+ fromIntegral baseCycles)

    pcVal <- readIORef (pc regs)
    operand <- readMemory mem (pcVal + 1)

    writeIORef (pc regs) (pcVal + 2)
    instr (fromIntegral operand) mem regs False

-- zeropage,X       value is at (zeropage + value of X reg) (will overflow)
zeropageX :: (Word16 -> Memory -> CPURegs -> Bool -> IO ()) -> Word8 -> Memory -> CPURegs -> IO ()
zeropageX instr baseCycles mem regs = do
    modifyIORef' (cycleCount mem) (+ fromIntegral baseCycles)

    pcVal <- readIORef (pc regs)
    operand <- readMemory mem (pcVal + 1)

    x' <- readIORef (x regs)
    let address = operand + x'

    writeIORef (pc regs) (pcVal + 2)
    instr (fromIntegral address) mem regs False

-- zeropage,Y       like zeropage X but with Y (rarely used by instructions)
zeropageY :: (Word16 -> Memory -> CPURegs -> Bool -> IO ()) -> Word8 -> Memory -> CPURegs -> IO ()
zeropageY instr baseCycles mem regs = do
    modifyIORef' (cycleCount mem) (+ fromIntegral baseCycles)

    pcVal <- readIORef (pc regs)
    operand <- readMemory mem (pcVal + 1)

    y' <- readIORef (y regs)
    let address = operand + y'

    writeIORef (pc regs) (pcVal + 2)
    instr (fromIntegral address) mem regs False

combineTwoBytes :: Word8 -> Word8 -> Word16
-- 6502 is little endian for some reason
combineTwoBytes low high = (fromIntegral high `shiftL` 8) .|. fromIntegral low

-- absolute         value is at address pointed to by the next 2 bytes (the whole memory)
absolute :: (Word16 -> Memory -> CPURegs -> Bool -> IO ()) -> Word8 -> Memory -> CPURegs -> IO ()
absolute instr baseCycles mem regs = do
    modifyIORef' (cycleCount mem) (+ fromIntegral baseCycles)

    pcVal <- readIORef (pc regs)
    operand1 <- readMemory mem (pcVal + 1)
    operand2 <- readMemory mem (pcVal + 2)
    let address = combineTwoBytes operand1 operand2

    writeIORef (pc regs) (pcVal + 3)
    instr address mem regs False

-- absolute,X       value is at Absolute address plus X
absoluteX :: (Word16 -> Memory -> CPURegs -> Bool -> IO ()) -> Word8 -> Memory -> CPURegs -> IO ()
absoluteX instr baseCycles mem regs = do
    modifyIORef' (cycleCount mem) (+ fromIntegral baseCycles)

    pcVal <- readIORef (pc regs)
    operand1 <- readMemory mem (pcVal + 1)
    operand2 <- readMemory mem (pcVal + 2)

    x' <- readIORef (x regs)
    let address = combineTwoBytes operand1 operand2 + fromIntegral x'

    when (operand1 + x' < operand1) $ modifyIORef' (cycleCount mem) (+ 1) --when page boundry crossed

    writeIORef (pc regs) (pcVal + 3)
    instr address mem regs False

-- absolute,Y       ''
absoluteY :: (Word16 -> Memory -> CPURegs -> Bool -> IO ()) -> Word8 -> Memory -> CPURegs -> IO ()
absoluteY instr baseCycles mem regs = do
    modifyIORef' (cycleCount mem) (+ fromIntegral baseCycles)

    pcVal <- readIORef (pc regs)
    operand1 <- readMemory mem (pcVal + 1)
    operand2 <- readMemory mem (pcVal + 2)

    y' <- readIORef (y regs)
    let address = combineTwoBytes operand1 operand2 + fromIntegral y'

    when (operand1 + y' < operand1) $ modifyIORef' (cycleCount mem) (+ 1) --when page boundry crossed

    writeIORef (pc regs) (pcVal + 3)
    instr address mem regs False

-- (indirect,X)     read value at operand (8 bit, only zeropage) + X (overflows), then read 2 bytes and value is memory at those 2 bytes
indirectX :: (Word16 -> Memory -> CPURegs -> Bool -> IO ()) -> Word8 -> Memory -> CPURegs -> IO ()
indirectX instr baseCycles mem regs = do
    modifyIORef' (cycleCount mem) (+ fromIntegral baseCycles)

    pcVal <- readIORef (pc regs)
    operand <- readMemory mem (pcVal + 1)

    x' <- readIORef (x regs)
    let pointer = operand + x' -- will wrap around automatically

    byte1 <- readMemory mem (fromIntegral pointer)
    byte2 <- readMemory mem (fromIntegral ((pointer + 1) :: Word8))

    let address = combineTwoBytes byte1 byte2

    writeIORef (pc regs) (pcVal + 2)
    instr address mem regs False

-- (indirect),Y     read two bytes at zero-page operand and add Y, Use that as address
indirectY :: (Word16 -> Memory -> CPURegs -> Bool -> IO ()) -> Word8 -> Memory -> CPURegs -> IO ()
indirectY instr baseCycles mem regs = do
    modifyIORef' (cycleCount mem) (+ fromIntegral baseCycles)

    pcVal <- readIORef (pc regs)
    operand <- readMemory mem (pcVal + 1)
    byte1 <- readMemory mem (fromIntegral operand)
    byte2 <- readMemory mem (fromIntegral (operand + 1))

    y' <- readIORef (y regs)
    let address = combineTwoBytes byte1 byte2 + fromIntegral y'

    when (byte1 + y' < byte1) $ modifyIORef' (cycleCount mem) (+1) --when page boundry crossed

    writeIORef (pc regs) (pcVal + 2)
    instr address mem regs False

-- For INC, DEC, ASL, ROL, LSR, ROR, STA
-- no page crossing cycle penalty, these are mostly still copies the base addressing mode
-- this code repetition is pretty horrible
absoluteXRMW :: (Word16 -> Memory -> CPURegs -> Bool -> IO ()) -> Word8 -> Memory -> CPURegs -> IO ()
absoluteXRMW instr baseCycles mem regs = do
    modifyIORef' (cycleCount mem) (+ fromIntegral baseCycles)
    pcVal <- readIORef (pc regs)
    operand1 <- readMemory mem (pcVal + 1)
    operand2 <- readMemory mem (pcVal + 2)
    x' <- readIORef (x regs)
    let address = combineTwoBytes operand1 operand2 + fromIntegral x'
    writeIORef (pc regs) (pcVal + 3)
    instr address mem regs False
indirectYRMW :: (Word16 -> Memory -> CPURegs -> Bool -> IO ()) -> Word8 -> Memory -> CPURegs -> IO ()
indirectYRMW instr baseCycles mem regs = do
    modifyIORef' (cycleCount mem) (+ fromIntegral baseCycles)
    pcVal <- readIORef (pc regs)
    operand <- readMemory mem (pcVal + 1)
    byte1 <- readMemory mem (fromIntegral operand)
    byte2 <- readMemory mem (fromIntegral (operand + 1))
    y' <- readIORef (y regs)
    let address = combineTwoBytes byte1 byte2 + fromIntegral y'
    writeIORef (pc regs) (pcVal + 2)
    instr address mem regs False
absoluteYRMW :: (Word16 -> Memory -> CPURegs -> Bool -> IO ()) -> Word8 -> Memory -> CPURegs -> IO ()
absoluteYRMW instr baseCycles mem regs = do
    modifyIORef' (cycleCount mem) (+ fromIntegral baseCycles)
    pcVal <- readIORef (pc regs)
    operand1 <- readMemory mem (pcVal + 1)
    operand2 <- readMemory mem (pcVal + 2)
    y' <- readIORef (y regs)
    let address = combineTwoBytes operand1 operand2 + fromIntegral y'
    writeIORef (pc regs) (pcVal + 3)
    instr address mem regs False

-- (indirect)       only used by JMP
indirect :: (Word16 -> Memory -> CPURegs -> Bool -> IO ()) -> Word8 -> Memory -> CPURegs -> IO ()
indirect instr baseCycles mem regs = do
    modifyIORef' (cycleCount mem) (+ fromIntegral baseCycles)

    pcVal <- readIORef (pc regs)

    operand1 <- readMemory mem (pcVal + 1)
    operand2 <- readMemory mem (pcVal + 2)

    -- this part simulates the indirect JMP page boundary bug
    let lowAddr  = combineTwoBytes operand1 operand2
    let highAddr = if (lowAddr .&. 0x00FF) == 0x00FF
                then lowAddr .&. 0xFF00   -- wrap within the same page
                else lowAddr + 1

    byte1 <- readMemory mem lowAddr
    byte2 <- readMemory mem highAddr
    let address = combineTwoBytes byte1 byte2

    writeIORef (pc regs) (pcVal + 3)
    instr address mem regs False

-- useAcc       use the value in the accumulator instead
useAcc :: (Word16 -> Memory -> CPURegs -> Bool -> IO ()) -> Word8 -> Memory -> CPURegs -> IO ()
useAcc instr baseCycles mem regs = do
    modifyIORef' (cycleCount mem) (+ fromIntegral baseCycles)

    pcVal <- readIORef (pc regs)
    writeIORef (pc regs) (pcVal + 1)
    instr 0 mem regs True

-- implied      No operands because memory is not used (actually it seems it is)
implied :: (Word16 -> Memory -> CPURegs -> Bool -> IO ()) -> Word8 -> Memory -> CPURegs -> IO ()
implied instr baseCycles mem regs = do
    modifyIORef' (cycleCount mem) (+ fromIntegral baseCycles)

    pcVal <- readIORef (pc regs)
    writeIORef (pc regs) (pcVal + 1)
    instr 0 mem regs False

-- relative     Used by branch instructions
relative :: (Word16 -> Memory -> CPURegs -> Bool -> IO ()) -> Word8 -> Memory -> CPURegs -> IO ()
relative instr baseCycles mem regs = do
    modifyIORef' (cycleCount mem) (+ fromIntegral baseCycles)

    pcVal <- readIORef (pc regs)
    operand <- readMemory mem (pcVal + 1)
    let offset = fromIntegral (fromIntegral operand :: Int8) :: Int -- keeps the sign bit (hopefully)
        branchTargetInt = fromIntegral pcVal + 2 + offset
        (branchTarget :: Word16) = fromIntegral branchTargetInt

    writeIORef (pc regs) (pcVal + 2) -- in case we dont branch

    instr branchTarget mem regs False

-- __________Instructions__________

-- %%% Arithmetic operations %%%

-- Add Memory to Accumulator with Carry
adc :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
adc address mem regs isImmediate = do
    inputVal <- if isImmediate
        then return (fromIntegral address :: Word8)
        else readMemory mem address

    acc <- readIORef (accumulator regs)
    carry <- srReadCarry (statusReg regs)

    decimal <- srReadDecimalMode (statusReg regs)

    -- Normal binary addition
    let binaryVal16 = (fromIntegral acc :: Word16) + (fromIntegral inputVal :: Word16) + (fromIntegral (fromEnum carry) :: Word16)
        binaryVal = fromIntegral binaryVal16 :: Word8

    let (newCarry, result) = if decimal
        then
            -- binary coded decimal BCD
            let accLowNibble = acc .&. 0x0F
                inputLowNibble = inputVal  .&. 0x0F
                accHighNibble = acc `shiftR` 4
                inputHighNibble = inputVal `shiftR` 4

                (lowCarry, lowResult) = bcdAddNibble carry accLowNibble inputLowNibble
                (highCarry, highResult) = bcdAddNibble lowCarry accHighNibble inputHighNibble
            in (highCarry, (highResult `shiftL` 4) + lowResult)

        else
            -- Normal binary addition
            (binaryVal16 >= 256, binaryVal)


    writeIORef (accumulator regs) result

    srWriteCarry (statusReg regs) newCarry
    srWriteZero (statusReg regs) (result == 0)

    -- these flags are not right
    srWriteNegative (statusReg regs) (testBit binaryVal 7)
    srWriteOverflow (statusReg regs) (((acc `xor` binaryVal) .&. (inputVal `xor` binaryVal) .&. 0x80) /=0)

debugNibbleVal :: Word8 -> String
debugNibbleVal input =
    let lowNibble = input  .&. 0x0F
        highNibble = input `shiftR` 4
    in show highNibble ++ "_" ++ show lowNibble

bcdAddNibble :: Bool -> Word8 -> Word8 -> (Bool,  Word8)
bcdAddNibble carry nibble1 nibble2 =
    let carryNum = (fromIntegral . fromEnum) carry :: Word8
        sumBin = nibble1 + nibble2 + carryNum
        (newCarry, result) = if sumBin > 9
                                then (True, (sumBin + 6) .&. 0x0F)
                                else (False, sumBin)
    in (newCarry, result)

-- Subtract Memory from Accumulator with Borrow
sbc :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
sbc address mem regs isImmediate = do
    inputVal <- if isImmediate
        then return (fromIntegral address :: Word8)
        else readMemory mem address

    let invertedVal = complement inputVal
    adc (fromIntegral invertedVal) mem regs True
    -- Invert then use ADC works because 6502 reuses add circuitry for sub just like I am reusing the code for it

-- %%% Transfer operations %%%

-- Load Accumulator with Memory
lda :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
lda address mem regs isImmediate = do
    inputVal <- if isImmediate
        then return (fromIntegral address :: Word8)
        else readMemory mem address

    writeIORef (accumulator regs) inputVal
    srWriteZero (statusReg regs) (inputVal == 0)
    srWriteNegative (statusReg regs) (testBit inputVal 7)

-- Load Index X with Memory
ldx :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
ldx address mem regs isImmediate = do
    inputVal <- if isImmediate
        then return (fromIntegral address :: Word8)
        else readMemory mem address

    writeIORef (x regs) inputVal
    srWriteZero (statusReg regs) (inputVal == 0)
    srWriteNegative (statusReg regs) (testBit inputVal 7)

-- Load Index Y with Memory
ldy :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
ldy address mem regs isImmediate = do
    inputVal <- if isImmediate
        then return (fromIntegral address :: Word8)
        else readMemory mem address

    writeIORef (y regs) inputVal
    srWriteZero (statusReg regs) (inputVal == 0)
    srWriteNegative (statusReg regs) (testBit inputVal 7)

-- Store Accumulator in Memory
sta :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
sta address mem regs _ = do
    acc <- readIORef (accumulator regs)
    writeMemory mem address acc

-- Store Index X in Memory
stx :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
stx address mem regs _ = do
    x' <- readIORef (x regs)
    writeMemory mem address x'

-- Store Index Y in Memory
sty :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
sty address mem regs _ = do
    y' <- readIORef (y regs)
    writeMemory mem address y'

-- Transfer Accumulator to Index X
tax :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
tax _ _ regs _ = do
    acc <- readIORef (accumulator regs)
    writeIORef (x regs) acc

    srWriteZero (statusReg regs) (acc == 0)
    srWriteNegative (statusReg regs) (testBit acc 7)

-- Transfer Accumulator to Index Y
tay :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
tay _ _ regs _ = do
    acc <- readIORef (accumulator regs)
    writeIORef (y regs) acc

    srWriteZero (statusReg regs) (acc == 0)
    srWriteNegative (statusReg regs) (testBit acc 7)

-- Transfer Stack Pointer to Index X
tsx :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
tsx _ _ regs _ = do
    sp <- readIORef (stackP regs)
    writeIORef (x regs) sp

    srWriteZero (statusReg regs) (sp == 0)
    srWriteNegative (statusReg regs) (testBit sp 7)

-- Transfer Index X to Accumulator
txa :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
txa _ _ regs _ = do
    x' <- readIORef (x regs)
    writeIORef (accumulator regs) x'

    srWriteZero (statusReg regs) (x' == 0)
    srWriteNegative (statusReg regs) (testBit x' 7)

-- Transfer Index X to Stack Pointer
txs :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
txs _ _ regs _ = do
    x' <- readIORef (x regs)
    writeIORef (stackP regs) x'

-- Transfer Index Y to Accumulator
tya :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
tya _ _ regs _ = do
    y' <- readIORef (y regs)
    writeIORef (accumulator regs) y'

    srWriteZero (statusReg regs) (y' == 0)
    srWriteNegative (statusReg regs) (testBit y' 7)

-- %%% Decrements & Increments %%%

-- Decrement Memory by One
dec :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
dec address mem regs _ = do
    memVal <- readMemory mem address
    let newVal = memVal - 1
    writeMemory mem address newVal

    srWriteZero (statusReg regs) (newVal == 0)
    srWriteNegative (statusReg regs) (testBit newVal 7)

-- Decrement Index X by One
dex :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
dex _ _ regs _ = do
    x' <- readIORef (x regs)
    let newVal = x' - 1
    writeIORef (x regs) newVal

    srWriteZero (statusReg regs) (newVal == 0)
    srWriteNegative (statusReg regs) (testBit newVal 7)

-- Decrement Index Y by One
dey :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
dey _ _ regs _ = do
    y' <- readIORef (y regs)
    let newVal = y' - 1
    writeIORef (y regs) newVal

    srWriteZero (statusReg regs) (newVal == 0)
    srWriteNegative (statusReg regs) (testBit newVal 7)

-- Increment Memory by One
inc :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
inc address mem regs _ = do
    memVal <- readMemory mem address
    let newVal = memVal + 1
    writeMemory mem address newVal

    srWriteZero (statusReg regs) (newVal == 0)
    srWriteNegative (statusReg regs) (testBit newVal 7)

-- Increment Index X by One
inx :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
inx _ _ regs _ = do
    x' <- readIORef (x regs)
    let newVal = x' + 1
    writeIORef (x regs) newVal

    srWriteZero (statusReg regs) (newVal == 0)
    srWriteNegative (statusReg regs) (testBit newVal 7)

-- Increment Index Y by One
iny :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
iny _ _ regs _ = do
    y' <- readIORef (y regs)
    let newVal = y' + 1
    writeIORef (y regs) newVal

    srWriteZero (statusReg regs) (newVal == 0)
    srWriteNegative (statusReg regs) (testBit newVal 7)

-- %%% Logical Operations %%%

-- AND Memory with Accumulator (bitwise)
and_ :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
and_ address mem regs isImmediate = do
    inputVal <- if isImmediate
        then return (fromIntegral address :: Word8)
        else readMemory mem address
    acc <- readIORef (accumulator regs)
    let result = inputVal .&. acc
    writeIORef (accumulator regs) result

    srWriteZero (statusReg regs) (result == 0)
    srWriteNegative (statusReg regs) (testBit result 7)

-- Exclusive Or (xor) Memory with Accumulator (bitwise)
eor :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
eor address mem regs isImmediate = do
    inputVal <- if isImmediate
        then return (fromIntegral address :: Word8)
        else readMemory mem address
    acc <- readIORef (accumulator regs)
    let result = inputVal `xor` acc
    writeIORef (accumulator regs) result

    srWriteZero (statusReg regs) (result == 0)
    srWriteNegative (statusReg regs) (testBit result 7)

-- OR Memory with Accumulator (bitwise)
ora :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
ora address mem regs isImmediate = do
    inputVal <- if isImmediate
        then return (fromIntegral address :: Word8)
        else readMemory mem address
    acc <- readIORef (accumulator regs)
    let result = inputVal .|. acc
    writeIORef (accumulator regs) result

    srWriteZero (statusReg regs) (result == 0)
    srWriteNegative (statusReg regs) (testBit result 7)

-- %%% Shift and Rotate %%%

-- Shift Left One Bit (Memory or Accumulator)

asl :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
asl address mem regs accNotMem = do
    inputVal <- if accNotMem
        then readIORef (accumulator regs)
        else readMemory mem address

    let result = inputVal `shiftL` 1
    if accNotMem
        then writeIORef (accumulator regs) result
        else writeMemory mem address result

    srWriteZero (statusReg regs) (result == 0)
    srWriteNegative (statusReg regs) (testBit result 7)
    srWriteCarry (statusReg regs) (testBit inputVal 7)

-- Shift Right One Bit (Memory or Accumulator)

lsr :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
lsr address mem regs accNotMem = do
    inputVal <- if accNotMem
        then readIORef (accumulator regs)
        else readMemory mem address

    let result = inputVal `shiftR` 1
    if accNotMem
        then writeIORef (accumulator regs) result
        else writeMemory mem address result

    srWriteZero (statusReg regs) (result == 0)
    srWriteNegative (statusReg regs) False
    srWriteCarry (statusReg regs) (testBit inputVal 0)

-- Rotate One Bit Left (Memory or Accumulator)
rol :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
rol address mem regs accNotMem = do
    inputVal <- if accNotMem
        then readIORef (accumulator regs)
        else readMemory mem address

    oldCarry <- srReadCarry (statusReg regs)
    let carryOut = testBit inputVal 7
        result = (inputVal `shiftL` 1) .|. (if oldCarry then 1 else 0)
    if accNotMem
        then writeIORef (accumulator regs) result
        else writeMemory mem address result

    srWriteCarry (statusReg regs) carryOut
    srWriteZero (statusReg regs) (result == 0)
    srWriteNegative (statusReg regs) (testBit result 7)

-- Rotate One Bit Right (Memory or Accumulator)
ror :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
ror address mem regs accNotMem = do
    inputVal <- if accNotMem
        then readIORef (accumulator regs)
        else readMemory mem address

    oldCarry <- srReadCarry (statusReg regs)
    let carryOut = testBit inputVal 0
        result = (inputVal `shiftR` 1) .|. (if oldCarry then 128 else 0)
    if accNotMem
        then writeIORef (accumulator regs) result
        else writeMemory mem address result

    srWriteCarry (statusReg regs) carryOut
    srWriteZero (statusReg regs) (result == 0)
    srWriteNegative (statusReg regs) (testBit result 7)

-- %%% Flag Instructions %%%

-- clear carry
clc :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
clc _ _ regs _ = do
    srWriteCarry (statusReg regs) False

-- clear decimal (BCD arithmetics disabled)
cld :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
cld _ _ regs _ = do
    srWriteDecimalMode (statusReg regs) False

-- clear interrupt disable
cli :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
cli _ _ regs _ = do
    srWriteInterruptDisable (statusReg regs) False

-- clear overflow
clv :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
clv _ _ regs _ = do
    srWriteOverflow (statusReg regs) False

-- set carry
sec :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
sec _ _ regs _ = do
    srWriteCarry (statusReg regs) True

-- set decimal (BCD arithmetics enabled)
sed :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
sed _ _ regs _ = do
    putStrLn "DECIMAL MODE SET!!!!!!!!!"
    srWriteDecimalMode (statusReg regs) True

-- set interrupt disable
sei :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
sei _ _ regs _ = do
    srWriteInterruptDisable (statusReg regs) True

-- %%% Comparisons %%%

-- Compare Accumulator with Memory Value
cmp :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
cmp address mem regs isImmediate = do
    inputVal <- if isImmediate
        then return (fromIntegral address :: Word8)
        else readMemory mem address

    acc <- readIORef (accumulator regs)

    srWriteCarry (statusReg regs) (acc >= inputVal)
    srWriteZero (statusReg regs) (acc == inputVal)
    srWriteNegative (statusReg regs) (testBit (acc-inputVal) 7)

-- Compare Index X with Memory Value
cpx :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
cpx address mem regs isImmediate = do
    inputVal <- if isImmediate
        then return (fromIntegral address :: Word8)
        else readMemory mem address

    x' <- readIORef (x regs)

    srWriteCarry (statusReg regs) (x' >= inputVal)
    srWriteZero (statusReg regs) (x' == inputVal)
    srWriteNegative (statusReg regs) (testBit (x'-inputVal) 7)

-- Compare Index y with Memory Value
cpy :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
cpy address mem regs isImmediate = do
    inputVal <- if isImmediate
        then return (fromIntegral address :: Word8)
        else readMemory mem address

    y' <- readIORef (y regs)

    srWriteCarry (statusReg regs) (y' >= inputVal)
    srWriteZero (statusReg regs) (y' == inputVal)
    srWriteNegative (statusReg regs) (testBit (y'-inputVal) 7)

-- %%% Bit Test %%%

-- Strange, Cannot be Summarised (but it does an AND operation)
bit :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
bit address mem regs _ = do
    memVal <- readMemory mem address
    acc <- readIORef (accumulator regs)

    srWriteZero (statusReg regs) (acc .&. memVal == 0)
    srWriteOverflow (statusReg regs) (testBit memVal 6)
    srWriteNegative (statusReg regs) (testBit memVal 7)

-- %%% Conditional Branch %%%

branchIf :: Bool -> Word16 -> Memory -> CPURegs -> IO ()
branchIf condition branchTarget mem regs = do
    when condition $ do
        oldPc <- readIORef (pc regs)
        if (oldPc .&. 0xFF00) == (branchTarget .&. 0xFF00)
            then modifyIORef' (cycleCount mem) (+1)
            else modifyIORef' (cycleCount mem) (+2)
        writeIORef (pc regs) branchTarget

-- Branch if Carry Clear
bcc :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
bcc branchTarget mem regs _ = do
    carry' <- srReadCarry (statusReg regs)
    branchIf (not carry') branchTarget mem regs

-- Branch if Carry Set
bcs :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
bcs branchTarget mem regs _ = do
    carry' <- srReadCarry (statusReg regs)
    branchIf carry' branchTarget mem regs

-- Branch if Equal (Zero Set)
beq :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
beq branchTarget mem regs _ = do
    zero' <- srReadZero (statusReg regs)
    branchIf zero' branchTarget mem regs

-- Branch if Not Equal (Zero clear)
bne :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
bne branchTarget mem regs _ = do
    zero' <- srReadZero (statusReg regs)
    branchIf (not zero') branchTarget mem regs

-- Branch if Minus (Negative Set)
bmi :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
bmi branchTarget mem regs _ = do
    negative' <- srReadNegative (statusReg regs)
    branchIf negative' branchTarget mem regs

-- Branch if Positive (Negative clear)
bpl :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
bpl branchTarget mem regs _ = do
    negative' <- srReadNegative (statusReg regs)
    branchIf (not negative') branchTarget mem regs

--- Branch if Overflow Clear
bvc :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
bvc branchTarget mem regs _ = do
    overflow' <- srReadOverflow (statusReg regs)
    branchIf (not overflow') branchTarget mem regs

-- Branch if Overflow Set
bvs :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
bvs branchTarget mem regs _ = do
    overflow' <- srReadOverflow (statusReg regs)
    branchIf overflow' branchTarget mem regs

-- %%% No Operation %%%

-- Do nothing
nop :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
nop _ _ _ _ = return ()

-- %%% Stack Instructions %%%

stackBase :: Word16
stackBase = 0x0100

pushStack :: Memory -> CPURegs -> Word8 -> IO ()
pushStack mem regs value = do
    stackPointer <- readIORef (stackP regs)

    let stackTop = stackBase + fromIntegral stackPointer
    writeMemory mem stackTop value -- here the decrement is done after writing to the stack

    writeIORef (stackP regs) (stackPointer - 1)

pullStack :: Memory -> CPURegs -> IO Word8
pullStack mem regs = do
    stackPointer <- readIORef (stackP regs)

    let stackTop = stackBase + fromIntegral (stackPointer + 1) -- here the increment is done 'before' reading the stack

    writeIORef (stackP regs) (stackPointer + 1)

    readMemory mem stackTop


-- Push Accumulator
pha :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
pha _ mem regs _ = do
    acc <- readIORef (accumulator regs)

    pushStack mem regs acc

-- Push Processor Status
php :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
php _ mem regs _ = do

    sr <- readIORef (statusReg regs)

    let correctedSrVal = sr .|. 0x30 -- Unused is always 1 and break is set high in this instruction

    pushStack mem regs correctedSrVal

-- Pull Accumulator
pla :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
pla _ mem regs _ = do

    stackVal <- pullStack mem regs

    writeIORef (accumulator regs) stackVal

    srWriteNegative (statusReg regs) (testBit stackVal 7)
    srWriteZero (statusReg regs) (stackVal == 0)

-- Pull Processor Status
plp :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
plp _ mem regs _ = do

    srValFromStack <- pullStack mem regs

    -- Ensure bit 5 (Unused) = 1, bit 4 (Break) = 0
    let correctedSrVal = (srValFromStack .|. 0x20) .&. 0xEF

    writeIORef (statusReg regs) correctedSrVal

-- %%% Jumps & Subroutines %%%

--Jump to Location
jmp :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
jmp address _ regs _ = do
    writeIORef (pc regs) address

jsrRecalcutateAddress :: Word16 -> Memory -> IO Word16
jsrRecalcutateAddress operandFirstAddress mem = do
    operand1 <- readMemory mem operandFirstAddress
    operand2 <- readMemory mem (operandFirstAddress + 1)
    let address = combineTwoBytes operand1 operand2

    return address


-- Jump to Subroutine
jsr :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
jsr address mem regs _ = do
    pCounter <- readIORef (pc regs)
    let instStartPCounter = pCounter - 3 -- absolute addressing function previously incremented PC which is unhelpful here

        returnAddress = instStartPCounter + 2 -- "the return address on the stack points 1 byte before the start of the next instruction"
        lowByte  = fromIntegral returnAddress              :: Word8
        highByte = fromIntegral (returnAddress `shiftR` 8) :: Word8


    pushStack mem regs highByte

    pushStack mem regs lowByte -- is this the correct order I'm not sure

    --address2 <- jsrRecalcutateAddress (instStartPCounter+1) mem -- I hate this 

    writeIORef (pc regs) address

-- Return from Subroutine
rts :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
rts _ mem regs _ = do
    lowByte <- pullStack mem regs
    highByte <- pullStack mem regs

    let returnAddress = combineTwoBytes lowByte highByte + 1

    writeIORef (pc regs) returnAddress

-- %%% Interrupts %%%

irqVector :: Word16
irqVector = 0xFFFE

-- IRQ interrupt
irqShared :: Memory -> CPURegs -> IO ()
irqShared mem regs = do
    pCounter <- readIORef (pc regs)

    let returnAddress = pCounter

        highByte = fromIntegral (returnAddress `shiftR` 8)  :: Word8
        lowByte  = fromIntegral returnAddress               :: Word8

    sr <- readIORef (statusReg regs)
    let correctedSrVal = sr .|. 0x20 -- ensure unused is set

    pushStack mem regs highByte
    pushStack mem regs lowByte
    pushStack mem regs correctedSrVal

    srWriteInterruptDisable (statusReg regs) True

    lowByte2 <- readMemory mem irqVector
    highByte2 <- readMemory mem (irqVector + 1) --What if it overflows????????????update lowbyte???

    let jumpAddress :: Word16
        jumpAddress = (fromIntegral highByte2 :: Word16) `shiftL` 8 + (fromIntegral lowByte2 :: Word16)

    writeIORef (pc regs) jumpAddress

irq :: Memory -> CPURegs -> IO ()
irq mem regs = do
    srWriteBreak (statusReg regs) False
    irqShared mem regs

-- break: Software IRQ
brk :: (Word16 -> Memory -> CPURegs -> Bool -> IO ()) -> Word8 -> Memory -> CPURegs -> IO ()
brk _ baseCycles mem regs = do
    modifyIORef' (cycleCount mem) (+ fromIntegral baseCycles)

    pcVal <- readIORef (pc regs)
    writeIORef (pc regs) (pcVal + 2)
    srWriteBreak (statusReg regs) True
    irqShared mem regs

-- Return From Interrupt
rti :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
rti _ mem regs _ = do
    srPushed <- pullStack mem regs
    lowByte <- pullStack mem regs
    highByte <- pullStack mem regs
    let returnAddress = (fromIntegral highByte `shiftL` 8) + fromIntegral lowByte

    -- keep break and unused at their current value
    srCurrent <- readIORef (statusReg regs)
    let mask = complement 0x30
        correctedSrVal = (srCurrent .&. 0x30) .|. (srPushed .&. mask)

    writeIORef (statusReg regs) correctedSrVal
    writeIORef (pc regs) returnAddress

-- %%% Illegal Instructions %%%

-- another nop
ill_0C :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
ill_0C _ _ _ _ = return ()