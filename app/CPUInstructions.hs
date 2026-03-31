module CPUInstructions where

import Data.Word (Word16, Word8)
import Data.Int (Int8)
import Data.Bits ( Bits((.|.), testBit, complement, (.&.), shiftL, shiftR, xor))
import Data.IORef (readIORef, modifyIORef')
import Control.Monad (when, forM)
import qualified Data.Vector as IBVector

-- temp
--import System.Exit (exitSuccess)
import qualified Data.ByteString as ByteStr
import GHC.IO.Handle.FD (withBinaryFile)
import GHC.IO.IOMode (IOMode(WriteMode))

import Utilities (showHexF)
import MemoryRegisters (readMemory, writeMemory, Memory (cycleCount, regs), CPURegs(pc, x, y, stackP, accumulator, statusReg), readRegs, writeRegs)

-- __________StatusReg Read Helpers__________
srReadBit :: Word8 -> Memory -> IO Bool
srReadBit mask mem = do
    val <- readIORef (statusReg (regs mem))
    return $ (val .&. mask) /=0
srReadCarry            :: Memory -> IO Bool; srReadCarry            = srReadBit 0x01
srReadZero             :: Memory -> IO Bool; srReadZero             = srReadBit 0x02
srReadInterruptDisable :: Memory -> IO Bool; srReadInterruptDisable = srReadBit 0x04
srReadDecimalMode      :: Memory -> IO Bool; srReadDecimalMode      = srReadBit 0x08
srReadBreak            :: Memory -> IO Bool; srReadBreak            = srReadBit 0x10
srReadUnused           ::           IO Bool; srReadUnused           = return True --haha
srReadOverflow         :: Memory -> IO Bool; srReadOverflow         = srReadBit 0x40
srReadNegative         :: Memory -> IO Bool; srReadNegative         = srReadBit 0x80

-- __________StatusReg Write Helpers__________
srWriteBit :: Word8 -> Memory -> Bool -> IO ()
srWriteBit mask mem val = modifyIORef' (statusReg (regs mem)) update
  where
    update sr = if val then sr .|. mask else sr .&. complement mask

srWriteCarry            :: Memory -> Bool -> IO (); srWriteCarry            = srWriteBit 0x01
srWriteZero             :: Memory -> Bool -> IO (); srWriteZero             = srWriteBit 0x02
srWriteInterruptDisable :: Memory -> Bool -> IO (); srWriteInterruptDisable = srWriteBit 0x04
srWriteDecimalMode      :: Memory -> Bool -> IO (); srWriteDecimalMode      = srWriteBit 0x08
srWriteBreak            :: Memory -> Bool -> IO (); srWriteBreak            = srWriteBit 0x10
srWriteUnused           :: Memory -> Bool -> IO (); srWriteUnused           = srWriteBit 0x20
srWriteOverflow         :: Memory -> Bool -> IO (); srWriteOverflow         = srWriteBit 0x40
srWriteNegative         :: Memory -> Bool -> IO (); srWriteNegative         = srWriteBit 0x80

opcodeTable :: IBVector.Vector (Memory -> IO ())
opcodeTable = IBVector.generate 256 assign
    where
        assign :: Int -> (Memory -> IO ())
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

instrUnimplemented :: Memory -> IO ()
instrUnimplemented mem = do
    --return ()
    --pcVal <- readRegs pc mem
    pcVal <- readRegs pc mem
    putStrLn $ "unimplementedOpcode at " ++ showHexF pcVal
    opcode <- readMemory mem pcVal
    putStrLn $ "Value: " ++ showHexF opcode

    -- dumpRAM mem
    -- exitSuccess

-- __________Addressing Modes__________
-- immediate        value is the value right there in the instruction
immediate :: (Word16 -> Memory -> Bool -> IO ()) -> Word8 -> Memory -> IO ()
immediate instr baseCycles mem = do
    modifyIORef' (cycleCount mem) (+ fromIntegral baseCycles)

    pcVal <- readRegs pc mem
    operand <- readMemory mem (pcVal + 1)

    writeRegs pc mem (pcVal + 2)
    writeRegs pc mem (pcVal + 2)
    instr (fromIntegral operand) mem True

-- zeropage         value is in 0th page can be addressed with 8bits
zeropage :: (Word16 -> Memory -> Bool -> IO ()) -> Word8 -> Memory -> IO ()
zeropage instr baseCycles mem = do
    modifyIORef' (cycleCount mem) (+ fromIntegral baseCycles)

    pcVal <- readRegs pc mem
    operand <- readMemory mem (pcVal + 1)

    writeRegs pc mem (pcVal + 2)
    instr (fromIntegral operand) mem False

-- zeropage,X       value is at (zeropage + value of X reg) (will overflow)
zeropageX :: (Word16 -> Memory -> Bool -> IO ()) -> Word8 -> Memory -> IO ()
zeropageX instr baseCycles mem = do
    modifyIORef' (cycleCount mem) (+ fromIntegral baseCycles)

    pcVal <- readRegs pc mem
    operand <- readMemory mem (pcVal + 1)

    x' <- readRegs x mem
    let address = operand + x'

    writeRegs pc mem (pcVal + 2)
    instr (fromIntegral address) mem False

-- zeropage,Y       like zeropage X but with Y (rarely used by instructions)
zeropageY :: (Word16 -> Memory -> Bool -> IO ()) -> Word8 -> Memory -> IO ()
zeropageY instr baseCycles mem = do
    modifyIORef' (cycleCount mem) (+ fromIntegral baseCycles)

    pcVal <- readRegs pc mem
    operand <- readMemory mem (pcVal + 1)

    y' <- readRegs y mem
    let address = operand + y'

    writeRegs pc mem (pcVal + 2)
    instr (fromIntegral address) mem False

combineTwoBytes :: Word8 -> Word8 -> Word16
-- 6502 is little endian for some reason
combineTwoBytes low high = (fromIntegral high `shiftL` 8) .|. fromIntegral low

-- absolute         value is at address pointed to by the next 2 bytes (the whole memory)
absolute :: (Word16 -> Memory -> Bool -> IO ()) -> Word8 -> Memory -> IO ()
absolute instr baseCycles mem = do
    modifyIORef' (cycleCount mem) (+ fromIntegral baseCycles)

    pcVal <- readRegs pc mem
    operand1 <- readMemory mem (pcVal + 1)
    operand2 <- readMemory mem (pcVal + 2)
    let address = combineTwoBytes operand1 operand2

    writeRegs pc mem (pcVal + 3)
    instr address mem False

-- absolute,X       value is at Absolute address plus X
absoluteX :: (Word16 -> Memory -> Bool -> IO ()) -> Word8 -> Memory -> IO ()
absoluteX instr baseCycles mem = do
    modifyIORef' (cycleCount mem) (+ fromIntegral baseCycles)

    pcVal <- readRegs pc mem
    operand1 <- readMemory mem (pcVal + 1)
    operand2 <- readMemory mem (pcVal + 2)

    x' <- readRegs x mem
    let address = combineTwoBytes operand1 operand2 + fromIntegral x'

    when (operand1 + x' < operand1) $ modifyIORef' (cycleCount mem) (+ 1) --when page boundry crossed

    writeRegs pc mem (pcVal + 3)
    instr address mem False

-- absolute,Y       ''
absoluteY :: (Word16 -> Memory -> Bool -> IO ()) -> Word8 -> Memory -> IO ()
absoluteY instr baseCycles mem = do
    modifyIORef' (cycleCount mem) (+ fromIntegral baseCycles)

    pcVal <- readRegs pc mem
    operand1 <- readMemory mem (pcVal + 1)
    operand2 <- readMemory mem (pcVal + 2)

    y' <- readRegs y mem
    let address = combineTwoBytes operand1 operand2 + fromIntegral y'

    when (operand1 + y' < operand1) $ modifyIORef' (cycleCount mem) (+ 1) --when page boundry crossed

    writeRegs pc mem (pcVal + 3)
    instr address mem False

-- (indirect,X)     read value at operand (8 bit, only zeropage) + X (overflows), then read 2 bytes and value is memory at those 2 bytes
indirectX :: (Word16 -> Memory -> Bool -> IO ()) -> Word8 -> Memory -> IO ()
indirectX instr baseCycles mem = do
    modifyIORef' (cycleCount mem) (+ fromIntegral baseCycles)

    pcVal <- readRegs pc mem
    operand <- readMemory mem (pcVal + 1)

    x' <- readRegs x mem
    let pointer = operand + x' -- will wrap around automatically

    byte1 <- readMemory mem (fromIntegral pointer)
    byte2 <- readMemory mem (fromIntegral ((pointer + 1) :: Word8))

    let address = combineTwoBytes byte1 byte2

    writeRegs pc mem (pcVal + 2)
    instr address mem False

-- (indirect),Y     read two bytes at zero-page operand and add Y, Use that as address
indirectY :: (Word16 -> Memory -> Bool -> IO ()) -> Word8 -> Memory -> IO ()
indirectY instr baseCycles mem = do
    modifyIORef' (cycleCount mem) (+ fromIntegral baseCycles)

    pcVal <- readRegs pc mem
    operand <- readMemory mem (pcVal + 1)
    byte1 <- readMemory mem (fromIntegral operand)
    byte2 <- readMemory mem (fromIntegral (operand + 1))

    y' <- readRegs y mem
    let address = combineTwoBytes byte1 byte2 + fromIntegral y'

    when (byte1 + y' < byte1) $ modifyIORef' (cycleCount mem) (+1) --when page boundry crossed

    writeRegs pc mem (pcVal + 2)
    instr address mem False

-- For INC, DEC, ASL, ROL, LSR, ROR, STA
-- no page crossing cycle penalty, these are mostly still copies the base addressing mode
-- this code repetition is pretty horrible
absoluteXRMW :: (Word16 -> Memory -> Bool -> IO ()) -> Word8 -> Memory -> IO ()
absoluteXRMW instr baseCycles mem = do
    modifyIORef' (cycleCount mem) (+ fromIntegral baseCycles)
    pcVal <- readRegs pc mem
    operand1 <- readMemory mem (pcVal + 1)
    operand2 <- readMemory mem (pcVal + 2)
    x' <- readRegs x mem
    let address = combineTwoBytes operand1 operand2 + fromIntegral x'
    writeRegs pc mem (pcVal + 3)
    instr address mem False
indirectYRMW :: (Word16 -> Memory -> Bool -> IO ()) -> Word8 -> Memory -> IO ()
indirectYRMW instr baseCycles mem = do
    modifyIORef' (cycleCount mem) (+ fromIntegral baseCycles)
    pcVal <- readRegs pc mem
    operand <- readMemory mem (pcVal + 1)
    byte1 <- readMemory mem (fromIntegral operand)
    byte2 <- readMemory mem (fromIntegral (operand + 1))
    y' <- readRegs y mem
    let address = combineTwoBytes byte1 byte2 + fromIntegral y'
    writeRegs pc mem (pcVal + 2)
    instr address mem False
absoluteYRMW :: (Word16 -> Memory -> Bool -> IO ()) -> Word8 -> Memory -> IO ()
absoluteYRMW instr baseCycles mem = do
    modifyIORef' (cycleCount mem) (+ fromIntegral baseCycles)
    pcVal <- readRegs pc mem
    operand1 <- readMemory mem (pcVal + 1)
    operand2 <- readMemory mem (pcVal + 2)
    y' <- readRegs y mem
    let address = combineTwoBytes operand1 operand2 + fromIntegral y'
    writeRegs pc mem (pcVal + 3)
    instr address mem False

-- (indirect)       only used by JMP
indirect :: (Word16 -> Memory -> Bool -> IO ()) -> Word8 -> Memory -> IO ()
indirect instr baseCycles mem = do
    modifyIORef' (cycleCount mem) (+ fromIntegral baseCycles)

    pcVal <- readRegs pc mem

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

    writeRegs pc mem (pcVal + 3)
    instr address mem False

-- useAcc       use the value in the accumulator instead
useAcc :: (Word16 -> Memory -> Bool -> IO ()) -> Word8 -> Memory -> IO ()
useAcc instr baseCycles mem = do
    modifyIORef' (cycleCount mem) (+ fromIntegral baseCycles)

    pcVal <- readRegs pc mem
    writeRegs pc mem (pcVal + 1)
    instr 0 mem True

-- implied      No operands because memory is not used (actually it seems it is)
implied :: (Word16 -> Memory -> Bool -> IO ()) -> Word8 -> Memory -> IO ()
implied instr baseCycles mem = do
    modifyIORef' (cycleCount mem) (+ fromIntegral baseCycles)

    pcVal <- readRegs pc mem
    writeRegs pc mem (pcVal + 1)
    instr 0 mem False

-- relative     Used by branch instructions
relative :: (Word16 -> Memory -> Bool -> IO ()) -> Word8 -> Memory -> IO ()
relative instr baseCycles mem = do
    modifyIORef' (cycleCount mem) (+ fromIntegral baseCycles)

    pcVal <- readRegs pc mem
    operand <- readMemory mem (pcVal + 1)
    let offset = fromIntegral (fromIntegral operand :: Int8) :: Int -- keeps the sign bit (hopefully)
        branchTargetInt = fromIntegral pcVal + 2 + offset
        (branchTarget :: Word16) = fromIntegral branchTargetInt

    writeRegs pc mem (pcVal + 2) -- in case we dont branch

    instr branchTarget mem False

-- __________Instructions__________

-- %%% Arithmetic operations %%%

-- Add Memory to Accumulator with Carry
adc :: Word16 -> Memory -> Bool -> IO ()
adc address mem isImmediate = do
    inputVal <- if isImmediate
        then return (fromIntegral address :: Word8)
        else readMemory mem address

    acc <- readRegs accumulator mem
    carry <- srReadCarry mem

    decimal <- srReadDecimalMode mem

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


    writeRegs accumulator mem result

    srWriteCarry mem newCarry
    srWriteZero mem (result == 0)

    -- these flags are not right
    srWriteNegative mem (testBit binaryVal 7)
    srWriteOverflow mem (((acc `xor` binaryVal) .&. (inputVal `xor` binaryVal) .&. 0x80) /=0)

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
sbc :: Word16 -> Memory -> Bool -> IO ()
sbc address mem isImmediate = do
    inputVal <- if isImmediate
        then return (fromIntegral address :: Word8)
        else readMemory mem address

    let invertedVal = complement inputVal
    adc (fromIntegral invertedVal) mem True
    -- Invert then use ADC works because 6502 reuses add circuitry for sub just like I am reusing the code for it

-- %%% Transfer operations %%%

-- Load Accumulator with Memory
lda :: Word16 -> Memory -> Bool -> IO ()
lda address mem isImmediate = do
    inputVal <- if isImmediate
        then return (fromIntegral address :: Word8)
        else readMemory mem address

    writeRegs accumulator mem inputVal
    srWriteZero mem (inputVal == 0)
    srWriteNegative mem (testBit inputVal 7)

-- Load Index X with Memory
ldx :: Word16 -> Memory -> Bool -> IO ()
ldx address mem isImmediate = do
    inputVal <- if isImmediate
        then return (fromIntegral address :: Word8)
        else readMemory mem address

    writeRegs x mem inputVal
    srWriteZero mem (inputVal == 0)
    srWriteNegative mem (testBit inputVal 7)

-- Load Index Y with Memory
ldy :: Word16 -> Memory -> Bool -> IO ()
ldy address mem isImmediate = do
    inputVal <- if isImmediate
        then return (fromIntegral address :: Word8)
        else readMemory mem address

    writeRegs y mem inputVal
    srWriteZero mem (inputVal == 0)
    srWriteNegative mem (testBit inputVal 7)

-- Store Accumulator in Memory
sta :: Word16 -> Memory -> Bool -> IO ()
sta address mem _ = do
    acc <- readRegs accumulator mem
    writeMemory mem address acc

-- Store Index X in Memory
stx :: Word16 -> Memory -> Bool -> IO ()
stx address mem _ = do
    x' <- readRegs x mem
    writeMemory mem address x'

-- Store Index Y in Memory
sty :: Word16 -> Memory -> Bool -> IO ()
sty address mem _ = do
    y' <- readRegs y mem
    writeMemory mem address y'

-- Transfer Accumulator to Index X
tax :: Word16 -> Memory -> Bool -> IO ()
tax _ mem _ = do
    acc <- readRegs accumulator mem
    writeRegs x mem acc

    srWriteZero mem (acc == 0)
    srWriteNegative mem (testBit acc 7)

-- Transfer Accumulator to Index Y
tay :: Word16 -> Memory -> Bool -> IO ()
tay _ mem _ = do
    acc <- readRegs accumulator mem
    writeRegs y mem acc

    srWriteZero mem (acc == 0)
    srWriteNegative mem (testBit acc 7)

-- Transfer Stack Pointer to Index X
tsx :: Word16 -> Memory -> Bool -> IO ()
tsx _ mem _ = do
    sp <- readRegs stackP mem
    writeRegs x mem sp

    srWriteZero mem (sp == 0)
    srWriteNegative mem (testBit sp 7)

-- Transfer Index X to Accumulator
txa :: Word16 -> Memory -> Bool -> IO ()
txa _ mem _ = do
    x' <- readRegs x mem
    writeRegs accumulator mem x'

    srWriteZero mem (x' == 0)
    srWriteNegative mem (testBit x' 7)

-- Transfer Index X to Stack Pointer
txs :: Word16 -> Memory -> Bool -> IO ()
txs _ mem _ = do
    x' <- readRegs x mem
    writeRegs stackP mem x'

-- Transfer Index Y to Accumulator
tya :: Word16 -> Memory -> Bool -> IO ()
tya _ mem _ = do
    y' <- readRegs y mem
    writeRegs accumulator mem y'

    srWriteZero mem (y' == 0)
    srWriteNegative mem (testBit y' 7)

-- %%% Decrements & Increments %%%

-- Decrement Memory by One
dec :: Word16 -> Memory -> Bool -> IO ()
dec address mem _ = do
    memVal <- readMemory mem address
    let newVal = memVal - 1
    writeMemory mem address newVal

    srWriteZero mem (newVal == 0)
    srWriteNegative mem (testBit newVal 7)

-- Decrement Index X by One
dex :: Word16 -> Memory -> Bool -> IO ()
dex _ mem _ = do
    x' <- readRegs x mem
    let newVal = x' - 1
    writeRegs x mem newVal

    srWriteZero mem (newVal == 0)
    srWriteNegative mem (testBit newVal 7)

-- Decrement Index Y by One
dey :: Word16 -> Memory -> Bool -> IO ()
dey _ mem _ = do
    y' <- readRegs y mem
    let newVal = y' - 1
    writeRegs y mem newVal

    srWriteZero mem (newVal == 0)
    srWriteNegative mem (testBit newVal 7)

-- Increment Memory by One
inc :: Word16 -> Memory -> Bool -> IO ()
inc address mem _ = do
    memVal <- readMemory mem address
    let newVal = memVal + 1
    writeMemory mem address newVal

    srWriteZero mem (newVal == 0)
    srWriteNegative mem (testBit newVal 7)

-- Increment Index X by One
inx :: Word16 -> Memory -> Bool -> IO ()
inx _ mem _ = do
    x' <- readRegs x mem
    let newVal = x' + 1
    writeRegs x mem newVal

    srWriteZero mem (newVal == 0)
    srWriteNegative mem (testBit newVal 7)

-- Increment Index Y by One
iny :: Word16 -> Memory -> Bool -> IO ()
iny _ mem _ = do
    y' <- readRegs y mem
    let newVal = y' + 1
    writeRegs y mem newVal

    srWriteZero mem (newVal == 0)
    srWriteNegative mem (testBit newVal 7)

-- %%% Logical Operations %%%

-- AND Memory with Accumulator (bitwise)
and_ :: Word16 -> Memory -> Bool -> IO ()
and_ address mem isImmediate = do
    inputVal <- if isImmediate
        then return (fromIntegral address :: Word8)
        else readMemory mem address
    acc <- readRegs accumulator mem
    let result = inputVal .&. acc
    writeRegs accumulator mem result

    srWriteZero mem (result == 0)
    srWriteNegative mem (testBit result 7)

-- Exclusive Or (xor) Memory with Accumulator (bitwise)
eor :: Word16 -> Memory -> Bool -> IO ()
eor address mem isImmediate = do
    inputVal <- if isImmediate
        then return (fromIntegral address :: Word8)
        else readMemory mem address
    acc <- readRegs accumulator mem
    let result = inputVal `xor` acc
    writeRegs accumulator mem result

    srWriteZero mem (result == 0)
    srWriteNegative mem (testBit result 7)

-- OR Memory with Accumulator (bitwise)
ora :: Word16 -> Memory -> Bool -> IO ()
ora address mem isImmediate = do
    inputVal <- if isImmediate
        then return (fromIntegral address :: Word8)
        else readMemory mem address
    acc <- readRegs accumulator mem
    let result = inputVal .|. acc
    writeRegs accumulator mem result

    srWriteZero mem (result == 0)
    srWriteNegative mem (testBit result 7)

-- %%% Shift and Rotate %%%

-- Shift Left One Bit (Memory or Accumulator)

asl :: Word16 -> Memory -> Bool -> IO ()
asl address mem accNotMem = do
    inputVal <- if accNotMem
        then readRegs accumulator mem
        else readMemory mem address

    let result = inputVal `shiftL` 1
    if accNotMem
        then writeRegs accumulator mem result
        else writeMemory mem address result

    srWriteZero mem (result == 0)
    srWriteNegative mem (testBit result 7)
    srWriteCarry mem (testBit inputVal 7)

-- Shift Right One Bit (Memory or Accumulator)

lsr :: Word16 -> Memory -> Bool -> IO ()
lsr address mem accNotMem = do
    inputVal <- if accNotMem
        then readRegs accumulator mem
        else readMemory mem address

    let result = inputVal `shiftR` 1
    if accNotMem
        then writeRegs accumulator mem result
        else writeMemory mem address result

    srWriteZero mem (result == 0)
    srWriteNegative mem False
    srWriteCarry mem (testBit inputVal 0)

-- Rotate One Bit Left (Memory or Accumulator)
rol :: Word16 -> Memory -> Bool -> IO ()
rol address mem accNotMem = do
    inputVal <- if accNotMem
        then readRegs accumulator mem
        else readMemory mem address

    oldCarry <- srReadCarry mem
    let carryOut = testBit inputVal 7
        result = (inputVal `shiftL` 1) .|. (if oldCarry then 1 else 0)
    if accNotMem
        then writeRegs accumulator mem result
        else writeMemory mem address result

    srWriteCarry mem carryOut
    srWriteZero mem (result == 0)
    srWriteNegative mem (testBit result 7)

-- Rotate One Bit Right (Memory or Accumulator)
ror :: Word16 -> Memory -> Bool -> IO ()
ror address mem accNotMem = do
    inputVal <- if accNotMem
        then readRegs accumulator mem
        else readMemory mem address

    oldCarry <- srReadCarry mem
    let carryOut = testBit inputVal 0
        result = (inputVal `shiftR` 1) .|. (if oldCarry then 128 else 0)
    if accNotMem
        then writeRegs accumulator mem result
        else writeMemory mem address result

    srWriteCarry mem carryOut
    srWriteZero mem (result == 0)
    srWriteNegative mem (testBit result 7)

-- %%% Flag Instructions %%%

-- clear carry
clc :: Word16 -> Memory -> Bool -> IO ()
clc _ mem _ = do
    srWriteCarry mem False

-- clear decimal (BCD arithmetics disabled)
cld :: Word16 -> Memory -> Bool -> IO ()
cld _ mem _ = do
    srWriteDecimalMode mem False

-- clear interrupt disable
cli :: Word16 -> Memory -> Bool -> IO ()
cli _ mem _ = do
    srWriteInterruptDisable mem False

-- clear overflow
clv :: Word16 -> Memory -> Bool -> IO ()
clv _ mem _ = do
    srWriteOverflow mem False

-- set carry
sec :: Word16 -> Memory -> Bool -> IO ()
sec _ mem _ = do
    srWriteCarry mem True

-- set decimal (BCD arithmetics enabled)
sed :: Word16 -> Memory -> Bool -> IO ()
sed _ mem _ = do
    putStrLn "DECIMAL MODE SET!!!!!!!!!"
    srWriteDecimalMode mem True

-- set interrupt disable
sei :: Word16 -> Memory -> Bool -> IO ()
sei _ mem _ = do
    srWriteInterruptDisable mem True

-- %%% Comparisons %%%

-- Compare Accumulator with Memory Value
cmp :: Word16 -> Memory -> Bool -> IO ()
cmp address mem isImmediate = do
    inputVal <- if isImmediate
        then return (fromIntegral address :: Word8)
        else readMemory mem address

    acc <- readRegs accumulator mem

    srWriteCarry mem (acc >= inputVal)
    srWriteZero mem (acc == inputVal)
    srWriteNegative mem (testBit (acc-inputVal) 7)

-- Compare Index X with Memory Value
cpx :: Word16 -> Memory -> Bool -> IO ()
cpx address mem isImmediate = do
    inputVal <- if isImmediate
        then return (fromIntegral address :: Word8)
        else readMemory mem address

    x' <- readRegs x mem

    srWriteCarry mem (x' >= inputVal)
    srWriteZero mem (x' == inputVal)
    srWriteNegative mem (testBit (x'-inputVal) 7)

-- Compare Index y with Memory Value
cpy :: Word16 -> Memory -> Bool -> IO ()
cpy address mem isImmediate = do
    inputVal <- if isImmediate
        then return (fromIntegral address :: Word8)
        else readMemory mem address

    y' <- readRegs y mem

    srWriteCarry mem (y' >= inputVal)
    srWriteZero mem (y' == inputVal)
    srWriteNegative mem (testBit (y'-inputVal) 7)

-- %%% Bit Test %%%

-- Strange, Cannot be Summarised (but it does an AND operation)
bit :: Word16 -> Memory -> Bool -> IO ()
bit address mem _ = do
    memVal <- readMemory mem address
    acc <- readRegs accumulator mem

    srWriteZero mem (acc .&. memVal == 0)
    srWriteOverflow mem (testBit memVal 6)
    srWriteNegative mem (testBit memVal 7)

-- %%% Conditional Branch %%%

branchIf :: Bool -> Word16 -> Memory -> IO ()
branchIf condition branchTarget mem = do
    when condition $ do
        oldPc <- readRegs pc mem
        if (oldPc .&. 0xFF00) == (branchTarget .&. 0xFF00)
            then modifyIORef' (cycleCount mem) (+1)
            else modifyIORef' (cycleCount mem) (+2)
        writeRegs pc mem branchTarget

-- Branch if Carry Clear
bcc :: Word16 -> Memory -> Bool -> IO ()
bcc branchTarget mem _ = do
    carry' <- srReadCarry mem
    branchIf (not carry') branchTarget mem

-- Branch if Carry Set
bcs :: Word16 -> Memory -> Bool -> IO ()
bcs branchTarget mem _ = do
    carry' <- srReadCarry mem
    branchIf carry' branchTarget mem

-- Branch if Equal (Zero Set)
beq :: Word16 -> Memory -> Bool -> IO ()
beq branchTarget mem _ = do
    zero' <- srReadZero mem
    branchIf zero' branchTarget mem

-- Branch if Not Equal (Zero clear)
bne :: Word16 -> Memory -> Bool -> IO ()
bne branchTarget mem _ = do
    zero' <- srReadZero mem
    branchIf (not zero') branchTarget mem

-- Branch if Minus (Negative Set)
bmi :: Word16 -> Memory -> Bool -> IO ()
bmi branchTarget mem _ = do
    negative' <- srReadNegative mem
    branchIf negative' branchTarget mem

-- Branch if Positive (Negative clear)
bpl :: Word16 -> Memory -> Bool -> IO ()
bpl branchTarget mem _ = do
    negative' <- srReadNegative mem
    branchIf (not negative') branchTarget mem

--- Branch if Overflow Clear
bvc :: Word16 -> Memory -> Bool -> IO ()
bvc branchTarget mem _ = do
    overflow' <- srReadOverflow mem
    branchIf (not overflow') branchTarget mem

-- Branch if Overflow Set
bvs :: Word16 -> Memory -> Bool -> IO ()
bvs branchTarget mem _ = do
    overflow' <- srReadOverflow mem
    branchIf overflow' branchTarget mem

-- %%% No Operation %%%

-- Do nothing
nop :: Word16 -> Memory -> Bool -> IO ()
nop _ _ _ = return ()

-- %%% Stack Instructions %%%

stackBase :: Word16
stackBase = 0x0100

pushStack :: Memory -> Word8 -> IO ()
pushStack mem value = do
    stackPointer <- readRegs stackP mem

    let stackTop = stackBase + fromIntegral stackPointer
    writeMemory mem stackTop value -- here the decrement is done after writing to the stack

    writeRegs stackP mem (stackPointer - 1)

pullStack :: Memory -> IO Word8
pullStack mem = do
    stackPointer <- readRegs stackP mem

    let stackTop = stackBase + fromIntegral (stackPointer + 1) -- here the increment is done 'before' reading the stack

    writeRegs stackP mem (stackPointer + 1)

    readMemory mem stackTop


-- Push Accumulator
pha :: Word16 -> Memory -> Bool -> IO ()
pha _ mem _ = do
    acc <- readRegs accumulator mem

    pushStack mem acc

-- Push Processor Status
php :: Word16 -> Memory -> Bool -> IO ()
php _ mem _ = do

    sr <- readRegs statusReg mem

    let correctedSrVal = sr .|. 0x30 -- Unused is always 1 and break is set high in this instruction

    pushStack mem correctedSrVal

-- Pull Accumulator
pla :: Word16 -> Memory -> Bool -> IO ()
pla _ mem _ = do

    stackVal <- pullStack mem

    writeRegs accumulator mem stackVal

    srWriteNegative mem (testBit stackVal 7)
    srWriteZero mem (stackVal == 0)

-- Pull Processor Status
plp :: Word16 -> Memory -> Bool -> IO ()
plp _ mem _ = do

    srValFromStack <- pullStack mem

    -- Ensure bit 5 (Unused) = 1, bit 4 (Break) = 0
    let correctedSrVal = (srValFromStack .|. 0x20) .&. 0xEF

    writeRegs statusReg mem correctedSrVal

-- %%% Jumps & Subroutines %%%

--Jump to Location
jmp :: Word16 -> Memory -> Bool -> IO ()
jmp address mem _ = do
    writeRegs pc mem address

jsrRecalcutateAddress :: Word16 -> Memory -> IO Word16
jsrRecalcutateAddress operandFirstAddress mem = do
    operand1 <- readMemory mem operandFirstAddress
    operand2 <- readMemory mem (operandFirstAddress + 1)
    let address = combineTwoBytes operand1 operand2

    return address


-- Jump to Subroutine
jsr :: Word16 -> Memory -> Bool -> IO ()
jsr address mem _ = do
    pCounter <- readRegs pc mem
    let instStartPCounter = pCounter - 3 -- absolute addressing function previously incremented PC which is unhelpful here

        returnAddress = instStartPCounter + 2 -- "the return address on the stack points 1 byte before the start of the next instruction"
        lowByte  = fromIntegral returnAddress              :: Word8
        highByte = fromIntegral (returnAddress `shiftR` 8) :: Word8


    pushStack mem highByte

    pushStack mem lowByte -- is this the correct order I'm not sure

    --address2 <- jsrRecalcutateAddress (instStartPCounter+1) mem -- I hate this 

    writeRegs pc mem address

-- Return from Subroutine
rts :: Word16 -> Memory -> Bool -> IO ()
rts _ mem _ = do
    lowByte <- pullStack mem
    highByte <- pullStack mem

    let returnAddress = combineTwoBytes lowByte highByte + 1

    writeRegs pc mem returnAddress

-- %%% Interrupts %%%

irqVector :: Word16
irqVector = 0xFFFE

-- IRQ interrupt
irqShared :: Memory -> IO ()
irqShared mem = do
    pCounter <- readRegs pc mem

    let returnAddress = pCounter

        highByte = fromIntegral (returnAddress `shiftR` 8)  :: Word8
        lowByte  = fromIntegral returnAddress               :: Word8

    sr <- readRegs statusReg mem
    let correctedSrVal = sr .|. 0x20 -- ensure unused is set

    pushStack mem highByte
    pushStack mem lowByte
    pushStack mem correctedSrVal

    srWriteInterruptDisable mem True

    lowByte2 <- readMemory mem irqVector
    highByte2 <- readMemory mem (irqVector + 1) --What if it overflows????????????update lowbyte???

    let jumpAddress :: Word16
        jumpAddress = (fromIntegral highByte2 :: Word16) `shiftL` 8 + (fromIntegral lowByte2 :: Word16)

    writeRegs pc mem jumpAddress

irq :: Memory -> IO ()
irq mem = do
    srWriteBreak mem False
    irqShared mem

-- break: Software IRQ
brk :: (Word16 -> Memory -> Bool -> IO ()) -> Word8 -> Memory -> IO ()
brk _ baseCycles mem = do
    modifyIORef' (cycleCount mem) (+ fromIntegral baseCycles)

    pcVal <- readRegs pc mem
    writeRegs pc mem (pcVal + 2)
    srWriteBreak mem True
    irqShared mem

-- Return From Interrupt
rti :: Word16 -> Memory -> Bool -> IO ()
rti _ mem _ = do
    srPushed <- pullStack mem
    lowByte <- pullStack mem
    highByte <- pullStack mem
    let returnAddress = (fromIntegral highByte `shiftL` 8) + fromIntegral lowByte

    -- keep break and unused at their current value
    srCurrent <- readRegs statusReg mem
    let mask = complement 0x30
        correctedSrVal = (srCurrent .&. 0x30) .|. (srPushed .&. mask)

    writeRegs statusReg mem correctedSrVal
    writeRegs pc mem returnAddress

-- %%% Illegal Instructions %%%

-- another nop
ill_0C :: Word16 -> Memory -> Bool -> IO ()
ill_0C _ _ _ = return ()