module CPU6502 where

import MemoryRegisters (initMemory, readMemory, writeMemory, Memory, CPURegs(pc, x, y, stackP, accumulator, statusReg), initRegisters) 
import LoadRom (loadRom)
import Debug ( DebugState(..), handleInput, debuggerLineOutput, manageSimulatedKeyPress)

import Data.Word (Word16, Word8)
import Data.Int (Int8)
import Data.Bits ( Bits((.|.), testBit, complement, (.&.), shiftL, shiftR, xor))
import Data.IORef (IORef, readIORef, modifyIORef', writeIORef)
import Control.Monad (unless, when)
import qualified Data.Vector as IBVector
import Utilities (showHexX)

cpuInit :: IO (Memory, CPURegs)
cpuInit = do
    mem <- initMemory 0xFF
    regs <- initRegisters 0 0 0 0 0xFF 0x20

    -- This is the 'machine operating system' in the upper quarter of address space
    loadRom "roms/os12_bemdump.rom" 0xC000 mem
    -- Load the correct basic rom to the sideways rom area
    loadRom "roms/basic2.rom" 0x8000 mem

    initialPC <- getInitialPC mem
    writeIORef (pc regs) initialPC
    srWriteInterruptDisable (statusReg regs) True

    return (mem, regs)

getInitialPC :: Memory -> IO Word16
getInitialPC mem = do
    let resetVector :: Word16
        resetVector = 0xFFFC
    low <- readMemory mem resetVector
    high <- readMemory mem (resetVector+1)

    return $ combineTwoBytes low high


runInstructions :: Memory -> CPURegs -> Int -> IO ()
runInstructions mem regs count = loop 0 --replicateM might be cleaner here
    where
        loop n
            | n >= count = return ()
            | otherwise = do
                pcVal <- readIORef (pc regs)
                currentInstructionOpcode <- readMemory mem pcVal
                let instr = opcodeTable IBVector.! fromIntegral currentInstructionOpcode

                instr mem regs
                loop (n+1)

debuggerStart :: Memory -> CPURegs -> IO ()
debuggerStart mem regs = do
    pcVal <- readIORef (pc regs)
    operand1 <- readMemory mem (pcVal+1)
    operand2 <- readMemory mem (pcVal+2)
    currentInstructionOpcode <- readMemory mem pcVal

    putStr $ debuggerLineOutput pcVal currentInstructionOpcode operand1 operand2


runInstructionsDebug :: Memory -> CPURegs -> Int -> DebugState -> IO DebugState
runInstructionsDebug mem regs count = loop 0 --dbs
    where
        loop :: Int -> DebugState -> IO DebugState
        loop n debugState@(DebugState _ _ pause _)
            | n >= count = return debugState
            | otherwise = do

                -- get user input
                newDebugState <- if pause
                    then do 
                        handleInput mem regs debugState

                    else return debugState

                let simKP = simulatedKeyPress newDebugState

                --print simKP

                when (simKP == 2) $ manageSimulatedKeyPress mem
                when (simKP == 1) $ do 
                    manageSimulatedKeyPress mem-- >> irq mem regs
                    putStrLn "Hi i did IRQ"

                -- run current instruction
                pcVal <- readIORef (pc regs)
                currentInstructionOpcode <- readMemory mem pcVal
                let instr = opcodeTable IBVector.! fromIntegral currentInstructionOpcode
                instr mem regs

                -- next instruction
                nextPcVal <- readIORef (pc regs)
                operand1 <- readMemory mem (nextPcVal+1)
                operand2 <- readMemory mem (nextPcVal+2)
                nextInstructionOpcode <- readMemory mem nextPcVal
                
                -- decide if need to stop
                let bps      = breakpoints newDebugState
                    stepsRem = stepsRemaining newDebugState
                    newStepsRem = if stepsRem == -1 -- treat -1 as meaning continueMode -- if stepsRem == 0 this will be caught earlier
                                            then stepsRem
                                            else stepsRem -1

                --handle simuated keypress (I hate this)
                    newSimKP = if simKP == 0
                        then 0
                        else 2

                if newStepsRem == 0 || (nextPcVal `elem` bps) then do
                    -- show instruction details
                    putStr $ debuggerLineOutput nextPcVal nextInstructionOpcode operand1 operand2
                    return newDebugState { pause = True, stepsRemaining = newStepsRem, simulatedKeyPress = newSimKP } -- yield to mainLoop
                else loop (n+1) newDebugState { pause = False, stepsRemaining = newStepsRem, simulatedKeyPress = newSimKP }



-- __________Memory__________
-- Memory woz ere

-- __________Registers__________
-- Registers woz ere

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

srWriteCarry            :: IORef Word8 -> Bool -> IO (); srWriteCarry            = srWriteBit 0x01
srWriteZero             :: IORef Word8 -> Bool -> IO (); srWriteZero             = srWriteBit 0x02
srWriteInterruptDisable :: IORef Word8 -> Bool -> IO (); srWriteInterruptDisable = srWriteBit 0x04
srWriteDecimalMode      :: IORef Word8 -> Bool -> IO (); srWriteDecimalMode      = srWriteBit 0x08
srWriteBreak            :: IORef Word8 -> Bool -> IO (); srWriteBreak            = srWriteBit 0x10
srWriteUnused           :: IORef Word8 -> Bool -> IO (); srWriteUnused           = srWriteBit 0x20
srWriteOverflow         :: IORef Word8 -> Bool -> IO (); srWriteOverflow         = srWriteBit 0x40
srWriteNegative         :: IORef Word8 -> Bool -> IO (); srWriteNegative         = srWriteBit 0x80

-- __________Create Opcode Table__________
opcodeTable :: IBVector.Vector (Memory -> CPURegs -> IO ())
opcodeTable = IBVector.generate 256 assign
    where
        assign :: Int -> (Memory -> CPURegs -> IO ())
        assign 0x00 = brk undefined
        assign 0x01 = indirectX ora
        assign 0x05 = zeropage ora
        assign 0x06 = zeropage asl
        assign 0x08 = implied php
        assign 0x09 = immediate ora
        assign 0x0A = useAcc asl
        assign 0x0D = absolute ora
        assign 0x0E = absolute asl
        assign 0x10 = relative bpl
        assign 0x11 = indirectY ora
        assign 0x15 = zeropageX ora
        assign 0x16 = zeropageX asl
        assign 0x18 = implied clc
        assign 0x19 = absoluteY ora
        assign 0x1D = absoluteX ora
        assign 0x1E = absoluteX asl
        assign 0x20 = absolute jsr
        assign 0x21 = indirectX and_
        assign 0x24 = zeropage bit
        assign 0x25 = zeropage and_
        assign 0x26 = zeropage rol
        assign 0x28 = implied plp
        assign 0x29 = immediate and_
        assign 0x2A = useAcc rol
        assign 0x2C = absolute bit
        assign 0x2D = absolute and_
        assign 0x2E = absolute rol
        assign 0x30 = relative bmi
        assign 0x31 = indirectY and_
        assign 0x35 = zeropageX and_
        assign 0x36 = zeropageX rol
        assign 0x38 = implied sec
        assign 0x39 = absoluteY and_
        assign 0x3D = absoluteX and_
        assign 0x3E = absoluteX rol
        assign 0x40 = implied rti
        assign 0x41 = indirectX eor
        assign 0x45 = zeropage eor
        assign 0x46 = zeropage lsr
        assign 0x48 = implied pha
        assign 0x49 = immediate eor
        assign 0x4A = useAcc lsr
        assign 0x4C = absolute jmp
        assign 0x4D = absolute eor
        assign 0x4E = absolute lsr
        assign 0x50 = relative bvc
        assign 0x51 = indirectY eor
        assign 0x55 = zeropageX eor
        assign 0x56 = zeropageX lsr
        assign 0x58 = implied cli
        assign 0x59 = absoluteY eor
        assign 0x5D = absoluteX eor
        assign 0x5E = absoluteX lsr
        assign 0x60 = implied rts
        assign 0x61 = indirectX adc
        assign 0x65 = zeropage adc
        assign 0x66 = zeropage ror
        assign 0x68 = implied pla
        assign 0x69 = immediate adc
        assign 0x6A = useAcc ror
        assign 0x6C = indirect jmp
        assign 0x6D = absolute adc
        assign 0x6E = absolute ror
        assign 0x70 = relative bvs
        assign 0x71 = indirectY adc
        assign 0x75 = zeropageX adc
        assign 0x76 = zeropageX ror
        assign 0x78 = implied sei
        assign 0x79 = absoluteY adc
        assign 0x7D = absoluteX adc
        assign 0x7E = absoluteX ror
        assign 0xB0 = relative bcs
        assign 0x81 = indirectX sta
        assign 0x84 = zeropage sty
        assign 0x85 = zeropage sta
        assign 0x86 = zeropage stx
        assign 0x88 = implied dey
        assign 0x8A = implied txa
        assign 0x8C = absolute sty
        assign 0x8D = absolute sta
        assign 0x8E = absolute stx
        assign 0x90 = relative bcc
        assign 0x91 = indirectY sta
        assign 0x94 = zeropageX sty
        assign 0x95 = zeropageX sta
        assign 0x96 = zeropageY stx
        assign 0x98 = implied tya
        assign 0x99 = absoluteY sta
        assign 0x9A = implied txs
        assign 0x9D = absoluteX sta
        assign 0xA0 = immediate ldy
        assign 0xA1 = indirectX lda
        assign 0xA2 = immediate ldx
        assign 0xA4 = zeropage ldy
        assign 0xA5 = zeropage lda
        assign 0xA6 = zeropage ldx
        assign 0xA8 = implied tay
        assign 0xA9 = immediate lda
        assign 0xAA = implied tax
        assign 0xAC = absolute ldy
        assign 0xAD = absolute lda
        assign 0xAE = absolute ldx
        assign 0xB1 = indirectY lda
        assign 0xB4 = zeropageX ldy
        assign 0xB5 = zeropageX lda
        assign 0xB6 = zeropageY ldx
        assign 0xB8 = implied clv
        assign 0xB9 = absoluteY lda
        assign 0xBA = implied tsx
        assign 0xBC = absoluteX ldy
        assign 0xBD = absoluteX lda
        assign 0xBE = absoluteY ldx
        assign 0xC0 = immediate cpy
        assign 0xC1 = indirectX cmp
        assign 0xC4 = zeropage cpy
        assign 0xC5 = zeropage cmp
        assign 0xC6 = zeropage dec
        assign 0xC8 = implied iny
        assign 0xC9 = immediate cmp
        assign 0xCA = implied dex
        assign 0xCC = absolute cpy
        assign 0xCD = absolute cmp
        assign 0xCE = absolute dec
        assign 0xD0 = relative bne
        assign 0xD1 = indirectY cmp
        assign 0xD5 = zeropageX cmp
        assign 0xD6 = zeropageX dec
        assign 0xD8 = implied cld
        assign 0xD9 = absoluteY cmp
        assign 0xDD = absoluteX cmp
        assign 0xDE = absoluteX dec
        assign 0xE0 = immediate cpx
        assign 0xE1 = indirectX sbc
        assign 0xE4 = zeropage cpx
        assign 0xE5 = zeropage sbc
        assign 0xE6 = zeropage inc
        assign 0xE8 = implied inx
        assign 0xE9 = immediate sbc
        assign 0xEA = implied nop
        assign 0xEC = absolute cpx
        assign 0xED = absolute sbc
        assign 0xEE = absolute inc
        assign 0xF0 = relative beq
        assign 0xF1 = indirectY sbc
        assign 0xF5 = zeropageX sbc
        assign 0xF6 = zeropageX inc
        assign 0xF8 = implied sed
        assign 0xF9 = absoluteY sbc
        assign 0xFD = absoluteX sbc
        assign 0xFE = absoluteX inc
        assign _    = instrUnimplemented

        -- Read Two Bytes helper function ???

instrUnimplemented :: Memory -> CPURegs -> IO ()
instrUnimplemented _ _ = putStrLn "unimplementedFunction"

instrTest1 :: Memory -> CPURegs -> IO ()
instrTest1 _ _ = putStrLn "Test1"
execute0 :: Memory -> CPURegs -> IO ()
execute0 _ _ = putStrLn "Execute0"

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

-- zeropage,Y       like zeropage X but with Y (rarely used by instructions)
zeropageY :: (Word16 -> Memory -> CPURegs -> Bool -> IO ()) -> Memory -> CPURegs -> IO ()
zeropageY instr mem regs = do
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

    x' <- readIORef (x regs)
    let pointer = operand + x' -- will wrap around automatically

    byte1 <- readMemory mem (fromIntegral pointer)
    byte2 <- readMemory mem (fromIntegral ((pointer + 1) :: Word8))

    let address = combineTwoBytes byte1 byte2

    writeIORef (pc regs) (pcVal + 2)
    instr address mem regs False

-- (indirect),Y     read two bytes at zero-page operand and add Y, Use that as address
indirectY :: (Word16 -> Memory -> CPURegs -> Bool -> IO ()) -> Memory -> CPURegs -> IO ()
indirectY instr mem regs = do
    pcVal <- readIORef (pc regs)
    operand <- readMemory mem (pcVal + 1)
    byte1 <- readMemory mem (fromIntegral operand)
    byte2 <- readMemory mem (fromIntegral (operand + 1))

    y' <- readIORef (y regs)
    let address = combineTwoBytes byte1 byte2 + fromIntegral y'

    writeIORef (pc regs) (pcVal + 2)
    instr address mem regs False


-- (indirect)       only used by JMP
indirect :: (Word16 -> Memory -> CPURegs -> Bool -> IO ()) -> Memory -> CPURegs -> IO ()
indirect instr mem regs = do
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
useAcc :: (Word16 -> Memory -> CPURegs -> Bool -> IO ()) -> Memory -> CPURegs -> IO ()
useAcc instr mem regs = do
    pcVal <- readIORef (pc regs)
    writeIORef (pc regs) (pcVal + 1)
    instr 0 mem regs True

-- implied      No operands because memory is not used (actually it seems it is)
implied :: (Word16 -> Memory -> CPURegs -> Bool -> IO ()) -> Memory -> CPURegs -> IO ()
implied instr mem regs = do
    pcVal <- readIORef (pc regs)
    writeIORef (pc regs) (pcVal + 1)
    instr 0 mem regs False

-- relative     Used by branch instructions
relative :: (Word16 -> Memory -> CPURegs -> Bool -> IO ()) -> Memory -> CPURegs -> IO ()
relative instr mem regs = do
    pcVal <- readIORef (pc regs)
    operand <- readMemory mem (pcVal + 1)
    let offset = fromIntegral (fromIntegral operand :: Int8) :: Int -- keeps the sign bit (hopefully)
        branchTargetInt = fromIntegral pcVal + 2 + offset
        (branchTarget :: Word16) = fromIntegral branchTargetInt

    writeIORef (pc regs) (pcVal + 2) -- in case we dont branch

    instr branchTarget undefined regs False

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
    srWriteOverflow (statusReg regs) (((acc `xor` binaryVal) .&. (inputVal `xor` binaryVal) .&. 0x80) /= 0)

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

-- Branch if Carry Clear
bcc :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
bcc branchTarget _ regs _ = do
    carry' <- srReadCarry (statusReg regs)
    unless carry' (writeIORef (pc regs) branchTarget)

-- Branch if Carry Set
bcs :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
bcs branchTarget _ regs _ = do
    carry' <- srReadCarry (statusReg regs)
    when carry' (writeIORef (pc regs) branchTarget)

-- Branch if Equal (Zero Set)
beq :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
beq branchTarget _ regs _ = do
    zero' <- srReadZero (statusReg regs)
    when zero' (writeIORef (pc regs) branchTarget)

-- Branch if Not Equal (Zero clear)
bne :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
bne branchTarget _ regs _ = do
    zero' <- srReadZero (statusReg regs)
    unless zero' (writeIORef (pc regs) branchTarget)

-- Branch if Minus (Negative Set)
bmi :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
bmi branchTarget _ regs _ = do
    negative' <- srReadNegative (statusReg regs)
    when negative' (writeIORef (pc regs) branchTarget)

-- Branch if Positive (Negative clear)
bpl :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
bpl branchTarget _ regs _ = do
    negative' <- srReadNegative (statusReg regs)
    unless negative' (writeIORef (pc regs) branchTarget)

--- Branch if Overflow Clear
bvc :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
bvc branchTarget _ regs _ = do
    overflow' <- srReadOverflow (statusReg regs)
    unless overflow' (writeIORef (pc regs) branchTarget)

-- Branch if Overflow Set
bvs :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
bvs branchTarget _ regs _ = do
    overflow' <- srReadOverflow (statusReg regs)
    when overflow' (writeIORef (pc regs) branchTarget)

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
    print "Hi I from IRQ"
    pCounter <- readIORef (pc regs)

    let returnAddress = pCounter

        highByte = fromIntegral (returnAddress `shiftR` 8)  :: Word8
        lowByte  = fromIntegral returnAddress               :: Word8
    
    sr <- readIORef (statusReg regs)
    let correctedSrVal = sr .|. 0x30 -- ensure break and unused are set

    pushStack mem regs highByte
    pushStack mem regs lowByte
    pushStack mem regs correctedSrVal

    srWriteInterruptDisable (statusReg regs) True

    lowByte2 <- readMemory mem irqVector
    highByte2 <- readMemory mem (irqVector + 1)

    let jumpAddress :: Word16
        jumpAddress = (fromIntegral highByte2 :: Word16) `shiftL` 8 + (fromIntegral lowByte2 :: Word16)

    putStrLn $ "I jump too: " ++ showHexX jumpAddress

    writeIORef (pc regs) jumpAddress

irq :: Memory -> CPURegs -> IO ()
irq mem regs = do
    irqShared mem regs
    srWriteBreak (statusReg regs) True

-- break: Software IRQ
brk :: (Word16 -> Memory -> CPURegs -> Bool -> IO ()) -> Memory -> CPURegs -> IO ()
brk _ mem regs = do

    pcVal <- readIORef (pc regs)
    writeIORef (pc regs) (pcVal + 2)
    irqShared mem regs
    srWriteBreak (statusReg regs) True

-- Return From Interrupt
rti :: Word16 -> Memory -> CPURegs -> Bool -> IO ()
rti _ mem regs _ = do
    sr <- pullStack mem regs
    lowByte <- pullStack mem regs
    highByte <- pullStack mem regs
    let returnAddress = fromIntegral (highByte `shiftL` 8) + fromIntegral lowByte + 1

    writeIORef (statusReg regs) sr
    writeIORef (pc regs) returnAddress