module Debug where

import MemoryRegisters (Memory (keyboardMatrix), readMemory, CPURegs(pc, x, y, stackP, accumulator, statusReg), kbMatrixCols, kbMatrixRows)
import Utilities (showHexX, showHexF)

import System.IO (hFlush, stdout)
import Data.IORef (readIORef, writeIORef)
import Data.Word (Word8, Word16)
import qualified Data.Map as M
import Control.Monad (forM_)
import Numeric (readHex)
import Data.Char (toUpper)
import Data.Bits (Bits(testBit))
import Data.List (delete)
import qualified Data.Vector as IBVector

showMemoryPage :: Memory -> Word8 -> IO ()
showMemoryPage memory pageNum = do
    let (startAddress :: Word16) = fromIntegral pageNum * 256
        itemSpacing = 3
    putStr $ replicate itemSpacing ' ' ++ "   "
    forM_ ([0 .. 15] :: [Word16]) $ \address -> do
        let hexVal = showHexF address
            padding = replicate (itemSpacing - length hexVal) ' '
        putStr $ hexVal ++ padding
    putStrLn ""
    forM_ [startAddress, startAddress + 16 .. startAddress + 255] $ \yAxisAddress -> do
        let hexVal = showHexF yAxisAddress
        let padding = replicate (itemSpacing - length hexVal) ' '
        putStr $ hexVal ++ padding ++ ": "

        forM_ [yAxisAddress .. yAxisAddress + 15] $ \address -> do
            val <- readMemory memory address
            let strVal = showHexF val
                padding2 = replicate (itemSpacing - length strVal) ' '
            putStr $ strVal ++ padding2
        putStrLn ""

printRegs :: CPURegs -> IO ()
printRegs regs = do
    pc' <- readIORef (pc regs)
    a'  <- readIORef (accumulator regs)
    x'  <- readIORef (x regs)
    y'  <- readIORef (y regs)
    sp' <- readIORef (stackP regs)
    sr' <- readIORef (statusReg regs)
    putStrLn $ "PC=" ++ showHexX pc' ++ " A=" ++ showHexX a' ++ " X=" ++ showHexX x' ++ " Y=" ++ showHexX y' ++ " SP=" ++ showHexX sp' ++ " SR=" ++ showStatusReg sr'

-- showStatusReg :: Word8 -> String
-- showStatusReg w = [if testBit w i then '1' else '0' | i <- [7,6..0]]

showStatusReg :: Word8 -> String
showStatusReg w =
    let flags = "NVBDIZC"
        bits  = [7,6,4,3,2,1,0]  -- map each letter to its bit
        showBit i c = if testBit w i then c else ' '
    in zipWith showBit bits flags


getMnemonic :: Word8 -> String
getMnemonic opcode = M.findWithDefault ("Unknown opcode: " ++ showHexX opcode) opcode opcodeNames

opcodeNames :: M.Map Word8 String
opcodeNames = M.fromList
    [ --Generated from opcode table by makeMnemonicTable.py
    (0x01, "Indirectx ORA"),
    (0x05, "Zeropage ORA"),    (0x06, "Zeropage ASL"),    (0x08, "Implied PHP"),
    (0x09, "Immediate ORA"),    (0x0A, "Useacc ASL"),    (0x0D, "Absolute ORA"),
    (0x0E, "Absolute ASL"),    (0x10, "Relative BPL"),    (0x11, "Indirecty ORA"),
    (0x15, "Zeropagex ORA"),    (0x16, "Zeropagex ASL"),    (0x18, "Implied CLC"),
    (0x19, "Absolutey ORA"),    (0x1D, "Absolutex ORA"),    (0x1E, "Absolutex ASL"),
    (0x20, "Absolute JSR"),    (0x21, "Indirectx AND"),    (0x24, "Zeropage BIT"),
    (0x25, "Zeropage AND"),    (0x26, "Zeropage ROL"),    (0x28, "Implied PLP"),
    (0x29, "Immediate AND"),    (0x2A, "Useacc ROL"),    (0x2C, "Absolute BIT"),
    (0x2D, "Absolute AND"),    (0x2E, "Absolute ROL"),    (0x30, "Relative BMI"),
    (0x31, "Indirecty AND"),    (0x35, "Zeropagex AND"),    (0x36, "Zeropagex ROL"),
    (0x38, "Implied SEC"),    (0x39, "Absolutey AND"),    (0x3D, "Absolutex AND"),
    (0x3E, "Absolutex ROL"),    (0x40, "Implied RTI"),    (0x41, "Indirectx EOR"),
    (0x45, "Zeropage EOR"),    (0x46, "Zeropage LSR"),    (0x48, "Implied PHA"),
    (0x49, "Immediate EOR"),    (0x4A, "Useacc LSR"),    (0x4C, "Absolute JMP"),
    (0x4D, "Absolute EOR"),    (0x4E, "Absolute LSR"),    (0x50, "Relative BVC"),
    (0x51, "Indirecty EOR"),    (0x55, "Zeropagex EOR"),    (0x56, "Zeropagex LSR"),
    (0x58, "Implied CLI"),    (0x59, "Absolutey EOR"),    (0x5D, "Absolutex EOR"),
    (0x5E, "Absolutex LSR"),    (0x60, "Implied RTS"),    (0x61, "Indirectx ADC"),
    (0x65, "Zeropage ADC"),    (0x66, "Zeropage ROR"),    (0x68, "Implied PLA"),
    (0x69, "Immediate ADC"),    (0x6A, "Useacc ROR"),    (0x6C, "Indirect JMP"),
    (0x6D, "Absolute ADC"),    (0x6E, "Absolute ROR"),    (0x70, "Relative BVS"),
    (0x71, "Indirecty ADC"),    (0x75, "Zeropagex ADC"),    (0x76, "Zeropagex ROR"),
    (0x78, "Implied SEI"),    (0x79, "Absolutey ADC"),    (0x7D, "Absolutex ADC"),
    (0x7E, "Absolutex ROR"),    (0xB0, "Relative BCS"),    (0x81, "Indirectx STA"),
    (0x84, "Zeropage STY"),    (0x85, "Zeropage STA"),    (0x86, "Zeropage STX"),
    (0x88, "Implied DEY"),    (0x8A, "Implied TXA"),    (0x8C, "Absolute STY"),
    (0x8D, "Absolute STA"),    (0x8E, "Absolute STX"),    (0x90, "Relative BCC"),
    (0x91, "Indirecty STA"),    (0x94, "Zeropagex STY"),    (0x95, "Zeropagex STA"),
    (0x96, "Zeropagey STX"),    (0x98, "Implied TYA"),    (0x99, "Absolutey STA"),
    (0x9A, "Implied TXS"),    (0x9D, "Absolutex STA"),    (0xA0, "Immediate LDY"),
    (0xA1, "Indirectx LDA"),    (0xA2, "Immediate LDX"),    (0xA4, "Zeropage LDY"),
    (0xA5, "Zeropage LDA"),    (0xA6, "Zeropage LDX"),    (0xA8, "Implied TAY"),
    (0xA9, "Immediate LDA"),    (0xAA, "Implied TAX"),    (0xAC, "Absolute LDY"),
    (0xAD, "Absolute LDA"),    (0xAE, "Absolute LDX"),    (0xB1, "Indirecty LDA"),
    (0xB4, "Zeropagex LDY"),    (0xB5, "Zeropagex LDA"),    (0xB6, "Zeropagey LDX"),
    (0xB8, "Implied CLV"),    (0xB9, "Absolutey LDA"),    (0xBA, "Implied TSX"),
    (0xBC, "Absolutex LDY"),    (0xBD, "Absolutex LDA"),    (0xBE, "Absolutey LDX"),
    (0xC0, "Immediate CPY"),    (0xC1, "Indirectx CMP"),    (0xC4, "Zeropage CPY"),
    (0xC5, "Zeropage CMP"),    (0xC6, "Zeropage DEC"),    (0xC8, "Implied INY"),
    (0xC9, "Immediate CMP"),    (0xCA, "Implied DEX"),    (0xCC, "Absolute CPY"),
    (0xCD, "Absolute CMP"),    (0xCE, "Absolute DEC"),    (0xD0, "Relative BNE"),
    (0xD1, "Indirecty CMP"),    (0xD5, "Zeropagex CMP"),    (0xD6, "Zeropagex DEC"),
    (0xD8, "Implied CLD"),    (0xD9, "Absolutey CMP"),    (0xDD, "Absolutex CMP"),
    (0xDE, "Absolutex DEC"),    (0xE0, "Immediate CPX"),    (0xE1, "Indirectx SBC"),
    (0xE4, "Zeropage CPX"),    (0xE5, "Zeropage SBC"),    (0xE6, "Zeropage INC"),
    (0xE8, "Implied INX"),    (0xE9, "Immediate SBC"),    (0xEA, "Implied NOP"),
    (0xEC, "Absolute CPX"),    (0xED, "Absolute SBC"),    (0xEE, "Absolute INC"),
    (0xF0, "Relative BEQ"),    (0xF1, "Indirecty SBC"),    (0xF5, "Zeropagex SBC"),
    (0xF6, "Zeropagex INC"),    (0xF8, "Implied SED"),    (0xF9, "Absolutey SBC"),
    (0xFD, "Absolutex SBC"),    (0xFE, "Absolutex INC")    ]

debuggerHelpMessage :: String
debuggerHelpMessage = 
    "CONTINUE           (c)  --run emulator\n\
    \STEP n             (s)  --run for n instructions\n\
    \BREAKPOINT n       (b)  --set breakpoint at n\n\
    \BREAKPOINTREMOVE n (br) --remove breakpoint at n\n\
    \BREAKPOINTSHOW n   (bs) --show all breakpoints\n\
    \MEMORY n           (m)  --show memory page containing n\n\
    \REGISTERS          (r)  --show CPU registers\n\
    \PASTEQ             (pq) --simulate Q key pressed\n\
    \HELP               (h)  --show this message"

data DebugState = DebugState { breakpoints :: [Word16],  stepsRemaining :: Int, pause :: Bool, simulatedKeyPress :: Int}

-- processing input is pain
handleInput :: Memory -> CPURegs -> DebugState -> IO DebugState
handleInput mem regs debugState@(DebugState bps _ _ simKP) = do
    (command, value) <- getValidInput -- assumes valid input recieved from this function
    case command of
        "C" -> return debugState { stepsRemaining = -1 }
        "S" -> return debugState { stepsRemaining = fromIntegral value }
        "B" -> handleInput mem regs debugState { breakpoints = fromIntegral value:bps }
        "BR" -> handleInput mem regs debugState { breakpoints = delete (fromIntegral value) bps }
        "BS" -> do
            putStrLn $ unwords $ map showHexX bps
            handleInput mem regs debugState
        "R" -> do
            printRegs regs
            handleInput mem regs debugState
        "M" -> do
            let pageIndex = fromIntegral (value `div` 256)
            showMemoryPage mem pageIndex
            handleInput mem regs debugState
        "PQ" -> do
            print "hi"
            handleInput mem regs debugState { simulatedKeyPress = 1 }
            -- if simKP == 0
            --     then handleInput mem regs debugState { simulatedKeyPress = 1 } -- fix
            --     else handleInput mem regs debugState { simulatedKeyPress = 2 }
        "H" -> putStrLn debuggerHelpMessage >> handleInput mem regs debugState
        _ -> do
            putStrLn "Invalid command, this should not be reachable should have been handled by getValidInput"
            return debugState


getValidInput :: IO (String, Int)
getValidInput = do
    putStr ">"
    hFlush stdout

    rawInput <- words <$> getLine

    (command, value :: Int, numErr) <- case rawInput of
        [] -> return ("", 0, True)
        [cmd] -> return (map toUpper cmd, 2, True)
        (cmd:val:_) ->
            case parseHex val of
                Just n  -> return (map toUpper cmd, n, False)
                Nothing -> return (map toUpper cmd, 2, True)
            where
                parseHex :: String -> Maybe Int
                parseHex s = case readHex s of
                    [(n, "")] | n >= 0 -> Just n
                    _ -> Nothing

    let commandShort = case command of
            "CONTINUE"          -> "C"
            "STEP"              -> "S"
            "BREAKPOINT"        -> "B"
            "BREAKPOINTREMOVE"  -> "BR"
            "BREAKPOINTSHOW"    -> "BS"
            "MEMORY"            -> "M"
            "REGISTERS"         -> "R"
            "PASTEQ"            -> "PQ"
            "HELP"              -> "H"
            _                   -> command

    case commandShort of
        "C" -> return (commandShort, 0)
        "R" -> return (commandShort, 0)
        
        "S" ->
            if value == 0 then
                putStrLn "Cannot step 0" >> getValidInput
            else if numErr then
                case rawInput of
                    [_] -> return (commandShort, 1)
                    _   -> putStrLn "Not a valid number" >> getValidInput
            else
                return (commandShort, read (rawInput !! 1)) -- messy but works, we need decimal value and know that input is already checked valid
                
        "B" ->
            if numErr then
                putStrLn "Not a valid number" >> getValidInput
            else if value > 0xFFFF then
                putStrLn "Value out of range" >> getValidInput
            else
                return (commandShort, value)
        "BR" ->
            if numErr then
                putStrLn "Not a valid number" >> getValidInput
            else if value > 0xFFFF then
                putStrLn "Value out of range" >> getValidInput
            else
                return (commandShort, value)
        "BS" -> return (commandShort, 0)
        "M" ->
            if numErr then
                putStrLn "Not a valid number" >> getValidInput
            else if value > 0xFFFF then
                putStrLn "Value out of range" >> getValidInput
            else
                return (commandShort, value)
        "PQ" -> return (commandShort, value)
        "H" -> return (commandShort, 0)
        _ ->
            putStrLn "Invalid command, try again." >> getValidInput

-- printing nice looking ouput is pain
debuggerLineOutput :: Word16 -> Word8 -> Word8 -> Word8 -> String
debuggerLineOutput pcVal opcode operand1 operand2 = do


    let instructionName = getMnemonic opcode
        (addressingMode, _) = case words instructionName of
                                            [a, i] -> (a, i)
                                            _      -> error instructionName

        str_pcAop = showHexF pcVal ++ ": " ++ showHexF opcode ++ " "
        operandCount :: Int = case addressingMode of
                                "Immediate"   -> 1
                                "Zeropage"    -> 1
                                "Zeropagex"   -> 1
                                "ZeropageY"   -> 1
                                "Absolute"    -> 2
                                "Absolutex"   -> 2
                                "Absolutey"   -> 2
                                "Indirectx"   -> 1
                                "Indirecty"   -> 1
                                "Indirect"    -> 2
                                "Implied"     -> 0
                                "Relative"    -> 1
                                "Useacc"      -> 0
                                _             -> -1
        str_operands = case operandCount of
                            0 ->  "       "
                            1 ->  showHexF operand1 ++ "     "
                            2 ->  showHexF operand1 ++ " " ++ showHexF operand2 ++ "  "
                            _ ->  "error. "
        -- case addressingMode of
        --     "Immediate" -> putStr $ showHexF operand1 ++ "       " ++ instructionName ++ " #" ++ showHexF operand1 ++ "     >"
        --     "Absolute"  -> putStr $ showHexF operand1 ++ " " ++ showHexF operand2 ++ "    " ++ instructionName ++ "   " ++ showHexF operand2 ++ showHexF operand1 ++ "   >"
        --     "Absolutex" -> putStr "idk"
        --     _           -> putStr "nothing"
        itemSpacing = 15
        padding = replicate (itemSpacing - length instructionName) ' '
        str_instrName = instructionName ++ padding

    str_pcAop ++ str_operands ++ str_instrName
    
manageSimulatedKeyPress :: Memory -> IO ()
manageSimulatedKeyPress mem = do
    let idx = 1 * kbMatrixCols + 0
        newMatrix =
            IBVector.replicate (kbMatrixRows * kbMatrixCols) False
            IBVector.// [(idx, True)]

    writeIORef (keyboardMatrix mem) newMatrix


-- updateKeyboardMatrix :: Memory -> IO ()
-- updateKeyboardMatrix mem = do
--     keyStates <- SDL.getKeyboardState

--     let updateKey :: IBVector.Vector Bool -> (SDL.Scancode, (Int, Int)) -> IBVector.Vector Bool
--         updateKey km (sc, (row, col)) =
--             let idx = row * kbMatrixCols + col
--             in km IBVector.// [(idx, keyStates sc)]

--         newMatrix = foldl updateKey (IBVector.replicate (kbMatrixRows * kbMatrixCols) False) keyMapping

--     writeIORef (keyboardMatrix mem) newMatrix
