module Debug where

import Memory (Memory, readMemory)
import Data.Word (Word8, Word16)
import qualified Data.Map as M
import Control.Monad (forM_)
import Numeric (showHex, readHex)
--import Text.Read (readMaybe)
import Data.Char (toUpper)

showMemoryPage :: Memory -> Word8 -> IO ()
showMemoryPage memory pageNum = do
    let (startAddress :: Word16) = fromIntegral pageNum * 256
        itemSpacing = 5
    putStr $ replicate itemSpacing ' '
    forM_ ([0 .. 15] :: [Word16]) $ \address -> do
        let hexVal = showHex address ""
            padding = replicate (itemSpacing - length hexVal) ' '
        putStr $ hexVal ++ padding
    putStrLn ""
    forM_ [startAddress, startAddress + 16 .. startAddress + 255] $ \yAxisAddress -> do
        let hexVal = showHex yAxisAddress ""
        let padding = replicate (itemSpacing - length hexVal) ' '
        putStr $ hexVal ++ padding

        forM_ [yAxisAddress .. yAxisAddress + 15] $ \address -> do
            val <- readMemory memory address
            let strVal = show val
                padding2 = replicate (itemSpacing - length strVal) ' '
            putStr $ strVal ++ padding2
        putStrLn ""

debugShowMemory16 :: Memory -> Word16 -> IO ()
debugShowMemory16 memory startAddress = do
    let itemSpacing = 5
    forM_ [startAddress .. startAddress + 16] $ \address -> do
        let hexVal = showHex address ""
            padding = replicate (itemSpacing - length hexVal) ' '
        putStr $ hexVal ++ padding
    putStrLn ""
    forM_ [startAddress .. startAddress + 16] $ \address -> do
        val <- readMemory memory address
        let strVal = show val
            padding = replicate (itemSpacing - length strVal) ' '
        putStr $ strVal ++ padding
    putStrLn ""

getMnemonic :: Word8 -> String
getMnemonic opcode = M.findWithDefault ("Unknown opcode: " ++ showHex opcode "") opcode opcodeNames

opcodeNames :: M.Map Word8 String
opcodeNames = M.fromList
    [ --Generated from opcode table
    (0x05, "Zeropage ORA"),
    (0x0D, "Absolute ORA"),    (0x09, "Immediate ORA"),    (0x10, "Relative BPL"),
    (0x11, "Indirecty ORA"),    (0x15, "Zeropagex ORA"),    (0x18, "Implied CLC"),
    (0x19, "Absolutex ORA"),    (0x1D, "Absolutex ORA"),    (0x21, "Indirectx AND"),
    (0x25, "Zeropage AND"),    (0x26, "Zeropage ROL"),    (0x29, "Immediate AND"),
    (0x2A, "Implied ROL"),    (0x2D, "Absolute AND"),    (0x2E, "Absolute ROL"),
    (0x30, "Relative BMI"),    (0x31, "Indirecty AND"),    (0x35, "Zeropagex AND"),
    (0x36, "Zeropagex ROL"),    (0x38, "Implied SEC"),    (0x39, "Absolutey AND"),
    (0x3D, "Absolutex AND"),    (0xE3, "Absolutex ROL"),    (0x41, "Indirectx EOR"),
    (0x45, "Zeropage EOR"),    (0x49, "Immediate EOR"),    (0x4C, "Absolute JMP"),
    (0x4D, "Absolute EOR"),    (0x50, "Relative BVC"),    (0x51, "Indirecty EOR"),
    (0x55, "Zeropagex EOR"),    (0x58, "Implied CLI"),    (0x59, "Absolutex EOR"),
    (0x5D, "Absolutex EOR"),    (0x61, "Indirectx ADC"),    (0x65, "Zeropage ADC"),
    (0x66, "Zeropage ROR"),    (0x69, "Immediate ADC"),    (0x6A, "Implied ROR"),
    (0x6C, "Indirect JMP"),    (0x6D, "Absolute ADC"),    (0x6E, "Absolute ROR"),
    (0x70, "Relative BVS"),    (0x71, "Indirecty ADC"),    (0x75, "Zeropagex ADC"),
    (0x76, "Zeropagex ROR"),    (0x78, "Implied SEI"),    (0x79, "Absolutey ADC"),
    (0x7D, "Absolutex ADC"),    (0x7E, "Absolutex ROR"),    (0xB0, "Relative BCS"),
    (0x81, "Indirectx STA"),    (0x84, "Zeropage STY"),    (0x85, "Zeropage STA"),
    (0x86, "Zeropage STX"),    (0x88, "Implied DEY"),    (0x8A, "Implied TXA"),
    (0x8C, "Absolute STY"),    (0x8D, "Absolute STA"),    (0x8E, "Absolute STX"),
    (0x90, "Relative BCC"),    (0x91, "Indirecty STA"),    (0x94, "Zeropagex STY"),
    (0x95, "Zeropagex STA"),    (0x96, "Zeropagey STX"),    (0x98, "Implied TYA"),
    (0x99, "Absolutey STA"),    (0x9A, "Implied TXS"),    (0x9D, "Absolutex STA"),
    (0xA0, "Immediate LDY"),    (0xA1, "Indirectx LDA"),    (0xA2, "Immediate LDX"),
    (0xA4, "Zeropage LDY"),    (0xA5, "Zeropage LDA"),    (0xA6, "Zeropage LDX"),
    (0xA8, "Implied TAY"),    (0xA9, "Immediate LDA"),    (0xAA, "Implied TAX"),
    (0xAC, "Absolute LDY"),    (0xAD, "Absolute LDA"),    (0xAE, "Absolute LDX"),
    (0xB1, "Indirecty LDA"),    (0xB4, "Zeropagex LDY"),    (0xB5, "Zeropagex LDA"),
    (0xB6, "Zeropagey LDX"),    (0xB8, "Implied CLV"),    (0xB9, "Absolutey LDA"),
    (0xBA, "Implied TSX"),    (0xBC, "Absolutex LDY"),    (0xBD, "Absolutex LDA"),
    (0xBE, "Absolutey LDX"),    (0xC0, "Immediate CPY"),    (0xC1, "Indirectx CMP"),
    (0xC4, "Zeropage CPY"),    (0xC5, "Zeropage CMP"),    (0xC6, "Zeropage DEC"),
    (0xC8, "Implied INY"),    (0xC9, "Immediate CMP"),    (0xCA, "Implied DEX"),
    (0xCC, "Absolute CPY"),    (0xCD, "Absolute CMP"),    (0xCE, "Absolute DEC"),
    (0xD0, "Relative BNE"),    (0xD1, "Indirecty CMP"),    (0xD5, "Zeropagex CMP"),
    (0xD6, "Zeropagex DEC"),    (0xD8, "Implied CLD"),    (0xD9, "Absolutex CMP"),
    (0xDD, "Absolutex CMP"),    (0xDE, "Absolutex DEC"),    (0xE0, "Immediate CPX"),
    (0xE1, "Indirectx SBC"),    (0xE4, "Zeropage CPX"),    (0xE5, "Zeropage SBC"),
    (0xE6, "Zeropage INC"),    (0xE8, "Implied INX"),    (0xE9, "Immediate SBC"),
    (0xEA, "Implied NOP"),    (0xEC, "Absolute CPX"),    (0xED, "Absolute SBC"),
    (0xEE, "Absolute INC"),    (0xF0, "Relative BEQ"),    (0xF1, "Indirecty SBC"),
    (0xF5, "Zeropagex SBC"),    (0xF6, "Zeropagex INC"),    (0xF8, "Implied SED"),
    (0xF9, "Absolutey SBC"),    (0xFD, "Absolutex SBC"),    (0xFE, "Absolutex INC")
    ]

-- C, Continue
-- S, Step
-- B, Breakpoint
-- M, show memory page

data DebugState = DebugState { breakpoints :: [Word16], continueMode :: Bool }

-- processing input is pain
handleInput :: Memory -> DebugState -> IO DebugState
handleInput mem debugState@(DebugState bps continueMode) = do
    (command, value) <- getValidInput
    case command of
        "C" -> return (DebugState bps True)
        "S" -> return (DebugState bps False)
        "B" -> handleInput mem (DebugState (value:bps) continueMode)
        "M" -> do
            let pageIndex = fromIntegral (value `div` 256)
            showMemoryPage mem pageIndex
            handleInput mem  debugState
        _ -> do
            putStrLn "Invalid command, this should not be reachable should have been handled by getValidInput"
            return debugState


getValidInput :: IO (String, Word16)
getValidInput = do
    putStrLn "Enter command: "
    input <- words <$> getLine

    (command, value :: Word16, numErr) <- case input of
        [] -> return ("", 0, True)
        [cmd] -> return (map toUpper cmd, 0, True)
        (cmd:val:_) ->
            -- case readMaybe val :: Maybe Int of
            --     Just n | n >= 0 && n <= 65535 -> return (map toUpper cmd, fromIntegral n, False)
            --     _ -> return (map toUpper cmd, 0, True)

            case parseHex val of
                Just n  -> return (map toUpper cmd, n, False)
                Nothing -> return (map toUpper cmd, 0, True)
            where
                parseHex :: String -> Maybe Word16
                parseHex s = case readHex s of
                    [(n, "")] | n >= 0 && n <= 0xFFFF -> Just (fromIntegral n)
                    _ -> Nothing

    let commandShort = case command of
            "CONTINUE"   -> "C"
            "STEP"       -> "S"
            "BREAKPOINT" -> "B"
            "MEMORY"     -> "M"
            _            -> command

    case commandShort of
        "C" -> return (commandShort, 0)
        "S" -> return (commandShort, 0)
        "B" ->
            if numErr
                then do
                    putStrLn "Not a valid number"
                    getValidInput
                else return (commandShort, value)
        "M" -> do
            if numErr
                then do
                    putStrLn "Not a valid number"
                    getValidInput
                else return (commandShort, value)
        _ -> do
            putStrLn "Invalid command, try again."
            getValidInput
