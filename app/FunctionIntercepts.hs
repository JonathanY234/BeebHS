module FunctionIntercepts where

import MemoryRegisters (Memory, CPURegs(pc, accumulator, statusReg, x, y), readMemory, fileTable, FileMode(Input), OpenFile(readWritePosition))
import DiskFileHandling (getFileData, fileNameToFileEntry, getExecAddr, loadFile, changeExecAddr, readSlot, addOpenFile, FileEntry(fileIndex), getFileEntryByFileIndex, getFileByte)
import CPUInstructions (rts, srWriteCarry)

import Data.IORef (readIORef, writeIORef)
import Data.Bits (Bits(shiftL))
import Data.Word (Word16, Word8)

checkForIntercept :: Memory -> CPURegs -> IO Bool
checkForIntercept mem regs = do
    pcVal <- readIORef (pc regs)
    case pcVal of
        --0xE23E -> starSaveload mem regs >> return True
        --0xF68D -> closeSpoolOrExecFile mem regs >> return False
        0xFFDD -> osFile mem regs >> return True
        0xFFCE -> osFind mem regs >> return True
        0xFFD7 -> osBget mem regs >> return False
        0xE031 -> passToCurrentFilingSystem mem regs >> return True
        _      -> return False

-- DFS function OSFILE
osFile :: Memory -> CPURegs -> IO ()
osFile mem regs = do
    putStr "OSFile "

    accVal <- readIORef (accumulator regs)
    case accVal of
        0 -> putStrLn "0, not implemented"
        1 -> putStrLn "1, not implemented"
        2 -> putStrLn "2, not implemented"
        3 -> putStrLn "3, not implemented"
        4 -> putStrLn "4, not implemented"
        5 -> putStrLn "5, not implemented"
        6 -> putStrLn "6, not implemented"
        0xFF -> do
            putStrLn "FF, Load Specified File"

            paramBlock <- getParamBlockXY regs
            filename <- getFileNameFromParamBlock mem paramBlock

            -- Get load address from param block
            loadAddrBytes <- mapM (readMemory mem) [paramBlock + 2, paramBlock + 3]
            let loadAddr = bytesToWord16 loadAddrBytes 

            loadIndicator <- readMemory mem (paramBlock + 6)

            osFileLoadFileHelper mem filename loadIndicator loadAddr

            -- A, P are destroyed; X,Y unchanged
            --writeIORef (accumulator regs) 0x00  -- or leave undefined
            rtsC mem regs
            

        n -> putStrLn $ "Unknown A val: " ++ show n

--OSFILE HELPERS
getParamBlockXY :: CPURegs -> IO Word16
getParamBlockXY regs = do
    xVal <- readIORef (x regs)
    yVal <- readIORef (y regs)
    return $ fromIntegral yVal `shiftL` 8 + fromIntegral xVal
getFileNameFromParamBlock :: Memory -> Word16 -> IO String
getFileNameFromParamBlock mem paramBlock = do
    filenameAddrL <- readMemory mem paramBlock
    filenameAddrH <- readMemory mem (paramBlock + 1)
    let filenameAddr = (fromIntegral filenameAddrH `shiftL` 8) + fromIntegral filenameAddrL
    readStringFromMemory mem filenameAddr
bytesToWord16 :: [Word8] -> Word16
bytesToWord16 [b0,b1] = fromIntegral b0 + (fromIntegral b1 `shiftL` 8)
bytesToWord16 _ = error "bytesToWord16: expected 2 bytes"

osFileLoadFileHelper :: Memory -> String -> Word8 -> Word16 -> IO ()
osFileLoadFileHelper mem filename loadIndicator possibleLoadAddr = do
    let (bytes, files) = getFileData

    case fileNameToFileEntry filename files of
        Just fileEntry -> do
            let newFileEntry = if loadIndicator == 0
                                    then changeExecAddr fileEntry possibleLoadAddr
                                    else fileEntry
            loadFile mem newFileEntry bytes
            print "load done!!!!"
        Nothing        -> putStrLn $ "fileName not found ahhhhhhh: " ++ filename
--END OSFILE HELPERS


-- DFS function OSBGET
osBget :: Memory -> CPURegs -> IO ()
osBget mem regs = do
    putStrLn "hello from OSBget"
    channelNumber <- readIORef (y regs)
    mfile <- readSlot channelNumber (fileTable mem)
    case mfile of
        Just openFile -> do
            -- could do file mode check here as well

            -- then do check to see if there is actually a valid file here

            let fileEntry = getFileEntryByFileIndex openFile
                byteValue = getFileByte fileEntry (readWritePosition openFile)
            writeIORef (accumulator regs) byteValue
            srWriteCarry (statusReg regs) False

            -- NEED TO INCREMENT THE POINTERRRRRR

            -- need to read a byte at readWritePosition and return that via Accumulator
            -- or an error code of 0xFE
            -- and set C=0 to indicate valid character
            -- then rtsC as usual
        Nothing -> writeIORef (accumulator regs) 0xFE --error code
    rtsC mem regs

        -- where
        --     incrementOpenFilePointer :: Memory -> OpenFile -> IO ()
        --     incrementOpenFilePointer mem openFile = do
        --         writeSlot (handle openFile) openFile {} fileTable


-- DFS function OSFIND
osFind :: Memory -> CPURegs -> IO ()
osFind mem regs = do
    putStr "OSFind "
    accVal <- readIORef (accumulator regs)
    putStrLn $ "Acc val is: " ++ show accVal
    case accVal of
        0 -> putStrLn "0, close a file or close all files, Not implemented"
        0x40 -> do
            putStrLn "0x40, open a file (input)"
            let usingFileHandle = 1
                (_, files) = getFileData

            slot <- readSlot usingFileHandle (fileTable mem)
            case slot of
                Just _  -> putStrLn "there is already an openFile here. AHHHH"
                Nothing -> do

                    fileName <- getFilenameFromXY mem regs
                    print fileName

                    case fileNameToFileEntry fileName files of
                        Just fileEntry -> do
                            addOpenFile (fileTable mem) usingFileHandle fileName Input (fileIndex fileEntry)
                            writeIORef (accumulator regs) usingFileHandle
                            rtsC mem regs
                        Nothing        -> do
                            putStrLn $ "fileName not found ahhhhhhh: " ++ fileName
                            writeIORef (accumulator regs) 0
                            rtsC mem regs


        0x80 -> putStrLn "0x80, open a file (output), Not implemented"
        0xC0 -> putStrLn "0xC0, open a file (input/output), Not implemented"
        
        _ -> putStrLn "Unexpected Acc Val"

-- implement DFS code for passToCurrentFilingSystem call. Use .fscEntryPoint as refrence
passToCurrentFilingSystem :: Memory -> CPURegs -> IO ()
passToCurrentFilingSystem mem regs = do
    accVal <- readIORef (accumulator regs)
    putStr "passToCurrentFilingSystem: "
    case accVal of
        0 -> putStrLn "0, *OPT, not implemented yet" >> rtsC mem regs
        1 -> putStrLn "1, EOF check, not implemented yet" >> rtsC mem regs
        2 -> do
            putStrLn "2, */ command, Working on it"

            command <- getFilenameFromXY mem regs
            print command

            let (bytes, files) = getFileData

            case fileNameToFileEntry command files of
                Just fileEntry -> do
                    -- potencially in future add 0xFFFFFFFF check
                    loadFile mem fileEntry bytes
                    writeIORef (pc regs) (getExecAddr fileEntry)
                    return ()
                Nothing        -> putStrLn $ "fileName not found ahhhhhhh: " ++ command
        3 -> putStrLn "3, unrecognised, not implemented yet" >> rtsC mem regs
        4 -> do
            putStrLn "4, *RUN"
            command <- getFilenameFromXY mem regs

            putStrLn command

            let (bytes, files) = getFileData

            case fileNameToFileEntry command files of
                Just fileEntry -> do
                    -- potencially in future add 0xFFFFFFFF check
                    loadFile mem fileEntry bytes
                    writeIORef (pc regs) (getExecAddr fileEntry)
                Nothing        -> putStrLn $ "fileName not found ahhhhhhh: " ++ command

        5 -> putStrLn "5, *CAT, not implemented yet" >> rtsC mem regs
        6 -> putStrLn "6, New filing system, not implemented yet May be no action needed" >> rtsC mem regs
        7 -> putStrLn "7, return file handle range, not implemented yet" >> rtsC mem regs
        8 -> putStrLn "8, OS recived star command, not implemented yet" >> rtsC mem regs
        9 -> putStrLn "9, *EX, not implemented yet" >> rtsC mem regs
        10 -> putStrLn "10, *INFO, not implemented yet" >> rtsC mem regs
        11 -> putStrLn "11, *RUN for library, not implemented yet" >> rtsC mem regs
        12-> putStrLn "12, *RENAME, not implemented yet" >> rtsC mem regs
        n -> putStrLn ("passToCurrentFilingSystem: Error unexpected acc val: " ++ show n) >> rtsC mem regs

-- helper functions
pad7 :: String -> String
pad7 = take 7 . (++ repeat ' ')

getFilenameFromXY :: Memory -> CPURegs -> IO String
getFilenameFromXY mem regs = do
    xVal <- readIORef (x regs)
    yVal <- readIORef (y regs)
    let fileAddressXY :: Word16
        fileAddressXY = (fromIntegral yVal `shiftL` 8) + fromIntegral xVal

    str <- readStringFromMemory mem fileAddressXY
    return $ pad7 $ stripQuotes str
readStringFromMemory :: Memory -> Word16 -> IO String
readStringFromMemory mem addr = go addr []
    where
        go current acc = do
            byte <- readMemory mem current
            if byte == 13 -- this is \r
                then return (map (toEnum . fromIntegral) (reverse acc)) -- convert to string
                else go (current + 1) (byte : acc)
stripQuotes :: String -> String
stripQuotes s = case s of
    ('"':rest) -> reverse $ dropWhile (== '"') $ reverse rest
    _          -> s


-- -- OS function .closeSpoolOrExecFile
-- closeSpoolOrExecFile :: Memory -> CPURegs -> IO ()
-- closeSpoolOrExecFile mem regs = do
--     putStrLn "hello from closeSpoolOrExecFile"

-- -- OS function .starLoadSave
-- starSaveload :: Memory -> CPURegs -> IO ()
-- starSaveload mem regs = do
--     accVal <- readIORef (accumulator regs)
--     if accVal == 0
--         then putStrLn "saveload called for *SAVE. not implemented" >> return ()
--         else putStrLn "saveload called for *LOAD"

--     xyFileName <- getFilenameFromXY mem regs
--     print xyFileName

--     (bytes, files) <- getFileData

--     case fileNameToFileEntry xyFileName files of
--         Just fileEntry -> do
--             loadFile mem fileEntry bytes
--             rtsC mem regs
--         Nothing        -> do
--             print "fileName not found"
--             writeIORef (pc regs) 0xE267


-- Return from Subroutine copy, because after a intercepted function its best to just rtsC to get back to 6502 code
-- Using imported rts function but adapted to remove uneeded parameters
rtsC :: Memory -> CPURegs -> IO ()
rtsC mem regs = rts undefined mem regs undefined