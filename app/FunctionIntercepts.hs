module FunctionIntercepts where

import MemoryRegisters (Memory, CPURegs(pc, accumulator, x, y), readMemory, fileTable, FileMode(Input), OpenFile(readWritePosition), readRegs, writeRegs)
import DiskFileHandling (getFileData, fileNameToFileEntry, getExecAddr, loadFile, changeExecAddr, readSlot, addOpenFile, FileEntry(fileIndex, len), getFileEntryByFileIndex, getFileByte, incrementOpenFilePointer, writeSlot)
import CPUInstructions (rts, srWriteCarry)

import Data.Bits (Bits(shiftL))
import Data.Word (Word16, Word8)

checkForIntercept :: Memory -> IO Bool
checkForIntercept mem = do
    pcVal <- readRegs pc mem
    case pcVal of
        --0xE23E -> starSaveload mem regs >> return True -- not needed
        0xFFDD -> osFile mem >> return True
        0xFFCE -> osFind mem >> return True
        0xFFD7 -> osBget mem >> return True
        0xE031 -> passToCurrentFilingSystem mem >> return True
        _      -> return False

-- DFS function OSFILE
osFile :: Memory -> IO ()
osFile mem = do
    putStr "OSFile "

    accVal <- readRegs accumulator mem
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

            paramBlock <- getParamBlockXY mem
            filename <- getFileNameFromParamBlock mem paramBlock

            -- Get load address from param block
            loadAddrBytes <- mapM (readMemory mem) [paramBlock + 2, paramBlock + 3]
            let loadAddr = bytesToWord16 loadAddrBytes

            loadIndicator <- readMemory mem (paramBlock + 6)

            osFileLoadFileHelper mem filename loadIndicator loadAddr

            -- A, P are destroyed; X,Y unchanged
            --writeIORef (accumulator regs) 0x00  -- or leave undefined
            rtsC mem


        n -> putStrLn $ "Unknown A val: " ++ show n

--OSFILE HELPERS
getParamBlockXY :: Memory -> IO Word16
getParamBlockXY mem = do
    xVal <- readRegs x mem
    yVal <- readRegs y mem
    return $ fromIntegral yVal `shiftL` 8 + fromIntegral xVal
getFileNameFromParamBlock :: Memory -> Word16 -> IO String
getFileNameFromParamBlock mem paramBlock = do
    filenameAddrL <- readMemory mem paramBlock
    filenameAddrH <- readMemory mem (paramBlock + 1)
    let filenameAddr = (fromIntegral filenameAddrH `shiftL` 8) + fromIntegral filenameAddrL
    str <- readStringFromMemory mem filenameAddr
    return $ pad7 str
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
osBget :: Memory -> IO ()
osBget mem = do
    putStrLn "hello from OSBget"
    channelNumber <- readRegs y mem

    mfile <- readSlot channelNumber (fileTable mem)
    case mfile of
        Just openFile -> do
            -- could do file mode check here as well

            -- then do check to see if there is actually a valid file here
            let fileEntry = getFileEntryByFileIndex openFile
                
            if readWritePosition openFile < len fileEntry then do

                let byteValue = getFileByte fileEntry (readWritePosition openFile)
                writeRegs accumulator mem byteValue
                srWriteCarry mem False
                incrementOpenFilePointer mem openFile
            else do

                writeRegs accumulator mem 0xFE --EOF
                srWriteCarry mem True

        Nothing -> writeRegs accumulator mem 0 --this should not happen. Not sure if this is correct responce
    rtsC mem


-- DFS function OSFIND
osFind :: Memory -> IO ()
osFind mem = do
    putStr "OSFind "
    accVal <- readRegs accumulator mem
    case accVal of
        0 -> do
            putStrLn "0, close a file or close all files, Only 'close a file' part implemented"
            channelNumber <- readRegs y mem

            writeSlot channelNumber Nothing (fileTable mem)

        0x40 -> do
            putStrLn "0x40, open a file (input)"
            let usingFileHandle = 1
                (_, files) = getFileData

            slot <- readSlot usingFileHandle (fileTable mem)
            case slot of
                Just _  -> putStrLn "there is already an openFile here. AHHHH"
                Nothing -> do

                    fileName <- getFilenameFromXY mem
                    print fileName

                    case fileNameToFileEntry fileName files of
                        Just fileEntry -> do
                            addOpenFile (fileTable mem) usingFileHandle fileName Input (fileIndex fileEntry)
                            writeRegs accumulator mem usingFileHandle
                        Nothing        -> do
                            putStrLn $ "fileName not found ahhhhhhh: " ++ fileName
                            writeRegs accumulator mem 0



        0x80 -> putStrLn "0x80, open a file (output), Not implemented"
        0xC0 -> putStrLn "0xC0, open a file (input/output), Not implemented"

        _ -> putStrLn "Unexpected Acc Val"
    rtsC mem

-- implement DFS code for passToCurrentFilingSystem call .fscEntryPoint as refrence
passToCurrentFilingSystem :: Memory -> IO ()
passToCurrentFilingSystem mem = do
    accVal <- readRegs accumulator mem
    putStr "passToCurrentFilingSystem: "
    case accVal of
        0 -> putStrLn "0, *OPT, not implemented yet" >> rtsC mem
        1 -> putStrLn "1, EOF check, not implemented yet" >> rtsC mem
        2 -> do
            putStrLn "2, */ command"

            command <- getFilenameFromXY mem
            print command

            let (bytes, files) = getFileData

            case fileNameToFileEntry command files of
                Just fileEntry -> do
                    -- potencially in future add 0xFFFFFFFF check
                    loadFile mem fileEntry bytes
                    writeRegs pc mem (getExecAddr fileEntry)
                    --return ()
                Nothing        -> putStrLn $ "fileName not found ahhhhhhh: " ++ command
        3 -> putStrLn "3, unrecognised, not implemented yet" >> rtsC mem
        4 -> do
            putStrLn "4, *RUN"
            command <- getFilenameFromXY mem

            putStrLn command

            let (bytes, files) = getFileData

            case fileNameToFileEntry command files of
                Just fileEntry -> do
                    -- potencially in future add 0xFFFFFFFF check
                    loadFile mem fileEntry bytes
                    writeRegs pc mem (getExecAddr fileEntry)
                Nothing        -> putStrLn $ "fileName not found ahhhhhhh: " ++ command

        5 -> putStrLn "5, *CAT, not implemented yet" >> rtsC mem
        6 -> putStrLn "6, New filing system, Currently no action taken, mostly works fine though" >> rtsC mem
        7 -> putStrLn "7, return file handle range, not implemented yet" >> rtsC mem
        8 -> rtsC mem
            --putStrLn "8, OS recived star command, we ignored it" >> rtsC mem
        9 -> putStrLn "9, *EX, not implemented yet" >> rtsC mem
        10 -> putStrLn "10, *INFO, not implemented yet" >> rtsC mem
        11 -> putStrLn "11, *RUN for library, not implemented yet" >> rtsC mem
        12-> putStrLn "12, *RENAME, not implemented yet" >> rtsC mem
        n -> putStrLn ("passToCurrentFilingSystem: Error unexpected acc val: " ++ show n) >> rtsC mem

-- helper functions
pad7 :: String -> String -- need to standardise where i use this function
pad7 = take 7 . (++ repeat ' ')

getFilenameFromXY :: Memory -> IO String
getFilenameFromXY mem = do
    xVal <- readRegs x mem
    yVal <- readRegs y mem
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

-- Return from Subroutine copy, because after a intercepted function its best to just rtsC to get back to 6502 code
-- Using imported rts function but adapted to remove uneeded parameters
rtsC :: Memory -> IO ()
rtsC mem  = rts undefined mem undefined