module DiskFileHandling where

import MemoryRegisters (Memory(fileTable), writeMemory, readMemory, FileTable, OpenFile(..), FileMode)
import Utilities ( showHexF )

import qualified Data.ByteString as ByteStr
import System.IO (withBinaryFile, IOMode(WriteMode))
import Control.Monad (forM)
import Data.Bits ((.&.), (.|.), Bits (shiftL, testBit))
import Data.Word (Word16, Word8)
import Data.List (find)
import Data.IORef (readIORef, writeIORef)
import qualified Data.Vector as IBVector
import System.IO.Unsafe (unsafePerformIO)

diskFileName :: String
diskFileName = "games/Disc022-SpaceInvadersArcadeAction.ssd" -- final solution should allow runtime selecting of .ssd file

-- Handle Read and parse .ssd file from disk
initFileData :: IO (ByteStr.ByteString, IBVector.Vector FileEntry)
initFileData = do
    bytes <- ByteStr.readFile diskFileName
    let files = parseFiles bytes
        fileVector = IBVector.fromList files
    print "Hello i expect this to be printed only once"
    mapM_ printFile files
    return (bytes, fileVector)

{-# NOINLINE diskBytes #-}
{-# NOINLINE fileEntryVector #-}
diskBytes :: ByteStr.ByteString
fileEntryVector :: IBVector.Vector FileEntry
(diskBytes, fileEntryVector) = unsafePerformIO initFileData
-- Use of unsafePerformIO ensures the file is read and parsed only once
-- This is safe because the file is read-only. Any external modifications
-- during runtime are ignored, which a good thing

data FileEntry = FileEntry
    { dir             :: Char
    , name            :: String
    , load            :: Word16
    , exec            :: Word16
    , len             :: Int
    , fileStartOffset :: Int
    , locked          :: Bool
    , fileIndex       :: Int
    } deriving Show

parseFiles :: ByteStr.ByteString -> [FileEntry]
parseFiles blob =
    let sector1 = ByteStr.take 256 blob
        sector2 = ByteStr.take 256 $ ByteStr.drop 256 blob

        -- extract 31 chunks of 8 bytes each from sector1
        sector1Chunks = [ByteStr.take 8 $ ByteStr.drop (8 + i*8) sector1 | i <- [0..30]]

        -- extract corresponding chunks from sector2
        sector2Chunks = [ByteStr.take 8 $ ByteStr.drop (4+1+1+2 + i*8) sector2 | i <- [0..30]]

        entries = zipWith3 buildFileEntry sector1Chunks sector2Chunks [1..]
    in entries

    where
        buildFileEntry :: ByteStr.ByteString -> ByteStr.ByteString -> Int -> FileEntry
        buildFileEntry sector1Chunk sector2Chunk fileNumberVal =
            let fname      = getString sector1Chunk 0 7
                dirChar    = toEnum . fromEnum $ getWord8 sector1Chunk 7 .&. 0x7F
                lockedFlag = testBit (getWord8 sector1Chunk 7) 7

                loadAddr   = getWord16LE sector2Chunk 0
                execAddr   = getWord16LE sector2Chunk 2
                lengthLow  = getWord16LE sector2Chunk 4
                lengthHigh = fromIntegral ((getWord8 sector2Chunk 6 .&. 0x30) `shiftL` 12)
                fullLength = lengthHigh + fromIntegral lengthLow

                highBits  = getWord8 sector2Chunk 6 .&. 0x03
                lowByte   = getWord8 sector2Chunk 7
                startSector = (fromIntegral highBits `shiftL` 8) + fromIntegral lowByte
                fileStartOffset  = startSector * 256
            in FileEntry dirChar  fname loadAddr execAddr fullLength fileStartOffset lockedFlag fileNumberVal

--parseFile helper functions
getWord16LE :: ByteStr.ByteString -> Int -> Word16
getWord16LE bs offset =
  let lo = fromIntegral (ByteStr.index bs offset)
      hi = fromIntegral (ByteStr.index bs (offset + 1))
  in (hi `shiftL` 8) .|. lo
getWord8 :: ByteStr.ByteString -> Int -> Word8
getWord8 = ByteStr.index
getString :: ByteStr.ByteString -> Int -> Int -> String
getString bs offset len = map (toEnum . fromEnum) $ ByteStr.unpack $ ByteStr.take len $ ByteStr.drop offset bs

--debug
printFile :: FileEntry -> IO ()
printFile f =
    let dirChar   = [dir f]
        fileName  = name f
        loadHex   = pad4 $ showHexF (load f)
        execHex   = pad4 $ showHexF (exec f)
        lenHex    = pad4 $ showHexF (len f)
        offsetHex = pad4 $ showHexF (fileStartOffset f)
        lockStr   = if locked f then "L" else ""

        pad4 s = replicate (4 - length s) '0' ++ s
        line = dirChar ++ "\t[" ++ fileName ++ "]\t" ++ loadHex ++ "\t" ++ execHex
               ++ "\t" ++ lenHex ++ "\t" ++ offsetHex ++ "\t" ++ lockStr
    in putStrLn line
dumpRAM :: Memory -> IO ()
dumpRAM mem = do
    bytes <- forM [0..0x7FFF] $ \addr -> do
        val <- readMemory mem addr
        return (fromIntegral (val .&. 0xFF) :: Word8)

    let bs = ByteStr.pack bytes
    withBinaryFile "ram_dump.bin" WriteMode $ \h -> ByteStr.hPut h bs
    putStrLn "dump done"

--Function Intercept helpers
getFileData :: (ByteStr.ByteString, [FileEntry])
getFileData = (diskBytes, IBVector.toList fileEntryVector)

getFileByte :: FileEntry -> Int -> Word8
getFileByte fileEntry index = 
    let fileStart = fileStartOffset fileEntry
        byteOfInterest = index + fileStart
    in ByteStr.index diskBytes byteOfInterest

loadFile :: Memory -> FileEntry -> ByteStr.ByteString -> IO ()
loadFile mem fileEntry fileBytes = do
    let slice = ByteStr.take (len fileEntry) $ ByteStr.drop (fileStartOffset fileEntry) fileBytes
    sequence_ [writeMemory mem (fromIntegral (load fileEntry + i)) b | (i, b) <- zip [0..] (ByteStr.unpack slice)]

getExecAddr :: FileEntry -> Word16
getExecAddr = exec

changeExecAddr :: FileEntry -> Word16 -> FileEntry
changeExecAddr fileEntry newExec = fileEntry {exec = newExec}

-- Deal with annoying fileEntries
fileNameToFileEntry :: String -> [FileEntry] -> Maybe FileEntry
fileNameToFileEntry fileName = find (\f -> name f == fileName)

getFileEntryByFileIndex :: OpenFile -> FileEntry
getFileEntryByFileIndex openFile = 
    let n = filePointer openFile
        
    in fileEntryVector IBVector.! (n - 1) -- will throw exception if out of bounds

readSlot :: Word8 -> FileTable -> IO (Maybe OpenFile)
readSlot 1 (a, _, _) = readIORef a
readSlot 2 (_, b, _) = readIORef b
readSlot 3 (_, _, c) = readIORef c
readSlot _ _         = return Nothing

writeSlot :: Word8 -> Maybe OpenFile -> FileTable -> IO ()
writeSlot 1 val (a, _, _) = writeIORef a val
writeSlot 2 val (_, b, _) = writeIORef b val
writeSlot 3 val (_, _, c) = writeIORef c val
writeSlot _ _ _           = return ()

addOpenFile :: FileTable -> Word8 -> String -> FileMode -> Int -> IO ()
addOpenFile fileTable newHandle newFileName newMode fileIndex = do
    let newOpenFile = OpenFile newHandle newFileName newMode 0 fileIndex

    writeSlot newHandle (Just newOpenFile) fileTable

incrementOpenFilePointer :: Memory -> OpenFile -> IO ()
incrementOpenFilePointer mem openFile = do
    let newOpenFile = openFile { readWritePosition = readWritePosition openFile + 1 }
    writeSlot (handle openFile) (Just newOpenFile) (fileTable mem)