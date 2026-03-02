module LoadRom where

import MemoryRegisters (Memory, writeMemoryArrayOnly, readMemory)
import Utilities ( showHexF )

import qualified Data.Vector as IBVector
import qualified Data.ByteString as ByteStr
import System.IO (withBinaryFile, IOMode(ReadMode, WriteMode))
import Control.Monad (forM_, forM)
import Data.Bits (shiftR, (.&.), (.|.), Bits (shiftL, testBit))
import Data.Word (Word16, Word8)

loadRom :: FilePath -> Word16 -> Memory -> IO ()
loadRom path codeOffset mem = withBinaryFile path ReadMode $ \fHandle -> do
    bytes <- ByteStr.unpack <$> ByteStr.hGetContents fHandle
    let bytesAndIndexes = zip ([codeOffset..] :: [Word16]) bytes
    forM_ bytesAndIndexes $ uncurry (writeMemoryArrayOnly mem)

loadMode7Font :: FilePath -> Int -> IO (IBVector.Vector [[Bool]])
loadMode7Font path headersize = withBinaryFile path ReadMode $ \fHandle -> do
    (bytes :: [Word8]) <- ByteStr.unpack <$> ByteStr.hGetContents fHandle
    let withoutHeader = drop headersize bytes
        letterSizedChunks = chunksOf 160 withoutHeader
        rowColumnFormat = map rearrangeLettersRowColumnFormat letterSizedChunks

    return $ IBVector.fromList rowColumnFormat


rearrangeLettersRowColumnFormat :: [Word8] -> [[Bool]]
rearrangeLettersRowColumnFormat input = concatMap separateDoubleRow doubleRows
    where
        width = 16
        doubleRows = chunksOf width input

        separateDoubleRow :: [Word8] -> [[Bool]]
        separateDoubleRow row =
            let top    = map (\b -> (b .&. 0xF) /=0) row
                bottom = map (\b -> (b `shiftR` 4) /=0) row
            in [top, bottom]

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (head_, tail_) = splitAt n xs in head_ : chunksOf n tail_

loadSsdAllFiles :: Memory -> IO ()
loadSsdAllFiles mem = do

    bytes <- ByteStr.readFile "games/DiscA13-MapQuizMode7.ssd"
    let files = parseFiles bytes
    mapM_ printFile files

    let loadFile fileEntry = do
            if (load fileEntry) /= 0 then do
                let slice = ByteStr.take (len fileEntry) $ ByteStr.drop (fileOffset fileEntry) bytes
                sequence_ [writeMemoryArrayOnly mem (fromIntegral (load fileEntry + i)) b | (i, b) <- zip [0..] (ByteStr.unpack slice)]
            else
                return ()
        
    mapM_ loadFile files
    --dumpRAM mem


data FileEntry = FileEntry
  { dir     :: Char
  , name    :: String
  , load    :: Word16
  , exec    :: Word16
  , len     :: Int
  , fileOffset :: Int
  , locked  :: Bool
  } deriving Show

-- Read a 16-bit little-endian value from ByteString
getWord16LE :: ByteStr.ByteString -> Int -> Word16
getWord16LE bs offset =
  let lo = fromIntegral (ByteStr.index bs offset)
      hi = fromIntegral (ByteStr.index bs (offset + 1))
  in (hi `shiftL` 8) .|. lo

-- Read 8-bit value
getWord8 :: ByteStr.ByteString -> Int -> Word8
getWord8 = ByteStr.index

-- Read a string from ByteString
getString :: ByteStr.ByteString -> Int -> Int -> String
getString bs offset len = map (toEnum . fromEnum) $ ByteStr.unpack $ ByteStr.take len $ ByteStr.drop offset bs

-- Parse all files from a disk image
parseFiles :: ByteStr.ByteString -> [FileEntry]
parseFiles blob =
    let sector1 = ByteStr.take 256 blob
        sector2 = ByteStr.take 256 $ ByteStr.drop 256 blob

        -- extract 31 chunks of 8 bytes each from sector1
        sector1Chunks = [ByteStr.take 8 $ ByteStr.drop (8 + i*8) sector1 | i <- [0..30]]

        -- extract corresponding chunks from sector2
        sector2Chunks = [ByteStr.take 8 $ ByteStr.drop (4+1+1+2 + i*8) sector2 | i <- [0..30]]

        entries = zipWith buildFileEntry sector1Chunks sector2Chunks
    in filter (\e -> not (null (name e) || all (== '\0') (name e))) entries

    where
        buildFileEntry :: ByteStr.ByteString -> ByteStr.ByteString -> FileEntry
        buildFileEntry sector1Chunk sector2Chunk =
            let fname      = getString sector1Chunk 0 7
                dirChar    = toEnum . fromEnum $ getWord8 sector1Chunk 7 .&. 0x7F
                lockedFlag = testBit (getWord8 sector1Chunk 7) 7

                loadAddr   = getWord16LE sector2Chunk 0
                execAddr   = getWord16LE sector2Chunk 2
                lengthLow  = getWord16LE sector2Chunk 4
                lengthHigh = fromIntegral ((getWord8 sector2Chunk 6 .&. 0x30) `shiftL` 12)
                fullLength = lengthHigh + fromIntegral lengthLow

                startSector = getWord8 sector2Chunk 7 .&. 0x03
                fileOffset  = fromIntegral startSector * 256
            in FileEntry dirChar fname loadAddr execAddr fullLength fileOffset lockedFlag

printFile :: FileEntry -> IO ()
printFile f =
    let loadHex = showHexF (load f)
        execHex = showHexF (exec f)
        lockStr = if locked f then "L" else ""
    in putStrLn $ [dir f] ++ " " ++ name f ++ " " ++ loadHex ++ " " ++ execHex ++ " " ++ showHexF (len f) ++ " " ++ lockStr

dumpRAM :: Memory -> IO ()
dumpRAM mem = do
    bytes <- forM [0..0x7FFF] $ \addr -> do
        val <- readMemory mem addr
        return (fromIntegral (val .&. 0xFF) :: Word8)

    let bs = ByteStr.pack bytes
    withBinaryFile "ram_dump.bin" WriteMode $ \h -> ByteStr.hPut h bs
    putStrLn "dump done"