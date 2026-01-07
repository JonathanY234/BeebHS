module LoadRom where

import Memory (Memory, writeMemory)

import qualified Data.Vector as IBVector
import qualified Data.ByteString as B
import System.IO (withBinaryFile, IOMode(ReadMode))
import Control.Monad (forM_)
import Data.Bits (shiftR, (.&.))
import Data.Word (Word16, Word8)

loadRom :: FilePath -> Word16 -> Memory -> IO ()
loadRom path codeOffset mem = withBinaryFile path ReadMode $ \fHandle -> do
    bytes <- B.unpack <$> B.hGetContents fHandle
    let bytesAndIndexes = zip ([codeOffset..] :: [Word16]) bytes
    forM_ bytesAndIndexes $ uncurry (writeMemory mem)

loadMode7Font :: FilePath -> Int -> IO (IBVector.Vector [[Bool]])
loadMode7Font path headersize = withBinaryFile path ReadMode $ \fHandle -> do
    (bytes :: [Word8]) <- B.unpack <$> B.hGetContents fHandle
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
            let top    = map (\b -> (b .&. 0xF) /= 0) row
                bottom = map (\b -> (b `shiftR` 4) /= 0) row
            in [top, bottom]

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (head_, tail_) = splitAt n xs in head_ : chunksOf n tail_