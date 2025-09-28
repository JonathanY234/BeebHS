module LoadRom where

import qualified Data.Vector.Unboxed as IUVector
import qualified Data.Vector as IBVector
import qualified Data.ByteString as B
import System.IO (withBinaryFile, IOMode(ReadMode))
import Data.Word (Word8)
import Control.Monad (when)
import Data.Bits (shiftR, (.&.))


loadRom :: FilePath -> Int -> IO (IUVector.Vector Word8)
loadRom path expectedSize = withBinaryFile path ReadMode $ \fHandle -> do

    (bytes :: [Word8]) <- B.unpack <$> B.hGetContents fHandle
    let actualSize = length bytes

    -- this double checks my understanding of the memory layout sizes
    when (actualSize /= expectedSize) $
        putStrLn $ "Warning: ROM size mismatch! Expected " ++ show expectedSize
                ++ " bytes but got " ++ show actualSize ++ " bytes"

    return $ IUVector.fromList bytes

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