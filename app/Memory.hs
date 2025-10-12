module Memory where

import Data.Word (Word16, Word8)
import qualified Data.Vector.Unboxed.Mutable as MUVector

data Memory = Memory {m :: MUVector.IOVector Word8}

readMemory :: Memory -> Word16 -> IO Word8
readMemory memory address = MUVector.read (m memory) (fromIntegral address)

writeMemory :: Memory -> Word16 -> Word8 -> IO ()
writeMemory memory address value = do
    MUVector.write (m memory) (fromIntegral address) value --value removed at the insistence of hlint
    --putStrLn $ "Hello attempted to write memory at " ++ showHex address ""

initMemory :: IO Memory
initMemory = Memory <$> MUVector.replicate (64*1024) 0