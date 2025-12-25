module Memory where

import Data.Word (Word16, Word8)
import Data.IORef (IORef, newIORef, readIORef)
import qualified Data.Vector.Unboxed.Mutable as MUVector
import qualified Data.Vector as IBVector
import Data.Bits (Bits(complement), (.&.), (.|.))

data Memory = Memory {m :: MUVector.IOVector Word8, keyboardMatrix :: IORef (IBVector.Vector Bool)}

readMemory :: Memory -> Word16 -> IO Word8
readMemory mem address = do
    value <- MUVector.read (m mem) (fromIntegral address)
    if address == 0xFE4F
        then do
            putStrLn $ "FE4F read: initially: " ++ show value
            ora <- MUVector.read (m mem) 0xFE41
            ddra <- MUVector.read (m mem) 0xFE43
            isQpressed <- keyPressed mem 0x10

            if isQpressed
                then putStrLn "Qpressed"
                else putStrLn "not"

            let isQpolled = ora == 0x10
            printKeyboardMatrix mem

            let temp = ora .&. ddra
            if isQpressed && isQpolled
                then do 
                    putStrLn $ "FE4F read true path: final : " ++ show (temp .&. complement 0x80)
                    return $ temp .&. complement 0x80  -- bit 7 high
                else do 
                    putStrLn $ "FE4F read false path: final : " ++ show (temp .|. 0x80 )
                    --return $ temp .|. 0x80             -- bit 7 low
                    return 0
        else return value

keyPressed :: Memory -> Int -> IO Bool
keyPressed Memory{keyboardMatrix = kbM} rowCol = do
    let row = rowCol `div` 16
    let col = rowCol `mod` 16
    kb <- readIORef kbM
    return $ kb IBVector.! (col + row * kbMatrixCols)

printKeyboardMatrix :: Memory -> IO ()
printKeyboardMatrix mem = do
    kb <- readIORef (keyboardMatrix mem)  -- get the vector
    -- convert to a list of Bool for printing
    let lst = IBVector.toList kb
    print lst

writeMemory :: Memory -> Word16 -> Word8 -> IO ()
writeMemory memory address value = do
    MUVector.write (m memory) (fromIntegral address) value
    --putStrLn $ "Hello attempted to write memory at " ++ showHex address ""
    -- if address == 0xFE4F
    --     then putStrLn $ "FE4F written. Value: " ++ show value
    --     else putStr ""
    -- if address == 0xFE40
    --     then putStrLn $ "FE40 written. Value: " ++ show value
    --     else putStr ""


kbMatrixRows :: Int
kbMatrixRows = 8
kbMatrixCols :: Int
kbMatrixCols = 10

initMemory :: Word8 -> IO Memory
initMemory initialValue = do
    mVec <- MUVector.replicate (64*1024) initialValue
    kMatr  <- newIORef (IBVector.replicate (kbMatrixRows * kbMatrixCols) False)
    return $ Memory mVec kMatr

data CPURegs = CPURegs {pc :: IORef Word16, accumulator :: IORef Word8, x :: IORef Word8, y :: IORef Word8, stackP :: IORef Word8, statusReg :: IORef Word8}

initRegisters :: Word16 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> IO CPURegs
initRegisters ipc ia ix iy isp isr = do
    pcRef <- newIORef ipc
    aRef  <- newIORef ia
    xRef  <- newIORef ix
    yRef  <- newIORef iy
    spRef <- newIORef isp
    srRef <- newIORef isr
    return (CPURegs pcRef aRef xRef yRef spRef srRef)