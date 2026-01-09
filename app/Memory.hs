module Memory where

import Data.Word (Word16, Word8)
import Data.IORef (IORef, newIORef, readIORef)
import qualified Data.Vector.Unboxed.Mutable as MUVector
import qualified Data.Vector as IBVector
import Data.Bits (Bits(complement), (.&.), (.|.))
import Control.Monad (forM_, when)

data Memory = Memory {m :: MUVector.IOVector Word8, keyboardMatrix :: IORef (IBVector.Vector Bool)}

readMemory :: Memory -> Word16 -> IO Word8
readMemory mem address = do

    if address == 0xFE4F && False -- temp
        then do
            --putStrLn $ "FE4F read: initially: " ++ show old_value
            ora <- MUVector.read (m mem) 0xFE41
            ddra <- MUVector.read (m mem) 0xFE43
            isQpressed <- keyPressed mem 0x10

            if isQpressed
                then putStrLn "Qpressed"
                else putStrLn "not"

            let isQpolled = ora == 0x10
            --printKeyboardMatrix mem

            let temp = ora .&. ddra
            if isQpressed && isQpolled
                then do 
                    putStrLn $ "FE4F read true path: final : " ++ show (temp .&. complement 0x80)
                    return $ temp .&. complement 0x80  -- bit 7 high
                else do 
                    putStrLn $ "FE4F read false path: final : " ++ show (temp .|. 0x80 )
                    return $ temp .|. 0x80             -- bit 7 low
                    --return 0
        else MUVector.read (m mem) (fromIntegral address)

keyPressed :: Memory -> Int -> IO Bool
keyPressed Memory{keyboardMatrix = kbM} rowCol = do
    let row = rowCol `div` 16
    let col = rowCol `mod` 16
    kb <- readIORef kbM
    return $ kb IBVector.! (col + row * kbMatrixCols)

printKeyboardMatrix :: Memory -> IO ()
printKeyboardMatrix mem = do
    kb <- readIORef (keyboardMatrix mem)
    let lst = IBVector.toList kb
    forM_ (zip [1..] lst) $ \(i, val) -> do
        putStr (if val then "1," else "0,")
        when (i `mod` kbMatrixCols == 0) $
            putStrLn ""


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

--       0x00      0x01  0x02  0x03 0x04 0x05 0x06 0x07 0x08 0x09    
-- 0x00  Shift     Ctrl  <------- starup up DIP swicthes ------->  
-- 0x10  Q         3     4     5    f4   8    f7   =-   ~^   Left    
-- 0x20  f0        W     E     T    7    I    9    0    £    Down    
-- 0x30  1         2     D     R    6    U    O    P    [{   Up      
-- 0x40  CapsLck   A     X     F    Y    J    K    @    :*   Return  
-- 0x50  ShiftLck  S     C     G    H    N    L    ;+   ]}   Delete  
-- 0x60  Tab       Z     SPC   V    B    M    <,   >.   /?   Copy    
-- 0x70  ESC       f1    f2    f3   f5   f6   f8   f9   \    Right   

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