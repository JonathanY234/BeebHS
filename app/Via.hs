module Via where

import Data.Word (Word32, Word16, Word8)
import qualified Data.Vector as IBVector
import Data.IORef (IORef, newIORef, readIORef, modifyIORef', writeIORef)
import Data.Bits ((.|.), (.&.), shiftR, shiftL, Bits (complement))
import Control.Monad (when, unless)

-- Via Registers

-- ORA (output register a)      0xFE01   |   ORB (output register a)      0xFE00
-- IRA (input register a)       ~        |   IRB (input register b)       ~

-- DDRA (data direction r a)    0xFE03   |   DDRB (data direction r b)    0xFE02
-- t1 and t2

-- IER (interrupt enable r)     0xFE0E   |   IFR (interrupt flag r)       0xFE0D
-- ACR (Auxiliary Control r)    0xFE0B   |   PCR (peripheral control r)   0xFE0C

kbMatrixRows :: Int
kbMatrixRows = 8
kbMatrixCols :: Int
kbMatrixCols = 10

data Via = Via {ora :: IORef Word8, orb :: IORef Word8, ira :: IORef Word8, irb :: IORef Word8,
                ddra :: IORef Word8, ddrb :: IORef Word8,
                t1_latch :: IORef Word32, t1_counter :: IORef Int, t2_latch :: IORef Word32, t2_counter :: IORef Int,
                t1pb7 :: IORef Word8, t1_hit :: IORef Int, t2_hit :: IORef Int,
                ifr :: IORef Word8, ier :: IORef Word8, acr :: IORef Word8, pcr :: IORef Word8, sr :: IORef Word8, srCount :: IORef Int,
                ca1 :: IORef Int, ca2 :: IORef Int, cb1 :: IORef Int, cb2 :: IORef Int,
                
                keyboardMatrix :: IORef (IBVector.Vector Bool),
                sysviaOnly :: SysviaOnly}

data SysviaOnly = SysviaOnly {ic32 :: IORef Word8, sdbVal :: IORef Word8, sdbOut :: IORef Word8}

initSysviaOnly :: IO SysviaOnly
initSysviaOnly = do
    ic32Ref     <- newIORef 0
    sdbValRef   <- newIORef 0
    sdbOutRef   <- newIORef 0
    return (SysviaOnly ic32Ref sdbValRef sdbOutRef)

initVia :: IO Via
initVia = do
    oraRef        <- newIORef 0
    orbRef        <- newIORef 0
    iraRef        <- newIORef 0
    irbRef        <- newIORef 0
    ddraRef       <- newIORef 0
    ddrbRef       <- newIORef 0
    t1_latchRef   <- newIORef 0
    t1_counterRef <- newIORef 0
    t2_latchRef   <- newIORef 0
    t2_counterRef <- newIORef 0
    t1pb7Ref      <- newIORef 0
    t1_hitRef     <- newIORef 0
    t2_hitRef     <- newIORef 0
    ifrRef        <- newIORef 0
    ierRef        <- newIORef 0
    acrRef        <- newIORef 0
    pcrRef        <- newIORef 0
    srRef         <- newIORef 0
    srCountRef    <- newIORef 0
    ca1Ref        <- newIORef 0
    ca2Ref        <- newIORef 0
    cb1Ref        <- newIORef 0
    cb2Ref        <- newIORef 0
    sysviaRef     <- initSysviaOnly
    kMatr  <- newIORef (IBVector.replicate (kbMatrixRows * kbMatrixCols) False)

    return (Via oraRef orbRef iraRef irbRef ddraRef ddrbRef
             t1_latchRef t1_counterRef t2_latchRef t2_counterRef
             t1pb7Ref t1_hitRef t2_hitRef
             ifrRef ierRef acrRef pcrRef srRef srCountRef ca1Ref ca2Ref cb1Ref cb2Ref
             kMatr
             sysviaRef)


intTimer1, intTimer2 :: Word8
intTimer1 = 0x40
intTimer2 = 0x20

intCB1, intCB2, intCA1, intCA2 :: Word8
intCB1 = 0x10
intCB2 = 0x08
intCA1 = 0x02
intCA2 = 0x01

viaUpdateIFR :: Via -> IO ()
viaUpdateIFR theVia = do
    theIFR <- readIORef (ifr theVia)
    theIER <- readIORef (ier theVia)
    if (theIFR .&. 0x7F) .&. (theIER .&. 0x7f)  /= 0 then
        writeIORef (ifr theVia) (theIFR .|. 0x80)
        -- figure out this interupt later
    else
        writeIORef (ifr theVia) (theIFR .&. complement 0x80)
        -- figure out this interupt later

sysviaWritePortA :: Via -> Word8 -> IO ()
sysviaWritePortA theVia val = do
    writeIORef (sdbOut (sysviaOnly theVia)) val
    
    sysviaUpdateSdb theVia

sysviaUpdateSdb :: Via -> IO ()
sysviaUpdateSdb theVia = do
    writeIORef (sdbVal (sysviaOnly theVia)) =<< readIORef (sdbOut (sysviaOnly theVia))

    keyUpdate theVia
    -- do stuff with sdbVal
    -- then some stuff only for (not) autoscan

    -- doesnt match cases where sdbval not set immidately before calling this function
    --      only matters for (not) autoscan as val not used in autoscan mode

sysviaWritePortB :: Via -> Word8 -> IO ()
sysviaWritePortB = sysviaWriteIC32

sysviaReadPortA :: Via -> IO Word8
sysviaReadPortA theVia = do
    sysviaUpdateSdb theVia
    readIORef (sdbVal (sysviaOnly theVia))

sysviaWriteIC32 :: Via -> Word8 -> IO ()
sysviaWriteIC32 theVia val = do
    --ic32Val <- readIORef (ic32 (sysViaOnly theVia))
    --let oldIc32Val = ic32Val

    if (val .&. 8) /= 0
        then modifyIORef' (ic32 (sysviaOnly theVia)) (.|. (1 `shiftL` fromIntegral (val .&. 7)))
        else modifyIORef' (ic32 (sysviaOnly theVia)) (.&. (1 `shiftL` fromIntegral (val .&. 7)))
    sysviaUpdateSdb theVia

    -- sn_write (sound) would go here
    -- as would scrsize

keyUpdate :: Via -> IO ()
keyUpdate theVia = do
    -- Assume always autoscan (for now)
    kbMatr <- readIORef (keyboardMatrix theVia)
    if IBVector.all not kbMatr then
        viaSetCa2 theVia 1
    else
        viaSetCa2 theVia 0



viaSetCa2 :: Via -> Word8 -> IO ()
viaSetCa2 theVia level = do
    ca2Val <- readIORef (ca2 theVia)
    pcrVal <- readIORef (pcr theVia)

    let ca2Unchanged = level == fromIntegral ca2Val
        outputMode = pcrVal .&. 0x08 /= 0
        idkWhat = (((pcrVal .&. 0x04) /= 0) && level /= 0)
                  || (pcrVal .&. 0x04 == 0) && (level == 0)


    unless (ca2Unchanged || outputMode ) $ do
        when idkWhat $ do
            modifyIORef' (ifr theVia) (.|. intCA2)
            viaUpdateIFR theVia

        writeIORef (ca2 theVia) (fromIntegral level)

viaSetCb2 :: Via -> Word8 -> IO ()
viaSetCb2 theVia level = do
    cb2Val <- readIORef (cb2 theVia)
    pcrVal <- readIORef (pcr theVia)

    let cb2Unchanged = level == fromIntegral cb2Val
        outputMode = pcrVal .&. 0x80 /= 0
        idkWhat = (((pcrVal .&. 0x40) /= 0) && level /= 0)
                  || (pcrVal .&. 0x40 == 0) && (level == 0)


    unless (cb2Unchanged || outputMode ) $ do
        when idkWhat $ do
            modifyIORef' (ifr theVia) (.|. intCB2)
            viaUpdateIFR theVia

        writeIORef (cb2 theVia) (fromIntegral level)

readVia :: Via -> Word16 -> IO Word8
readVia theVia 0xFE41 = do                                      --ORA
    modifyIORef' (ifr theVia) (.&. complement intCA1)
    pcrVal <- readIORef (pcr theVia)
    when ((pcrVal .&. 0x0A) /= 0x02) $
        modifyIORef' (ifr theVia) (.&. complement intCA2)
    viaUpdateIFR theVia
    doORAnhRead theVia
readVia theVia 0xFE4F = doORAnhRead theVia                      --ORAnh
readVia _ 0xFE40 = do                                           --ORB
    putStrLn "ORB read"
    return 0xFF
    -- modifyIORef' (ifr theVia) (.&. complement intCB1)
    -- pcrVal <- readIORef (pcr theVia)
    -- when ((pcrVal .&. 0xA0) /= 0x20) $ --Not independent interrupt for CB2
    --     modifyIORef' (ifr theVia) (.&. complement intCB2)
    -- viaUpdateIFR theVia

    -- orbVal <- readIORef (orb theVia)
    -- ddrbVal <- readIORef (ddrb theVia)
    -- let temp0 = orbVal .&. ddrbVal

    -- acrVal <- readIORef (acr theVia)
    -- irbVal <- readIORef (irb theVia)

    -- if (acrVal .&. 0x02) /= 0 then do

    --     return $ temp0 .|. (irbVal .&. complement ddrbVal)
    -- else do
    --     portB <- readPortB theVia
    --     let t = temp0 .|. (sysviaReadPortB .&. complement ddrbVal)
    --     if (acrVal .&. 0x80) /= 0 then do
    --         t1pb7Val <- readIORef (t1pb7 theVia)
    --         return $ (t .&. 0x7F) .|. t1pb7Val
    --     else
    --         return t

readVia theVia 0xFE43 = readIORef (ddra theVia)                 --DDRA
readVia theVia 0xFE42 = readIORef (ddrb theVia)                 --DDRB

readVia theVia 0xFE46 = do                                      --T1 low-order latches
    t1lVal <- readIORef (t1_latch theVia)
    return $ fromIntegral $ (t1lVal .&. 0x01FE) `shiftR` 1
readVia theVia 0xFE47 = do                                      --T1 high-order latches
    t1lVal <- readIORef (t1_latch theVia)
    return $ fromIntegral $ t1lVal `shiftR` 9

readVia theVia 0xFE44 = do                                      --T1 low-order counter
    -- Clear Timer 1 interrupt
    modifyIORef' (ifr theVia) (.&. complement intTimer1)
    viaUpdateIFR theVia

    t1cVal <- readIORef (t1_counter theVia)

    if t1cVal < (-1)
        then return 0xFF -- If counter is negative during reload, return 0xFF
        else return $ fromIntegral (((t1cVal + 1) `shiftR` 1) .&. 0xFF)

readVia theVia 0xFE45 = do                                      --T1 high-order counter
    t1cVal <- readIORef (t1_counter theVia)
    if t1cVal < (-1)
        then return 0xFF
        else return $ fromIntegral ((t1cVal + 1) `shiftR` 9)

readVia theVia 0xFE48 = do                                      --T2 low-order latches
    modifyIORef' (ifr theVia) (.&. complement intTimer2)
    viaUpdateIFR theVia
    t2cVal <- readIORef (t2_counter theVia)
    return $ fromIntegral (((t2cVal + 1) `shiftR` 1) .&. 0xFF)

readVia theVia 0xFE49 = do                                      --T2 high order counter 
    t2cVal <- readIORef (t2_counter theVia)
    return $ fromIntegral ((t2cVal + 1) `shiftR` 9)

readVia theVia 0xFE4A = readIORef (sr theVia)                   --SR
readVia theVia 0xFE4B = readIORef (acr theVia)                  --ACR
readVia theVia 0xFE4C = readIORef (pcr theVia)                  --PCR
readVia theVia 0xFE4E = (.|. 0x80) <$> readIORef (ier theVia)   --IER
readVia theVia 0xFE4D = readIORef (ifr theVia)                  --IFR
readVia _ _ = putStrLn "Unexpected Via Address" >> return 0xFE

doORAnhRead :: Via -> IO Word8
doORAnhRead theVia = do
    oraVal <- readIORef (ora theVia)
    ddraVal <- readIORef (ddra theVia)
    let temp = oraVal .&. ddraVal

    acrVal <- readIORef (acr theVia)
    iraVal <- readIORef (ira theVia)

    if (acrVal .&. 1) /= 0 then
        return $ temp .|. (iraVal .&. complement ddraVal)
    else do
        pA <- sysviaReadPortA theVia
        return $ temp .|. (pA .&. ddraVal)

doORAnhWrite :: Via -> Word8 -> IO()
doORAnhWrite theVia val = do
    ddraVal <- readIORef (ddra theVia)
    --write only output pins, leave inputs high
    sysviaWritePortA theVia ((val .&. ddraVal) .|. complement ddraVal)

    writeIORef (ora theVia) val

writeVia :: Via -> Word16 -> Word8 -> IO ()
writeVia theVia 0xFE41 val = do                                 --ORA
    modifyIORef' (ifr theVia) (.&. complement intCA1)

    pcrVal <- readIORef (pcr theVia)
    when ((pcrVal .&. 0x0A) /= 0x02) $ --Not independent interrupt for CA2
        modifyIORef' (ifr theVia) (.&. complement intCA2)
    viaUpdateIFR theVia

    if (pcrVal .&. 0x0E) == 0x08 then do --handshake mode
        viaSetCa2 theVia 0
        writeIORef (ca2 theVia) 0
    else when ((pcrVal .&. 0x0E) == 0x0A) $ do --pulse mode
        viaSetCa2 theVia 0 --idkWhy
        viaSetCa2 theVia 1
        writeIORef (ca2 theVia) 1 --not acually nessesary
    doORAnhWrite theVia val

writeVia theVia 0xFE4F val = doORAnhWrite theVia val            --ORAnh
writeVia theVia 0xFE40 val = do                                 --ORB
    modifyIORef' (ifr theVia) (.&. complement intCB1)

    pcrVal <- readIORef (pcr theVia)
    when ((pcrVal .&. 0xA0) /= 0x20) $ --Not independent interrupt for CB2
        modifyIORef' (ifr theVia) (.&. complement intCA2)
    viaUpdateIFR theVia

    writeIORef (orb theVia) val
    ddrbVal <- readIORef (ddrb theVia)
    let newval1 = (val .&. ddrbVal) .|. complement ddrbVal

    acrVal <- readIORef (acr theVia)
    t1pb7Val <- readIORef (t1pb7 theVia)
    let newVal2 = if acrVal .&. 0x80 /= 0
        then (newval1 .&. 0x8F) .|. t1pb7Val
        else newval1
    sysviaWritePortB theVia newVal2

    if (pcrVal .&. 0xE0) == 0x80 then do --Handshake mode
        viaSetCb2 theVia 0
        writeIORef (cb2 theVia) 0
    else when ((pcrVal .&. 0xE0) == 0xA0) $ do --pulse mode
        viaSetCb2 theVia 0 --idkWhy
        viaSetCb2 theVia 1
        writeIORef (cb2 theVia) 1 --not acually nessesary

writeVia theVia 0xFE43 val = do                                 --DDRA
    writeIORef (ddra theVia) val

    oraRef <- readIORef (ora theVia)
    let ddraRef = val
    sysviaWritePortA theVia ((oraRef .&. ddraRef) .|. complement ddraRef)

writeVia theVia 0xFE42 val = do                                 --DDRB
    writeIORef (ddrb theVia) val

    orbVal <- readIORef (ddrb theVia)
    acrVal <- readIORef (acr theVia)
    t1pb7Val <- readIORef (t1pb7 theVia)
    let newVal = (orbVal .&. val) .|. complement val
        newVal2 = if (acrVal .&. 0x80) /= 0
            then (val .&. 0x8F) .|. t1pb7Val
            else newVal
    sysviaWritePortB theVia newVal2

writeVia theVia 0xFE4b val = writeIORef (acr theVia) val        --ACR
writeVia theVia 0xFE4C val = do                                 --PCR
    writeIORef (pcr theVia) val

    if (val .&. 0x0E) == 0x0C then do
        viaSetCa2 theVia 0
        writeIORef (ca2 theVia) 0
    else when ((val .&. 0x08) /= 0) $ do
        viaSetCa2 theVia 1
        writeIORef (ca2 theVia) 1

    if (val .&. 0xE0) == 0xC0 then do
        viaSetCb2 theVia 0
        writeIORef (cb2 theVia) 0
    else when ((val .&. 0x80) /= 0) $ do
        viaSetCb2 theVia 1
        writeIORef (cb2 theVia) 1

writeVia theVia 0xFE4A val = do                                 --SR
    writeIORef (sr theVia) val
    writeIORef (srCount theVia) 16
    modifyIORef' (ifr theVia) (.&. complement 0x04)

writeVia theVia 0xFE46 val = do                                 --T1LL
    modifyIORef' (t1_latch theVia) $ \t1l ->
        (t1l .&. 0x1FE00) .|. ((fromIntegral val .&. 0xFF) `shiftL` 1)
writeVia theVia 0xFE44 val = do                                 --T1CL
    modifyIORef' (t1_latch theVia) $ \t1l ->
        (t1l .&. 0x1FE00) .|. ((fromIntegral val .&. 0xFF) `shiftL` 1)

writeVia theVia 0xFE47 val = do                                 --T1LH
    modifyIORef' (t1_latch theVia) $ \t1l ->
        (t1l .&. 0x1FE) .|. (fromIntegral val `shiftL` 9)

    modifyIORef' (ifr theVia) (.&. complement intTimer1)
    viaUpdateIFR theVia

writeVia theVia 0xFE45 val = do                                 --T1CH
    acrVal <- readIORef (acr theVia)

    when ((acrVal .&. 0xC0) == 0x80) $
        writeIORef (t1pb7 theVia) 0

    modifyIORef' (t1_latch theVia) $ \t1l ->
        (t1l .&. 0x1FE) .|. (fromIntegral val `shiftL` 9)

    t1lVal <- readIORef (t1_latch theVia)
    writeIORef (t1_counter theVia) (fromIntegral t1lVal + 1) --TODO figure out if should be int or what

    writeIORef (t1_hit theVia) 0 --TODO fix

    modifyIORef' (ifr theVia) (.&. complement intTimer1)
    viaUpdateIFR theVia

writeVia theVia 0xFE48 val =                                    --T1CL
    modifyIORef' (t2_latch theVia) $ \t2l -> (t2l .&. 0x1FE00) .|. (fromIntegral val `shiftL` 1)

writeVia theVia 0xFE49 val = do                                 --T2CH
    modifyIORef' (t2_latch theVia) $ \t2l -> (t2l .&. 0x01FE) .|. (fromIntegral val `shiftL` 9)

    acrVal <- readIORef (acr theVia)
    t2lVal <- readIORef (t2_latch theVia)
    let newT2cVal = if (acrVal .&. 0x20) /= 0 then t2lVal else t2lVal + 1
    writeIORef (t2_counter theVia) (fromIntegral newT2cVal)

    modifyIORef' (ifr theVia) (.&. complement intTimer2)
    viaUpdateIFR theVia
    writeIORef (t2_hit theVia) 0

writeVia theVia 0xFE4E val = do                                 --IER
    if (val .&. 0x80) /= 0 then
        modifyIORef' (ier theVia) (.|. (val .&. 0x7F))
    else
        modifyIORef' (ier theVia) (.&. complement (val .&. 0x7F))
    viaUpdateIFR theVia

writeVia theVia 0xFE4D val = do                                 --IFR
    modifyIORef' (ifr theVia) (.&. complement (val .&. 0x7F))
    viaUpdateIFR theVia

writeVia _ _ _ = putStrLn "Unexpected Via Address"