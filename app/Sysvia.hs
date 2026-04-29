module Sysvia where

import Via (Via (pcr, ifr, ier, ddrb, ddra, timer1c, timer1l, orb, irb, timer2c, sr, acr, srMode, ora, timer1HasShot, timer2HasShot, timer2l, ca2, cb2), initVia)
import ViaConstants

import Data.Word (Word8, Word16)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef', writeIORef)
import qualified Data.Vector as IBVector
import Data.Bits ((.|.), (.&.), shiftL, shiftR, Bits (clearBit, complement, setBit, xor))
import Control.Monad (when, unless)
import Data.Foldable (forM_)

kbdMatrixRows, kbdMatrixCols :: Int
kbdMatrixRows = 8
kbdMatrixCols = 10

data Sysvia = Sysvia {via :: Via, srTrigger :: IORef Int, sdbVal :: IORef Word8,
                      kbdRow :: IORef Int, kbdCol :: IORef Int, keyboardMatrix :: IORef (IBVector.Vector Bool), keyboardLinks :: IORef Word8,
                      ic32State :: IORef Word8,
                      intStatus :: IORef Word8, previousIntStatus :: IORef Word8, irqPendingFlag :: IORef Bool} --doesnt belong here

initSysvia :: IO Sysvia
initSysvia = do
    viaRef              <- initVia
    srTriggerRef        <- newIORef 0 --we dont need this one maybe???
    sdbValRef           <- newIORef 0
    kbdRowRef           <- newIORef 0
    kbdColRef           <- newIORef 0
    kMatrRef            <- newIORef (IBVector.replicate (kbdMatrixRows * kbdMatrixCols) False)
    keyboardLinksRef    <- newIORef 0
    ic32StateRef        <- newIORef 0
    intStatusRef        <- newIORef 0 --not specific to the sysvia (shared across multiple)
    previousIntStatusRef <- newIORef 0
    irqPendingFlagRef <- newIORef False --also not sysvia specific
    return (Sysvia viaRef srTriggerRef sdbValRef kbdRowRef kbdColRef kMatrRef keyboardLinksRef ic32StateRef intStatusRef previousIntStatusRef irqPendingFlagRef)

-- via inside sysvia helper functions
readViaF :: Sysvia -> (Via -> IORef a) -> IO a
readViaF sv fieldGetter = readIORef (fieldGetter (via sv))
writeViaF :: Sysvia -> (Via -> IORef a) -> a -> IO ()
writeViaF sv fieldSetter = writeIORef (fieldSetter (via sv))
modifyViaF :: Sysvia -> (Via -> IORef a) -> (a -> a) -> IO ()
modifyViaF sv field = modifyIORef' (field (via sv))

-- via update helper functions
updateIFRTopBit :: Sysvia -> IO ()
updateIFRTopBit svia = do
    ifrVal <- readViaF svia ifr
    ierVal <- readViaF svia ier
    if (ifrVal .&. (ierVal .&. 0x7F)) /=0 then do
        modifyViaF svia ifr (.|. ifr_irq)
        modifyIORef' (intStatus svia) (`setBit` 0)
    else do
        modifyViaF svia ifr (.&. complement ifr_irq)
        modifyIORef' (intStatus svia) (`clearBit` 0)

printKeyboardMatrix :: Sysvia -> IO ()
printKeyboardMatrix svia = do
    kb <- readIORef (keyboardMatrix svia)
    let lst = IBVector.toList kb
    forM_ (zip [1..] lst) $ \(i, val) -> do
        putStr (if val then "1," else "0,")
        when (i `mod` kbdMatrixCols == 0) $
            putStrLn ""
    putStrLn "_______________________________"

doKbdIntCheck :: Sysvia -> IO ()
doKbdIntCheck svia = do
    kbdMatrVal <- readIORef (keyboardMatrix svia)
    let anyKeyPressed = IBVector.any id kbdMatrVal

    pcrVal <- readViaF svia pcr

    when (anyKeyPressed && (pcrVal .&. 0x0C) == 4) $ do
        ic32StateVal <- readIORef (ic32State svia)

        if (ic32StateVal .&. ic32_keyboard_write) /=0 then do -- autoscan mode
            modifyViaF svia ifr (.|. ifr_ca2)
            updateIFRTopBit svia
        else do                                               -- scan specific key mode
            kbdColVal <- readIORef (kbdCol svia)
            when (kbdColVal < kbdMatrixCols) $ do
                when (keyPressedInColumn kbdMatrVal kbdColVal) $ do

                    modifyViaF svia ifr (.|. ifr_ca2)
                    updateIFRTopBit svia
    where
        keyPressedInColumn :: IBVector.Vector Bool -> Int -> Bool
        keyPressedInColumn kbd col = do
            any (\row -> kbd IBVector.! (row * kbdMatrixCols + col)) [0..7]


kbdOP :: Sysvia -> IO Bool
kbdOP svia = do
    kbdColVal <- readIORef (kbdCol svia)
    kbdRowVal <- readIORef (kbdRow svia)
    kbdMatrVal <- readIORef (keyboardMatrix svia)

    if kbdColVal > (kbdMatrixCols-1) || kbdRowVal > (kbdMatrixRows-1) then-- range check
        return False
    else
        return $ kbdMatrVal IBVector.! (kbdRowVal * kbdMatrixCols + kbdColVal)

ic32Write :: Sysvia -> Word8 -> IO ()
ic32Write svia val = do
    --not including the sound code that should go in this function
    -- or the caps light

    prevIc32StateVal <- readIORef (ic32State svia)

    let (bit :: Int) = fromIntegral val .&. 7
    if (val .&. 8) /=0 then
        modifyIORef' (ic32State svia) (.|. (1 `shiftL` bit))
    else
        modifyIORef' (ic32State svia) (.&. complement (1 `shiftL` bit))

    ic32StateVal <- readIORef (ic32State svia)

    when (((ic32StateVal .&. ic32_keyboard_write) == 0) && ((prevIc32StateVal .&. ic32_keyboard_write) /=0)) $ do
        sdbValVal <- readIORef (sdbVal svia)
        writeIORef (kbdRow svia) ((fromIntegral sdbValVal `shiftR` 4) .&. 7)
        writeIORef (kbdCol svia) (fromIntegral sdbValVal .&. 0x0F)
        doKbdIntCheck svia

slowDataBusWrite :: Sysvia -> Word8 -> IO ()
slowDataBusWrite svia val = do
    writeIORef (sdbVal svia) val

    ic32StateVal <- readIORef (ic32State svia)
    when ((ic32StateVal .&. ic32_keyboard_write) == 0) $ do
        writeIORef (kbdRow svia) (fromIntegral ((val `shiftR` 4) .&. 7))
        writeIORef (kbdCol svia) (fromIntegral (val .&. 0x0F))
        doKbdIntCheck svia
    --sound code go here

slowDataBusRead :: Sysvia -> IO Word8
slowDataBusRead svia = do
    oraVal <- readViaF svia ora
    ddraVal <- readViaF svia ddra
    let result = oraVal .&. ddraVal

    ic32StateVal <- readIORef (ic32State svia)

    kbdOPVal <- kbdOP svia
    if ((ic32StateVal .&. ic32_keyboard_write) == 0) && kbdOPVal
        then return $ result .|. 128
        else return result

--WRITE SYSVIA
writeSysvia :: Sysvia -> Word16 -> Word8 -> IO ()
writeSysvia svia 0xFE40 val = do                        --ORB
    writeViaF svia orb val
    ic32Write svia val

    ifrVal <- readViaF svia ifr
    pcrVal <- readViaF svia pcr

    when (((ifrVal .&. ifr_cb2) /=0) && ((pcrVal .&. 0x20) == 0)) $
        modifyViaF svia ifr (.&. complement ifr_cb2)
    modifyViaF svia ifr (.&. complement ifr_cb1)
    updateIFRTopBit svia
writeSysvia svia 0xFE41 val = do                        --ORA
    writeViaF svia ora val
    modifyViaF svia ifr (.&. complement (ifr_ca2 .|. ifr_ca1))
    updateIFRTopBit svia
    slowDataBusWrite svia val
writeSysvia svia 0xFE42 val = writeViaF svia ddrb val   --DDRB
writeSysvia svia 0xFE43 val = writeViaF svia ddra val   --DDRA
writeSysvia svia 0xFE44 val = do                        --Timer
    modifyViaF svia timer1l $ \old ->
                (old .&. 0xFF00) .|. fromIntegral val
writeSysvia svia 0xFE46 val = do                        --Timer
    modifyViaF svia timer1l $ \old ->
                (old .&. 0xFF00) .|. fromIntegral val

writeSysvia svia 0xFE45 val = do                        --Timer
    modifyViaF svia timer1l $ \old ->
                (old .&. 0xFF) .|. (fromIntegral val `shiftL` 8)
    newtimer1lVal <- readViaF svia timer1l
    writeViaF svia timer1c (newtimer1lVal * 2 + 1)

    acrVal <- readViaF svia acr
    when ((acrVal .&. acr_timer1_output_enable) /=0) $ do
        modifyViaF svia orb (.&. 0x7F)
        modifyViaF svia irb (.&. 0x7F)
    modifyViaF svia ifr (.&. complement ifr_timer1)
    updateIFRTopBit svia
    writeViaF svia timer1HasShot False

writeSysvia svia 0xFE47 val = do                        --Timer
    modifyViaF svia timer1l $ \old ->
                (old .&. 0xFF) .|. (fromIntegral val `shiftL` 8)

    modifyViaF svia ifr (.&. complement ifr_timer1)
    updateIFRTopBit svia

writeSysvia svia 0xFE48 val = do                        --Timer
    modifyViaF svia timer2l $ \old ->
                (old .&. 0xFF00) .|. fromIntegral val

writeSysvia svia 0xFE49 val = do                        --Timer
    modifyViaF svia timer2l $ \old ->
                (old .&. 0xFF) .|. (fromIntegral val `shiftL` 8)
    newtimer2lVal <- readViaF svia timer2l
    writeViaF svia timer2c (newtimer2lVal * 2 + 1)

    newTimer2cVal <- readViaF svia timer2c
    when (newTimer2cVal == 0) $
        writeViaF svia timer2c 0x20000

    modifyViaF svia ifr (.&. complement ifr_timer2)
    updateIFRTopBit svia
    writeViaF svia timer2HasShot False

writeSysvia svia 0xFE4A val = do                        --SR
    writeViaF svia sr val
    updateSRState svia True

writeSysvia svia 0xFE4B val = do                        --acr
    writeViaF svia acr val
    updateSRState svia False

writeSysvia svia 0xFE4C val = do                        --pcr
    writeViaF svia pcr val

    if (val .&. pcr_ca2_control) == pcr_ca2_output_high then
        writeViaF svia ca2 True

    else when ((val .&. pcr_ca2_control) == pcr_ca2_output_low) $
        writeViaF svia ca2 False

    if (val .&. pcr_cb2_control) == pcr_cb2_output_high then
        --cb2Val <- readViaF svia cb2
        -- unless cb2Val $
        --     -- Light pen?
        writeViaF svia cb2 True

    else when ((val .&. pcr_cb2_control) == pcr_cb2_output_low) $
        writeViaF svia cb2 False
writeSysvia svia 0xFE4D val = do                        --IFR
    modifyViaF svia ifr (.&. complement val)
    updateIFRTopBit svia
writeSysvia svia 0xFE4E val = do                        --IER
    if (val .&. 0x80) /=0 then
        modifyViaF svia ier (.|. val)
    else
        modifyViaF svia ier (.&. complement val)
    modifyViaF svia ier (.&. complement ier_set_clear)
    updateIFRTopBit svia

writeSysvia svia 0xFE4F val = do                        --ORAnh
    writeViaF svia ora val
    slowDataBusWrite svia val

writeSysvia _ addr _ = putStrLn ("write sysvia Unknown address" ++ show addr)

--READ SYSVIA
readSysvia :: Sysvia -> Word16 -> IO Word8
readSysvia svia 0xFE40 = do                     --ORB
    orbVal <- readViaF svia orb
    ddrbVal <- readViaF svia ddrb

    modifyViaF svia ifr (.&. complement ifr_cb1)
    updateIFRTopBit svia

    return $ (orbVal .&. ddrbVal) .|. 0xC0
readSysvia svia 0xFE42 = readViaF svia ddrb     --DDRB
readSysvia svia 0xFE43 = readViaF svia ddra     --DDRA
readSysvia svia 0xFE44 = do                     --Timer 1 lo counter
    timer1cVal <- readViaF svia timer1c
    let temp = if timer1cVal < 0 then
                    0xFF
               else
                    (timer1cVal `div` 2) .&. 0xFF
    modifyViaF svia ifr (.&. complement ifr_timer1)
    updateIFRTopBit svia
    return $ fromIntegral temp

readSysvia svia 0xFE45 = do                     --Timer 1 hi counter
    timer1cVal <- readViaF svia timer1c
    return $ fromIntegral $ (timer1cVal `shiftR` 9) .&. 0xFF
readSysvia svia 0xFE46 = do                     --Timer 1 lo latch
    timer1lVal <- readViaF svia timer1l
    return $ fromIntegral timer1lVal
readSysvia svia 0xFE47 = do                     --Timer 1 hi latch
    timer1lVal <- readViaF svia timer1l
    return $ fromIntegral (timer1lVal `shiftR` 8)
readSysvia svia 0xFE48 = do                     --Timer 2 lo counter
    timer2cVal <- readViaF svia timer2c

    modifyViaF svia ifr (.&. complement ifr_timer2)
    updateIFRTopBit svia
    return $ if timer2cVal < 0 then
                fromIntegral $ ((timer2cVal - 1) `div` 2) .&. 0xFF
             else
                fromIntegral $ (timer2cVal `div` 2) .&. 0xFF

readSysvia svia 0xFE49 = do                     --Timer 2 hi counter
    timer2cVal <- readViaF svia timer2c
    return $ fromIntegral $ timer2cVal `shiftR` 9

readSysvia svia 0xFE4A = do                     --SR
    srVal <- readViaF svia sr
    updateSRState svia True
    return srVal
readSysvia svia 0xFE4B = readViaF svia acr      --ACR
readSysvia svia 0xFE4C = readViaF svia pcr      --PCR
readSysvia svia 0xFE4D = do                     --IFR
    updateIFRTopBit svia
    readViaF svia ifr
readSysvia svia 0xFE4E = do                     --IER
    ierVal <- readViaF svia ifr
    return $ ierVal .|. ier_set_clear
readSysvia svia 0xFE41 = do                     --ORA
    modifyViaF svia ifr (.&. complement (ifr_ca2 .|. ifr_ca1))
    updateIFRTopBit svia
    slowDataBusRead svia
readSysvia svia 0xFE4F = do                     --ORAnh
    slowDataBusRead svia

readSysvia _ addr = putStrLn ("read sysvia Unknown address" ++ show addr) >> return 0



--other
updateSRState :: Sysvia -> Bool -> IO ()
updateSRState svia srRW = do
    acrVal <- readViaF svia acr

    let mode = fromIntegral $ (acrVal `shiftR` 2) .&. 0x07
    writeViaF svia srMode mode

    srTriggerVal <- readIORef (srTrigger svia)
    when ((mode == 2 || mode == 6) && srTriggerVal == maxBound) $
        writeIORef (srTrigger svia) 32 -- check this part, currently unused


    when srRW $ do
        ifrVal <- readViaF svia ifr
        when (ifrVal .&. ifr_shiftreg /=0) $ do
            writeIORef (ifr (via svia)) (ifrVal .&. complement ifr_shiftreg)
            updateIFRTopBit svia

sysviaPollReal :: Sysvia -> IO ()
sysviaPollReal svia = do
    -- simplified
    timer1cVal <- readViaF svia timer1c

    acrVal <- readViaF svia acr
    when (timer1cVal <= (-2)) $ do
        -- || ((acrVal .&. acr_timer1_continuous) /=0)) $ do (one shot or continuous)

        --idk what
        modifyViaF svia ifr (.|. ifr_timer1)
        updateIFRTopBit svia

        when ((acrVal .&. acr_timer1_output_enable) /=0) $ do
            modifyViaF svia orb (`xor` 0x80)
            modifyViaF svia irb (`xor` 0x80)

        -- trigger irq
        ierVal <- readViaF svia ier
        when ((ierVal .&. ier_timer1) /=0) $ do
            writeIORef (irqPendingFlag svia) True
        -- reset timer1c
        timer1lVal <- readViaF svia timer1l
        modifyViaF svia timer1c (+ ((timer1lVal * 2) + 4))


        --timer1cVal2 <- readViaF svia timer1c

    -- do timer2c as well

sysviaPoll :: Sysvia -> Int -> IO ()
sysviaPoll svia cyclesPassed = do
    modifyViaF svia timer1c (\x -> x - cyclesPassed) --decr timer1c

    acrVal <- readViaF svia acr
    when ((acrVal .&. acr_timer2_control) /=0) $
        modifyViaF svia timer2c (\x -> x - cyclesPassed) -- conditionally decr timer2c

    timer1cVal <- readViaF svia timer1c
    timer2cVal <- readViaF svia timer2c
    when ((timer1cVal < 0) || (timer2cVal < 0)) $
        sysviaPollReal svia
    -- Todo Shift Register
    -- customSRTriggerVal <- readIORef (customSRTrigger svia)
    -- when (customSRTrigger <= 0) $
    --     SRPoll svia

    doKbdIntCheck svia


--doesnt really belong in this file not sysvia specific 
-- nor does intStatus belong to the sysvia
doInterruptCheck :: Sysvia -> IO ()
doInterruptCheck svia = do
    irqPendingFlagVal <- readIORef (irqPendingFlag svia)



    intStatusVal <- readIORef (intStatus svia)

    unless irqPendingFlagVal $ do
        previousIntStatusVal <- readIORef (previousIntStatus svia)

        let irqPending = (intStatusVal /= 0) && previousIntStatusVal == 0 --rising edge detector
        when irqPending $ do
            writeIORef (irqPendingFlag svia) irqPending
    writeIORef (previousIntStatus svia) intStatusVal



