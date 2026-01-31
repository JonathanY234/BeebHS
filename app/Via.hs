module Via where

import ViaConstants ( ier_set_clear )

import Data.Word (Word8)
import Data.IORef (IORef, newIORef)

data Via = Via {ora :: IORef Word8, orb :: IORef Word8, ira :: IORef Word8, irb :: IORef Word8,
                ddra :: IORef Word8, ddrb :: IORef Word8,
                acr :: IORef Word8, pcr :: IORef Word8, ifr :: IORef Word8, ier :: IORef Word8,
                timer1c :: IORef Int, timer2c :: IORef Int, timer1l :: IORef Int, timer2l :: IORef Int,
                timer1HasShot :: IORef Bool, timer2HasShot :: IORef Bool, timer1Adjust :: IORef Int, timer2Adjust :: IORef Int,
                sr :: IORef Word8,
                ca2 :: IORef Bool, cb2 :: IORef Bool,-- cb1 :: IORef Int, cb2 :: IORef Int,
                srMode :: IORef Int}

initVia :: IO Via
initVia = do
    oraRef           <- newIORef 0xFF
    orbRef           <- newIORef 0xFF
    iraRef           <- newIORef 0x00
    irbRef           <- newIORef 0x00
    ddraRef          <- newIORef 0
    ddrbRef          <- newIORef 0
    acrRef           <- newIORef 0
    pcrRef           <- newIORef 0
    ifrRef           <- newIORef 0
    ierRef           <- newIORef ier_set_clear
    timer1cRef       <- newIORef 0
    timer1lRef       <- newIORef 0xFFFF
    timer2cRef       <- newIORef 0
    timer2lRef       <- newIORef 0xFFFF
    timer1HasShotRef <- newIORef False
    timer2HasShotRef <- newIORef False
    timer1Adjust     <- newIORef 0
    timer2Adjust     <- newIORef 0
    srRef            <- newIORef 0
    ca2Ref           <- newIORef False
    cb2Ref           <- newIORef False
    srModeRef        <- newIORef 0
    --sysviaRef      <- initSysviaOnly
    -- kMatr  <- newIORef (IBVector.replicate (kbMatrixRows * kbMatrixCols) False)

    return (Via oraRef orbRef iraRef irbRef ddraRef ddrbRef
             acrRef pcrRef ifrRef ierRef
             timer1cRef timer2cRef timer1lRef timer2lRef      
             timer1HasShotRef timer2HasShotRef timer1Adjust timer2Adjust
             srRef ca2Ref cb2Ref srModeRef)

     
    