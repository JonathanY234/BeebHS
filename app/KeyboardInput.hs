module KeyboardInput where

import Sysvia (kbdMatrixRows, keyboardMatrix, kbdMatrixCols)
import MemoryRegisters (Memory, sysvia)

import Data.IORef (readIORef, writeIORef)
import Data.Vector qualified as IBVector

import SDL qualified
import Control.Monad (forM_, when)

--       0x00      0x01  0x02  0x03 0x04 0x05 0x06 0x07 0x08 0x09    
-- 0x00  Shift     Ctrl  <------- starup up DIP swicthes ------->  
-- 0x10  Q         3     4     5    f4   8    f7   =-   ~^   Left    
-- 0x20  f0        W     E     T    7    I    9    0    £    Down    
-- 0x30  1         2     D     R    6    U    O    P    [{   Up      
-- 0x40  CapsLck   A     X     F    Y    J    K    @    :*   Return  
-- 0x50  ShiftLck  S     C     G    H    N    L    ;+   ]}   Delete  
-- 0x60  Tab       Z     SPC   V    B    M    <,   >.   /?   Copy    
-- 0x70  ESC       f1    f2    f3   f5   f6   f8   f9   \    Right   

keyMapping :: [(SDL.Scancode, (Int, Int))]
keyMapping =
    [   (SDL.ScancodeLShift, (0, 0)),
        (SDL.ScancodeLCtrl, (0, 1)),

        (SDL.ScancodeQ, (1, 0)),
        (SDL.Scancode3, (1, 1)),
        (SDL.Scancode4, (1, 2)),
        (SDL.Scancode5, (1, 3)),
        (SDL.ScancodeF4, (1, 4)),
        (SDL.Scancode8, (1, 5)),
        (SDL.ScancodeF7, (1, 6)),
        (SDL.ScancodeEquals, (1, 7)),
        -- (SDL.ScancodeTilda, (1, 8)),
        (SDL.ScancodeLeft, (1, 9)),
        (SDL.ScancodeF10, (2, 0)), -- should be F0
        (SDL.ScancodeW, (2, 1)),
        (SDL.ScancodeE, (2, 2)),
        (SDL.ScancodeT, (2, 3)),
        (SDL.Scancode7, (2, 4)),
        (SDL.ScancodeI, (2, 5)),
        (SDL.Scancode9, (2, 6)),
        (SDL.Scancode0, (2, 7)),
        -- (SDL.Scancode£, (2, 8)),
        (SDL.ScancodeDown, (2, 9)),
        (SDL.Scancode1, (3, 0)),
        (SDL.Scancode2, (3, 1)),
        (SDL.ScancodeD, (3, 2)),
        (SDL.ScancodeR, (3, 3)),
        (SDL.Scancode6, (3, 4)),
        (SDL.ScancodeU, (3, 5)),
        (SDL.ScancodeO, (3, 6)),
        (SDL.ScancodeP, (3, 7)),
        (SDL.ScancodeLeftBracket, (3, 8)),
        (SDL.ScancodeReturn, (3, 9)),
        (SDL.ScancodeCapsLock, (4, 0)),
        (SDL.ScancodeA, (4, 1)),
        (SDL.ScancodeX, (4, 2)),
        (SDL.ScancodeF, (4, 3)),
        (SDL.ScancodeY, (4, 4)),
        (SDL.ScancodeJ, (4, 5)),
        (SDL.ScancodeK, (4, 6)),
        -- , (SDL.Scancode@,  (4, 7))
        -- , (SDL.ScancodeColon,  (4, 8))
        (SDL.ScancodeReturn, (4, 9)),
        (SDL.ScancodeLShift, (5, 0)),
        (SDL.ScancodeS, (5, 1)),
        (SDL.ScancodeC, (5, 2)),
        (SDL.ScancodeG, (5, 3)),
        (SDL.ScancodeH, (5, 4)),
        (SDL.ScancodeN, (5, 5)),
        (SDL.ScancodeL, (5, 6)),
        (SDL.ScancodeSemicolon,  (5, 7)),
        (SDL.ScancodeRightBracket,  (5, 8)),
        (SDL.ScancodeBackspace, (5, 9)),

        (SDL.ScancodeTab, (6, 0)),
        (SDL.ScancodeZ, (6, 1)),
        (SDL.ScancodeSpace, (6, 2)),
        (SDL.ScancodeV, (6, 3)),
        (SDL.ScancodeB, (6, 4)),
        (SDL.ScancodeM, (6, 5)),
        (SDL.ScancodeComma, (6, 6)), -- This thing <
        (SDL.ScancodeStop,  (6, 7)), -- >
        (SDL.ScancodeSlash,  (6, 8)),
        (SDL.ScancodeCopy, (6, 9)),

        (SDL.ScancodeEscape, (7, 0)),
        (SDL.ScancodeF1, (7, 1)),
        (SDL.ScancodeF2, (7, 2)),
        (SDL.ScancodeF3, (7, 3)),
        (SDL.ScancodeF5, (7, 4)),
        (SDL.ScancodeF6, (7, 5)),
        (SDL.ScancodeF8, (7, 6)),
        (SDL.ScancodeF9,  (7, 7)),
        (SDL.ScancodeBackslash,  (7, 8)),
        (SDL.ScancodeRight, (7, 9))
    ]

updateKeyboardMatrix :: Memory -> IO ()
updateKeyboardMatrix mem = do
    keyStates <- SDL.getKeyboardState

    let updateKey :: IBVector.Vector Bool -> (SDL.Scancode, (Int, Int)) -> IBVector.Vector Bool
        updateKey km (sc, (row, col)) =
            let idx = row * kbdMatrixCols + col
            in km IBVector.// [(idx, keyStates sc)]

        newMatrix = foldl updateKey (IBVector.replicate (kbdMatrixRows * kbdMatrixCols) False) keyMapping

    writeIORef (keyboardMatrix (sysvia mem)) newMatrix
    -- printKeyboardMatrix mem

printKeyboardMatrix :: Memory -> IO ()
printKeyboardMatrix mem = do
    kb <- readIORef (keyboardMatrix (sysvia mem))
    let lst = IBVector.toList kb
    forM_ (zip [1..] lst) $ \(i, val) -> do
        putStr (if val then "1," else "0,")
        when (i `mod` kbdMatrixCols == 0) $
            putStrLn ""
    putStrLn "_______________________________"
