module KeyboardInput where

import Data.IORef (readIORef, writeIORef)
import Data.Vector qualified as IBVector
import Memory (Memory, kbMatrixCols, kbMatrixRows, keyboardMatrix, kbMatrixCols)
import SDL qualified
import Control.Monad (forM_, when)

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
        (SDL.ScancodeReturn, (4, 9))
    ]

updateKeyboardMatrix :: Memory -> IO ()
updateKeyboardMatrix mem = do
    keyStates <- SDL.getKeyboardState

    let updateKey :: IBVector.Vector Bool -> (SDL.Scancode, (Int, Int)) -> IBVector.Vector Bool
        updateKey km (sc, (row, col)) =
            let idx = row * kbMatrixCols + col
            in km IBVector.// [(idx, keyStates sc)]

        newMatrix = foldl updateKey (IBVector.replicate (kbMatrixRows * kbMatrixCols) False) keyMapping

    writeIORef (keyboardMatrix mem) newMatrix
    -- printKeyboardMatrix mem

printKeyboardMatrix :: Memory -> IO ()
printKeyboardMatrix mem = do
    kb <- readIORef (keyboardMatrix mem)
    let lst = IBVector.toList kb
    forM_ (zip [1..] lst) $ \(i, val) -> do
        putStr (if val then "1," else "0,")
        when (i `mod` kbMatrixCols == 0) $
            putStrLn ""
    putStrLn "_______________________________"
