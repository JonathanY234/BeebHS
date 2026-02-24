module SDLVideoOutput where

import Control.Monad (forM_, when)
import Data.Text qualified
import Data.Vector qualified as IBVector
import Data.Word (Word8, Word16)
import Foreign.C.Types (CInt)
import Foreign.Storable (pokeByteOff)
import SDL qualified
import SDL.Vect (V4 (..))

import MemoryRegisters(Memory, readMemory)
import Data.IORef (newIORef, readIORef, writeIORef)

data SDLContext = SDLContext {window :: SDL.Window, renderer :: SDL.Renderer, texture :: SDL.Texture}

screenWidth :: CInt
screenWidth = 640

screenHeight :: CInt
screenHeight = 500

borderSize :: CInt
borderSize = 28

initVideo :: IO SDLContext
initVideo = do
    SDL.initialize [SDL.InitVideo]

    -- Options: ScaleNearest, ScaleLinear, ScaleBest
    SDL.HintRenderScaleQuality SDL.$= SDL.ScaleNearest

    window <- SDL.createWindow (Data.Text.pack "BeebHS") SDL.defaultWindow {SDL.windowInitialSize = SDL.V2 (screenWidth + 2 * borderSize) (screenHeight + 2 * borderSize), SDL.windowResizable = True}
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    texture <- SDL.createTexture renderer SDL.RGBA8888 SDL.TextureAccessStreaming (SDL.V2 (screenWidth + 2 * borderSize) (screenHeight + 2 * borderSize))

    let ctxt = SDLContext window renderer texture

    SDL.clear renderer
    SDL.present renderer

    return ctxt

eventLoop :: IO (Bool, Bool)
eventLoop = do
    events <- SDL.pollEvents
    let quit = SDL.QuitEvent `elem` map SDL.eventPayload events

    let eventIsQPress event =
            case SDL.eventPayload event of
            SDL.KeyboardEvent keyboardEvent ->
                SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed
                && SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
            _ -> False
        qPressed = any eventIsQPress events
    when qPressed $ putStrLn "Q pressed"

    return (quit, qPressed)

data Mode7State = Mode7State { fgColour :: V4 Word8, bgColour :: V4 Word8, flash :: Bool, graphicsMode :: Bool, holdGraphics :: Bool, doubleHeight :: Bool}

-- default state
startState :: Mode7State
startState = Mode7State { fgColour = V4 255 255 255 255, bgColour = V4 0 0 0 255, flash = False, graphicsMode = False, holdGraphics = False, doubleHeight = False}

renderMode7Frame :: SDLContext -> IBVector.Vector [[Bool]] -> Memory -> Word16 -> IO ()
renderMode7Frame SDLContext {texture = texture_, renderer = renderer_} fontVector mem startOffset = do
    m7StateRef <- newIORef startState

    _ <-
        SDL.lockTexture texture_ Nothing >>= \(pixelsPtr, pitch) -> do
        --let letterScreenCoords = [(x, y) | y <- [0 .. 24], x <- [0 .. 39]]
        let videoMemStart = 0x7C00

            --letterIndex is what letter to draw within fontVector, (cellX, cellY) is the top left Screen coords
            drawLetter :: (Word16, Word16) -> IO ()
            drawLetter (cellX, cellY) = do

                let idx = ((startOffset + videoMemStart) + cellY*40 + cellX) `mod` 1024
                letterIndex <- readMemory mem (videoMemStart + idx)

                m7State <- readIORef m7StateRef

                if letterIndex >= 128 && letterIndex <= 157 then do
                    -- control code
                    let newState = case letterIndex of
                            --TODO: only if enabled with VDU 23,18,3,1;0;0;0;
                            128 -> m7State { fgColour = V4 0 0 0 255 }       -- black    only if
                            129 -> m7State { fgColour = V4 255 0 0 255 }     -- red
                            130 -> m7State { fgColour = V4 0 255 0 255 }     -- green
                            131 -> m7State { fgColour = V4 255 255 0 255 }   -- yellow
                            132 -> m7State { fgColour = V4 0 0 255 255 }     -- blue
                            133 -> m7State { fgColour = V4 255 0 255 255 }   -- magenta
                            134 -> m7State { fgColour = V4 0 255 255 255 }   -- cyan
                            135 -> m7State { fgColour = V4 255 255 255 255 } -- white

                            141 -> m7State { doubleHeight = True }
                            140 -> m7State { doubleHeight = False }

                            157 -> m7State { bgColour = fgColour m7State }   -- set background
                            _   -> m7State -- implement the rest
                    writeIORef m7StateRef newState

                else do
                    --normal character
                    let trueX = cellX * 16 + fromIntegral borderSize
                        trueY = cellY * 20 + fromIntegral borderSize
                        
                        charBitmap = fontVector IBVector.! fromIntegral (letterIndex - 0x20) -- -0x20 maps memory values to font indexed (temp solution)

                    --handle double height chars
                    let isTopHalf = even cellY
                        halfHeight = length charBitmap `div` 2
                        rowsToDraw =
                            if doubleHeight m7State then
                                if isTopHalf
                                    then duplicateValues $ take halfHeight charBitmap
                                    else duplicateValues $ drop halfHeight charBitmap
                            else
                                charBitmap
                    
                    -- draw the char
                    forM_ (zip [0 ..] rowsToDraw) $ \(row, rowPixels) ->
                        forM_ (zip [0 ..] rowPixels) $ \(col, bit) -> do
                            let px = trueX + col
                                py = trueY + row
                                colour =
                                    if bit then fgColour m7State else bgColour m7State
                            drawPixel (fromIntegral px) (fromIntegral py) colour

            duplicateValues :: [a] -> [a]
            duplicateValues = concatMap (replicate 2)

            drawPixel :: Int -> Int -> V4 Word8 -> IO ()
            drawPixel x y (V4 red green blue alpha) = do
                let offset = (y * fromIntegral pitch + x * 4) :: Int -- ABGR8888 = 4 bytes per pixel
                pokeByteOff pixelsPtr offset alpha
                pokeByteOff pixelsPtr (offset + 1) blue
                pokeByteOff pixelsPtr (offset + 2) green
                pokeByteOff pixelsPtr (offset + 3) red

        --forM_ letterScreenCoords drawLetter
        forM_ [0 .. 24] $ \cellY -> do
            writeIORef m7StateRef startState  -- reset state at newline
            forM_ [0 .. 39] $ \cellX ->
                drawLetter (cellX, cellY)

    SDL.unlockTexture texture_

    SDL.copy renderer_ texture_ Nothing Nothing
    SDL.present renderer_

endVideo :: SDLContext -> IO ()
endVideo ctxt = do
    SDL.destroyRenderer (renderer ctxt)
    SDL.destroyWindow (window ctxt)
    SDL.quit
