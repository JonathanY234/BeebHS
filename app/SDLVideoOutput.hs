module SDLVideoOutput where

import Control.Monad (forM_, when, forM)
import Data.Text qualified
import Data.Vector qualified as IBVector
import Data.Word (Word8, Word16)
import Foreign.C.Types (CInt)
import Foreign.Storable (pokeByteOff)
import SDL qualified
import SDL.Vect (V4 (..))

import MemoryRegisters(Memory, readMemory)
import Data.IORef (newIORef, readIORef, writeIORef)
import Utilities (showHexF)

data SDLContext = SDLContext {window :: SDL.Window, renderer :: SDL.Renderer, texture :: SDL.Texture}

screenWidth, screenHeight, borderSize :: CInt
screenWidth = 640
screenHeight = 500
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

data GraphicsMode = Text | GraphicsCont | GraphicsSep
data Mode7State = Mode7State { fgColour :: V4 Word8, bgColour :: V4 Word8, flash :: Bool, graphicsMode :: GraphicsMode, holdGraphics :: Bool, doubleHeight :: Bool}

startState :: Mode7State
startState = Mode7State { fgColour = V4 255 255 255 255, bgColour = V4 0 0 0 255, flash = False, graphicsMode = Text, holdGraphics = False, doubleHeight = False}

renderMode7Frame :: SDLContext -> IBVector.Vector [[Bool]] -> Memory -> Word16 -> IO ()
renderMode7Frame SDLContext {texture = texture_, renderer = renderer_} fontVector mem startOffset = do
    m7StateRef <- newIORef startState

    --dumpMode7Memory mem

    _ <-
        SDL.lockTexture texture_ Nothing >>= \(pixelsPtr, pitch) -> do
        let videoMemStart = 0x7C00

            --letterIndex is what letter to draw within fontVector, (cellX, cellY) is the top left Screen coords
            drawLetter :: (Word16, Word16) -> IO ()
            drawLetter (cellX, cellY) = do
                let idx = ((startOffset + videoMemStart) + cellY*40 + cellX) `mod` 1024
                letterIndex <- readMemory mem (videoMemStart + idx)

                m7State <- readIORef m7StateRef

                if letterIndex >= 128 && letterIndex <= 159 then do
                    -- control code
                    let newState = case letterIndex of
                            --TODO: only if enabled with VDU 23,18,3,1;0;0;0;
                            128 -> m7State { fgColour = V4 0 0 0 255, graphicsMode = Text }       -- black    only if VDU
                            129 -> m7State { fgColour = V4 255 0 0 255, graphicsMode = Text }     -- red
                            130 -> m7State { fgColour = V4 0 255 0 255, graphicsMode = Text }     -- green
                            131 -> m7State { fgColour = V4 255 255 0 255, graphicsMode = Text }   -- yellow
                            132 -> m7State { fgColour = V4 0 0 255 255, graphicsMode = Text }     -- blue
                            133 -> m7State { fgColour = V4 255 0 255 255, graphicsMode = Text }   -- magenta
                            134 -> m7State { fgColour = V4 0 255 255 255, graphicsMode = Text }   -- cyan
                            135 -> m7State { fgColour = V4 255 255 255 255, graphicsMode = Text } -- white

                            --136 flashing

                            140 -> m7State { doubleHeight = False }
                            141 -> m7State { doubleHeight = True }

                            144 -> m7State { fgColour = V4 0 0 0 255, graphicsMode = GraphicsCont }       -- black    only if VDU
                            145 -> m7State { fgColour = V4 255 0 0 255, graphicsMode = GraphicsCont }     -- red
                            146 -> m7State { fgColour = V4 0 255 0 255, graphicsMode = GraphicsCont }     -- green
                            147 -> m7State { fgColour = V4 255 255 0 255, graphicsMode = GraphicsCont }   -- yellow
                            148 -> m7State { fgColour = V4 0 0 255 255, graphicsMode = GraphicsCont }     -- blue
                            149 -> m7State { fgColour = V4 255 0 255 255, graphicsMode = GraphicsCont }   -- magenta
                            150 -> m7State { fgColour = V4 0 255 255 255, graphicsMode = GraphicsCont }   -- cyan
                            151 -> m7State { fgColour = V4 255 255 255 255, graphicsMode = GraphicsCont } -- white

                            154 -> m7State { graphicsMode = GraphicsSep }

                            157 -> m7State { bgColour = fgColour m7State }   -- set background
                            _   -> m7State -- implement the rest
                    writeIORef m7StateRef newState

                else do
                    --normal character
                    let trueX = cellX * 16 + fromIntegral borderSize
                        trueY = cellY * 20 + fromIntegral borderSize

                        safeIndex = if letterIndex >= 0xA0 then letterIndex - 0x80 else letterIndex

                        bankCorrectedLetterInde :: Int
                        bankCorrectedLetterInde = case graphicsMode m7State of
                            Text         -> fromIntegral safeIndex - 32
                            GraphicsCont -> fromIntegral safeIndex - 160 + 96 + 128
                            GraphicsSep  -> fromIntegral safeIndex - (160 + 192)

                        bankCorrectedLetterIndee = max bankCorrectedLetterInde 0
                        bankCorrectedLetterIndex = min bankCorrectedLetterIndee 287

                        charBitmap = fontVector IBVector.! fromIntegral bankCorrectedLetterIndex

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

dumpMode7Memory :: Memory -> IO ()
dumpMode7Memory mem = do
    let startAddr = 0x7C00
        rows :: Int
        rows = 25
        cols = 40
    forM_ [0 .. rows - 1] $ \y -> do
        rowVals <- forM [0 .. cols - 1] $ \x -> do
            let addr = startAddr + fromIntegral (y*cols + x)
            readMemory mem addr
        -- print each row as space-separated hex
        putStrLn $ unwords [showHexF v | v <- rowVals]
