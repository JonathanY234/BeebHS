module SDLVideoOutput where

import Control.Monad (forM_)
import Data.Text qualified
import Data.Vector qualified as IBVector
import Data.Word (Word8, Word16)
import Foreign.C.Types (CInt)
import Foreign.Storable (pokeByteOff)
import SDL qualified
import SDL.Vect (V4 (..))

import MemoryRegisters(Memory, readMemory)


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

eventLoop :: IO Bool
eventLoop = do
    events <- SDL.pollEvents
    let quit = SDL.QuitEvent `elem` map SDL.eventPayload events

    return quit

renderMode7Frame :: SDLContext -> IBVector.Vector [[Bool]] -> Memory -> Word16 -> IO ()
renderMode7Frame SDLContext {texture = texture_, renderer = renderer_} fontVector mem startOffset = do
    _ <-
        SDL.lockTexture texture_ Nothing >>= \(pixelsPtr, pitch) -> do
        let letterScreenCoords = [(x, y) | y <- [0 .. 24], x <- [0 .. 39]]
            videoMemStart = 0x7C00

            --letterIndex is what letter to draw within fontVector, (cellX, cellY) is the top left Screen coords
            drawLetter :: (Word16, Word16) -> IO ()
            drawLetter (cellX, cellY) = do

                let idx = ((startOffset + videoMemStart) + cellY*40 + cellX) `mod` 1024
                letterIndex <- readMemory mem (videoMemStart + idx)
                let trueX = cellX * 16 + fromIntegral borderSize
                    trueY = cellY * 20 + fromIntegral borderSize
                    charBitmap = fontVector IBVector.! fromIntegral (letterIndex - 0x20) -- -0x20 maps memory values to font indexed (temp solution)
                forM_ (zip [0 ..] charBitmap) $ \(row, rowPixels) ->
                    forM_ (zip [0 ..] rowPixels) $ \(col, bit) -> do
                        let px = trueX + col
                            py = trueY + row
                            colour =
                                if bit
                                    then V4 255 255 255 255 -- white
                                    else V4 0 0 0 255 -- black
                        drawPixel (fromIntegral px) (fromIntegral py) colour
                drawPixel (fromIntegral trueX) (fromIntegral trueY) (V4 255 0 0 255)

            drawPixel :: Int -> Int -> V4 Word8 -> IO ()
            drawPixel x y (V4 red green blue alpha) = do
                let offset = (y * fromIntegral pitch + x * 4) :: Int -- ABGR8888 = 4 bytes per pixel
                pokeByteOff pixelsPtr offset alpha
                pokeByteOff pixelsPtr (offset + 1) blue
                pokeByteOff pixelsPtr (offset + 2) green
                pokeByteOff pixelsPtr (offset + 3) red

        forM_ letterScreenCoords drawLetter

    SDL.unlockTexture texture_

    SDL.copy renderer_ texture_ Nothing Nothing
    SDL.present renderer_

endVideo :: SDLContext -> IO ()
endVideo ctxt = do
    SDL.destroyRenderer (renderer ctxt)
    SDL.destroyWindow (window ctxt)
    SDL.quit
