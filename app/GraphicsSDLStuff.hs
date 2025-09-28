module GraphicsSDLStuff where

import qualified SDL
import qualified Data.Text
import SDL.Vect (V4(..))
import Foreign.Storable (pokeByteOff)
import qualified Data.Vector as IBVector
import Foreign.C.Types (CInt)
import Data.Word (Word8)
import Control.Monad (forM_)

data SDLContext = SDLContext { window :: SDL.Window, renderer :: SDL.Renderer, texture :: SDL.Texture }

screenWidth :: CInt
screenWidth = 640
screenHeight :: CInt
screenHeight = 500

initVideo :: IO SDLContext
initVideo = do
    SDL.initialize [SDL.InitVideo]
    

    window <- SDL.createWindow (Data.Text.pack "BBCulator") SDL.defaultWindow { SDL.windowInitialSize = SDL.V2 screenWidth screenHeight }
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    texture <- SDL.createTexture renderer SDL.RGBA8888 SDL.TextureAccessStreaming (SDL.V2 screenWidth screenHeight)

    let ctxt = SDLContext window renderer texture

    -- possibly temporary
    SDL.rendererDrawColor renderer SDL.$= V4 0 128 255 255
    SDL.clear renderer
    SDL.present renderer

    return ctxt

eventLoop :: IO Bool
eventLoop = do
    events <- SDL.pollEvents
    let quit = SDL.QuitEvent `elem` map SDL.eventPayload events
    return quit
    -- if quit
    --     then return True   -- quit button pressed
    --     else return False


renderMode7Frame :: SDLContext -> IBVector.Vector [[Bool]] -> [Word8] -> IO ()
renderMode7Frame SDLContext {texture = texture_, renderer = renderer_} fontVector letterIndexes = do
    _ <- SDL.lockTexture texture_ Nothing >>= \(pixelsPtr, pitch) -> do
        
        let letterScreenCoords = [(x, y) | x <- [1..40], y <- [1..25]]

            drawLetter :: Word8 -> (Int, Int) -> IO ()
            drawLetter letterIndex (cellX , cellY) = do
                let trueX = (cellX -1) * 16
                    trueY = (cellY -1) * 20
                    charBitmap = fontVector IBVector.! fromIntegral letterIndex  -- [[Bool]]

                forM_ (zip [0..] charBitmap) $ \(row, rowPixels) ->
                    forM_ (zip [0..] rowPixels) $ \(col, bit) -> do
                        let px = trueX + col
                            py = trueY + row
                            colour = if bit
                                then V4 255 255 255 255   -- white
                                else V4 0 0 0 255         -- black
                        drawPixel px py colour
            
            drawPixel :: Int -> Int -> V4 Word8 -> IO ()
            drawPixel x y (V4 red green blue alpha) = do
                let offset = (y * fromIntegral pitch + x * 4) :: Int  -- ABGR8888 = 4 bytes per pixel
                pokeByteOff pixelsPtr offset     alpha
                pokeByteOff pixelsPtr (offset+1) blue
                pokeByteOff pixelsPtr (offset+2) green
                pokeByteOff pixelsPtr (offset+3) red

        mapM_ (uncurry drawLetter) (zip letterIndexes letterScreenCoords)
        -- drawLetter 65 (1,1)
        -- drawLetter 69 (40,1)
        -- drawLetter 69 (1,25)

    SDL.unlockTexture texture_

    SDL.copy renderer_ texture_ Nothing Nothing
    SDL.present renderer_


endVideo :: SDLContext -> IO ()
endVideo ctxt = do
    SDL.destroyRenderer (renderer ctxt)
    SDL.destroyWindow (window ctxt)
    SDL.quit

