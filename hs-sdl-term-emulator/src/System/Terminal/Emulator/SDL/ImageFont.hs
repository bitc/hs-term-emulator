module System.Terminal.Emulator.SDL.ImageFont where

import Control.Monad (forM_)
import Data.Bits (shiftR, (.&.), (.|.))
import Data.Word (Word32)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peekElemOff, pokeElemOff)
import Linear (V2 (..))
import qualified SDL as SDL

loadImageFont :: IO SDL.Surface
loadImageFont = do
  bmp <- SDL.loadBMP "font.bmp"
  rgba <- convertGrayscaleToAlpha bmp
  SDL.freeSurface bmp
  pure rgba

-- | Creates a new surface in RGBA format where the color of all pixels is
-- white, and the alpha channel is taken from the grayscale values of the
-- source surface
convertGrayscaleToAlpha ::
  -- | 'Surface' to copy from
  SDL.Surface ->
  -- | New 'Surface' is created
  IO SDL.Surface
convertGrayscaleToAlpha grayscaleRGB = do
  size@(V2 width height) <- SDL.surfaceDimensions grayscaleRGB
  rgba <- SDL.createRGBSurface size SDL.RGBA8888
  _ <- SDL.surfaceBlit grayscaleRGB Nothing rgba Nothing

  SDL.lockSurface rgba
  pixelsPtr <- SDL.surfacePixels rgba
  let pixels :: Ptr Word32
      pixels = castPtr pixelsPtr
  forM_ [0 .. fromIntegral (width * height) - 1] $ \i -> do
    pixel <- peekElemOff pixels i
    let intensity = (pixel .&. 0xFF000000) `shiftR` 24
    pokeElemOff pixels i (intensity .|. 0xFFFFFF00)
  SDL.unlockSurface rgba

  pure rgba
