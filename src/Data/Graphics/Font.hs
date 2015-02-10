-----------------------------------------------------------------------------
-- |
-- Module      :  Call.Data.Font
-- Copyright   :  (c) Fumiaki Kinoshita 2014
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Font rendering
--
-----------------------------------------------------------------------------
module Data.Graphics.Font
  ( Font
  , readFont
  , defaultFont
  , fontBoundingBox
  , metricsAscent
  , metricsDescent
  , renderChar
  ) where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad
import Data.BoundingBox
import qualified Data.Vector.Storable as V
import Linear
import Data.Graphics.Bitmap
import Graphics.Rendering.FreeType.Internal
import qualified Graphics.Rendering.FreeType.Internal.GlyphSlot as GS
import qualified Graphics.Rendering.FreeType.Internal.Vector as V
import Graphics.Rendering.FreeType.Internal.Bitmap as B
import Graphics.Rendering.FreeType.Internal.PrimitiveTypes as PT
import Graphics.Rendering.FreeType.Internal.Face as F
import Graphics.Rendering.FreeType.Internal.Library as L
import Graphics.Rendering.FreeType.Internal.BBox as BB
import Foreign.Marshal.Alloc
import Foreign.C.Types
import Foreign.C.String
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Ptr
import System.IO.Unsafe
import Codec.Picture
import Codec.Picture.RGBA8
import Paths_audiovisual

-- | Font object
data Font = Font FT_Face (Float, Float) (Box V2 Float)

defaultFont :: Font
defaultFont = unsafePerformIO $ do
  path <- getDataFileName "data/VL-PGothic-Regular.ttf"
  readFont path

-- | Create a 'Font' from the given file.
readFont :: MonadIO m => FilePath -> m Font
readFont path = liftIO $ alloca $ \p -> do
    runFreeType $ withCString path $ \str -> ft_New_Face freeType str 0 p
    f <- peek p
    b <- peek (F.bbox f)
    asc <- peek (ascender f)
    desc <- peek (descender f)
    u <- fromIntegral <$> peek (units_per_EM f)
    let box = fmap ((/u).fromIntegral) $ Box
            (V2 (xMin b) (yMin b))
            (V2 (xMax b) (yMax b))
    return $ Font f (fromIntegral asc/u, fromIntegral desc/u) box

-- | Get the font's metrics.
metricsAscent :: Font -> Float
metricsAscent (Font _ (a, _) _) = a

-- | Get the font's metrics.
metricsDescent :: Font -> Float
metricsDescent (Font _ (_, d) _) = d

-- | Get the font's bounding box.
fontBoundingBox :: Font -> Box V2 Float
fontBoundingBox (Font _ _ b) = b

runFreeType :: IO CInt -> IO ()
runFreeType m = do
    r <- m
    unless (r == 0) $ fail $ "FreeType Error:" Prelude.++ show r

freeType :: FT_Library
freeType = unsafePerformIO $ alloca $ \p -> do
  runFreeType $ ft_Init_FreeType p
  peek p

renderChar :: Font -> Float -> Char -> IO (Bitmap, V2 Float, V2 Float)
renderChar (Font face _ _) pixel ch = do
  let dpi = 300

  runFreeType $ ft_Set_Char_Size face 0 (floor $ pixel * 72 / fromIntegral dpi * 64) dpi dpi

  ix <- ft_Get_Char_Index face (fromIntegral $ fromEnum ch)
  runFreeType $ ft_Load_Glyph face ix ft_LOAD_DEFAULT

  slot <- peek $ glyph face
  runFreeType $ ft_Render_Glyph slot ft_RENDER_MODE_NORMAL

  bmp <- peek $ GS.bitmap slot
  left <- fmap fromIntegral $ peek $ GS.bitmap_left slot
  top <- fmap fromIntegral $ peek $ GS.bitmap_top slot

  let h = fromIntegral $ B.rows bmp
      w = fromIntegral $ B.width bmp

  fptr <- newForeignPtr_ $ castPtr $ buffer bmp

  adv <- peek $ GS.advance slot
  b <- liftImage' $ fromColorAndOpacity (PixelRGB8 255 255 255)
        $ Image w h $ V.unsafeFromForeignPtr0 fptr $ h * w
  return (b
    , V2 (left + fromIntegral w / 2) (-top + fromIntegral h / 2)
    , V2 (fromIntegral (V.x adv) / 64) 0)
