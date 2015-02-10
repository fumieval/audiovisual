{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Call.Data.Bitmap
-- Copyright   :  (c) Fumiaki Kinoshita 2014
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Data.Graphics.Bitmap (
  Bitmap(Blank, Bitmap)
  ,image
  ,offset
  ,hash
  ,liftImage
  ,liftImage'
  ,size
  ,clip
  ,bbox
  ,readFile
  ,writeFile
  ) where


import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.ST
import Data.Char
import Data.Monoid
import Language.Haskell.TH
import Linear
import Prelude hiding (readFile, writeFile)
import qualified Codec.Picture as C
import qualified Codec.Picture.RGBA8 as C
import qualified Codec.Picture.Types as C
import qualified Data.BoundingBox as B
import qualified Data.Hashable as H
import qualified Data.Vector.Storable as V
import System.Directory
import System.FilePath
import System.IO.Unsafe
import System.Random

data Bitmap = Blank | Bitmap { _image :: !(C.Image C.PixelRGBA8), _offset :: !(V2 Int), _hash :: !Int }

image :: Traversal' Bitmap (C.Image C.PixelRGBA8)
image _ Blank = pure Blank
image f (Bitmap i o h) = f i <&> \i' -> Bitmap i' o h

offset :: Traversal' Bitmap (V2 Int)
offset _ Blank = pure Blank
offset f (Bitmap i o h) = f o <&> \o' -> Bitmap i o' h

hash :: Traversal' Bitmap Int
hash _ Blank = pure Blank
hash f (Bitmap i o h) = f h <&> \h' -> Bitmap i o h'

-- | `mappend` stitches the right operand to the left
instance Monoid Bitmap where
  mempty = Blank
  mappend base@(Bitmap b (V2 x0 y0) h0) lay@(Bitmap l (V2 x1 y1) h1) = runST $ do
    let box = B.union (bbox base) (bbox lay)
    let V2 w h = box ^. B.size 0
    img <- C.createMutableImage w h (C.PixelRGBA8 0 0 0 0) >>= C.unsafeFreezeImage
    let ox = max 0 (x0 - x1)
    let oy = max 0 (y0 - y1)
    return $ Bitmap
      (C.patchImage (C.patchImage img (ox, oy) b) (max 0 (x1 - x0), max 0 (y1 - y0)) l)
      (V2 (x0 + ox) (y0 + oy))
      (H.hash (h0, h1))
  mappend Blank b = b
  mappend b Blank = b

clip :: Bitmap -> B.Box V2 Int -> Bitmap
clip (Bitmap b (V2 ox oy) k) (B.Box (V2 x0 y0) (V2 x1 y1)) = Bitmap
  (C.trimImage b (x1 - x0, y1 - y0) (x0 - ox, y0 - oy))
  (V2 ox oy)
  (H.hash (x0, y0, x1, y1, k))

clip Blank _ = Blank

bbox :: Bitmap -> B.Box V2 Int
bbox (Bitmap (C.Image w h _) (V2 x y) _) = B.Box (V2 x y) (V2 (x+w) (y+h))
bbox Blank = B.Box zero zero

size :: Bitmap -> V2 Int
size (Bitmap (C.Image w h _) _ _) = V2 w h
size Blank = zero

liftImage :: C.Image C.PixelRGBA8 -> Bitmap
liftImage b@(C.Image _ _ r) = Bitmap b zero (V.foldl H.hashWithSalt 0 r)

liftImage' :: MonadIO m => C.Image C.PixelRGBA8 -> m Bitmap
liftImage' b = liftIO $ Bitmap b zero <$> randomIO

-- | Load an image file.
readFile :: MonadIO m => FilePath -> m Bitmap
readFile path = liftIO $ Bitmap <$> C.readImageRGBA8 path <*> pure zero <*> randomIO

-- | Save 'Bitmap' into a file.
writeFile :: MonadIO m => FilePath -> Bitmap -> m ()
writeFile path (Bitmap p _ _) = liftIO $ C.writePng path p
writeFile _ Blank = fail "Blank bitmap"

-- | The type of the given 'ExpQ' must be @FilePath -> IO FilePath@
-- FIXME: This may cause name duplication if there are multiple non-alphanumeric file names.
loadBitmapsWith :: ExpQ -> FilePath -> Q [Dec]
loadBitmapsWith getFullPath path = do
    loc <- (</>path) <$> takeDirectory <$> loc_filename <$> location
    paths <- runIO $ getFileList loc

    sequence $ do
        p <- paths
        let name = pathToName p
        [ return $ SigD (mkName name) (ConT ''Bitmap)
            , funD (mkName name) [clause [] (normalB $ load name $ loc </> p) []]
            ]
    where
        load name fp = do
            runIO $ putStrLn $ "Defined: " ++ fp ++ " as `" ++ name ++ "'"

            appE (varE 'unsafePerformIO) $ uInfixE (appE getFullPath $ litE $ StringL fp)
                (varE '(>>=))
                (varE 'readFile)

-- | Load and define all pictures in the specified directory.
-- On base >= 4.6, file paths to actually load will be respect to the directory of the executable. Otherwise it will be based on the current directory.


getFileList :: FilePath -> IO [FilePath]
getFileList path = do
    allContents <- filter notHidden `fmap` getDirectoryContents path

    files <- filterM (doesFileExist . (path</>)) allContents
    dirs <- filterM (doesDirectoryExist . (path</>)) allContents
    fmap ((files++).concat) $ forM dirs $ \i -> map (i</>) `fmap` getFileList (path</>i)
    where
        notHidden ('.':_) = False
        notHidden _ = True

pathToName :: FilePath -> String
pathToName = ('_':) . map p where
    p c | isAlphaNum c = c
        | otherwise = '_'
