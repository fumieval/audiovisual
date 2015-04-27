{-# LANGUAGE Rank2Types, ViewPatterns, TypeFamilies, DeriveFunctor, GeneralizedNewtypeDeriving #-}
module Data.Graphics.Scene
  ( -- * Scene
   Rendering(..)
  , VFX(..)
  , vfx
  , withVertices
  , drawPrimitive
  , applyMatrix
  , vertices
  , foggy
  , embedIO
  , Scene(..)
  -- * Picture
  , Picture(..)
  , opacity
  , bitmap
  , toward
  ) where

import qualified Data.Graphics.Bitmap as B
import Linear
import qualified Data.Vector.Storable as V
import Control.Lens
import Data.Graphics.Class
import Data.Graphics.Vertex

bitmap :: B.Bitmap -> Picture
bitmap bmp = Picture $ Scene $ vertices bmp TriangleStrip vx
  where
    V2 w h = fmap fromIntegral $ B.size bmp
    vx = V.fromList [V3 (-w/2) (-h/2) 0 `positionUV` V2 0 0
        , V3 (w/2) (-h/2) 0 `positionUV` V2 1 0
        , V3 (-w/2) (h/2) 0 `positionUV` V2 0 1
        , V3 (w/2) (h/2) 0 `positionUV` V2 1 1]

toward :: Vec3 -> Picture -> Scene
toward n@((^/norm n) -> V3 x y z) (Picture (Scene s)) = Scene $ applyMatrix
  (m33_to_m44 $ fromQuaternion $ axisAngle (V3 (-y) x 0) $ acos $ z / norm n)
  s

newtype Rendering s = Rendering { runRendering :: forall r. Monoid r => (VFX s r -> r) -> r }

instance Affine (Rendering s) where
  type Vec (Rendering s) = V3 Float
  type Normal (Rendering s) = V3 Float
  rotateOn v = applyMatrix (m33_to_m44 $ fromQuaternion $ axisAngle v (norm v))
  {-# INLINE rotateOn #-}
  scale (V3 x y z) = applyMatrix (scaled (V4 x y z 1))
  {-# INLINE scale #-}
  translate (V3 x y z) = applyMatrix $ V4 (V4 1 0 0 x) (V4 0 1 0 y) (V4 0 0 1 z) (V4 0 0 0 1)
  {-# INLINE translate #-}

instance Monoid (Rendering s) where
  mempty = Rendering mempty
  {-# INLINE mempty #-}
  mappend (Rendering a) (Rendering b) = Rendering (mappend a b)
  {-# INLINE mappend #-}

newtype Scene = Scene { unScene :: forall s. Rendering s }

vfx :: VFX s (Rendering s) -> Rendering s
vfx v = Rendering $ \t -> t (fmap (`runRendering` t) v)
{-# INLINE vfx #-}

withVertices :: PrimitiveMode -> V.Vector Vertex -> (s -> Rendering s) -> Rendering s
withVertices m v c = vfx $ WithVertices m v c
{-# INLINE withVertices #-}

drawPrimitive :: B.Bitmap -> s -> Rendering s
drawPrimitive b = vfx . DrawPrimitive b
{-# INLINE drawPrimitive #-}

applyMatrix :: M44 Float -> Rendering s -> Rendering s
applyMatrix m = vfx . ApplyMatrix m
{-# INLINE applyMatrix #-}

vertices :: B.Bitmap -> PrimitiveMode -> V.Vector Vertex -> Rendering s
vertices b m v = withVertices m v $ drawPrimitive b
{-# INLINE vertices #-}

embedIO :: IO (Rendering s) -> Rendering s
embedIO = vfx . EmbedIO

foggy :: Float -> V4 Float -> Rendering s -> Rendering s
foggy d col = vfx . Foggy d col

data VFX s r = DrawPrimitive !B.Bitmap !s
  | ApplyMatrix !(M44 Float) r
  | Diffuse !(V4 Float) r
  | WithVertices !PrimitiveMode !(V.Vector Vertex) (s -> r)
  | Foggy !Float !(V4 Float) r
  | EmbedIO (IO r)
  deriving Functor

instance Affine Scene where
  type Vec Scene = V3 Float
  type Normal Scene = V3 Float
  rotateOn v (Scene s) = Scene $ rotateOn v s
  scale v (Scene s) = Scene $ scale v s
  translate v (Scene s) = Scene $ translate v s
  {-# INLINE translate #-}

instance Figure Scene where
  primitive m vs = Scene $ vertices B.Blank m $ V.fromList $ map positionOnly vs
  color col (Scene s) = Scene $ vfx (Diffuse col s)
  line = primitive LineStrip
  polygon = primitive TriangleFan
  polygonOutline = primitive LineLoop
  circle v = toward v $ circle $ norm v
  circleOutline v = toward v $ circleOutline $ norm v

unit_circle :: Int -> [V2 Float]
unit_circle n = map angle [0,2*pi/fromIntegral n..2*pi]

instance Monoid Scene where
  mempty = Scene mempty
  {-# INLINE mempty #-}
  mappend (Scene x) (Scene y) = Scene (mappend x y)
  {-# INLINE mappend #-}

v2ToV3 :: Num a => V2 a -> V3 a
v2ToV3 (V2 x y) = V3 x y 0

newtype Picture = Picture { unPicture :: Scene } deriving Monoid

instance Affine Picture where
  type Vec Picture = V2 Float
  type Normal Picture = Float
  rotateOn t (Picture (Scene s)) = Picture $ Scene $ applyMatrix m s where
    m = V4 (V4 (cos t) (-sin t) 0 0) (V4 (sin t) (cos t) 0 0) (V4 0 0 1 0) (V4 0 0 0 1)
  translate (V2 x y) (Picture (Scene s)) = Picture $ Scene $ applyMatrix (translation .~ V3 x y 0 $ identity) s
  scale (V2 x y) (Picture (Scene s)) = Picture $ Scene $ applyMatrix m s where
    m = V4 (V4 x 0 0 0) (V4 0 y 0 0) (V4 0 0 1 0) (V4 0 0 0 1)

instance Figure Picture where
  primitive m v = Picture $ primitive m $ map v2ToV3 v
  color col (Picture s) = Picture (color col s)
  line = primitive LineStrip
  polygon = primitive TriangleFan
  polygonOutline = primitive LineLoop
  circle r = polygon $ map (^*r) $ unit_circle 33
  circleOutline r = polygonOutline $ map (^*r) $ unit_circle 33
