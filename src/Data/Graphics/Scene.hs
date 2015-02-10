{-# LANGUAGE Rank2Types, ViewPatterns, TypeFamilies, DeriveFunctor, GeneralizedNewtypeDeriving #-}
module Data.Graphics.Scene where
import Data.Graphics.Class
import Data.Graphics.Vertex
import qualified Data.BoundingBox as B
import Data.Graphics.Bitmap as Bitmap (Bitmap(..), size)
import Data.Monoid
import Linear
import qualified Data.Vector.Storable as V
import Control.Lens

newtype Scene = Scene { unScene :: forall r.
  r
  -> (r -> r -> r)
  -> (Bitmap -> PrimitiveMode -> V.Vector Vertex -> r)
  -> (VFX r -> r)
  -> (M44 Float -> r -> r)
  -> r
  }

data VFX r = SphericalAdd Bitmap r
  | SphericalMultiply Bitmap r
  | Diffuse (V4 Float) r
  | Specular (V3 Float) r
  | Ambient (V3 Float) r
  | NormalMap Bitmap r
  | EmbedIO (IO r)
  deriving Functor

applyVFX :: VFX Scene -> Scene
applyVFX vf = Scene $ \e a f b t -> b $ fmap (\(Scene s) -> s e a f b t) vf

instance Affine Scene where
  type Vec Scene = V3 Float
  type Normal Scene = V3 Float
  rotateOn v = transformScene $ m33_to_m44 $ fromQuaternion $ axisAngle v (norm v)
  scale (V3 x y z) = transformScene $ V4
    (V4 x 0 0 0)
    (V4 0 y 0 0)
    (V4 0 0 z 0)
    (V4 0 0 0 1)
  translate v = transformScene $ translation .~ v $ eye4

instance Figure Scene where
  primitive m vs = Scene $ \_ _ f _ _ -> f Blank m (V.fromList $ map positionOnly vs)
  color col (Scene s) = Scene $ \e a f b t -> b (Diffuse col (s e a f b t))
  line = primitive LineStrip
  polygon = primitive TriangleFan
  polygonOutline = primitive LineLoop
  circle v = toward v $ circle $ norm v
  circleOutline v = toward v $ circleOutline $ norm v

unit_circle :: Int -> [V2 Float]
unit_circle n = map angle [0,2*pi/fromIntegral n..2*pi]

instance Monoid Scene where
  mempty = Scene $ \e _ _ _ _ -> e
  mappend (Scene x) (Scene y) = Scene $ \e a f b t -> a (x e a f b t) (y e a f b t)

vertices :: Bitmap -> PrimitiveMode -> V.Vector Vertex -> Scene
vertices b m v = Scene $ \_ _ f _ _ -> f b m v

transformScene :: M44 Float -> Scene -> Scene
transformScene m (Scene pic) = Scene $ \e a f b t -> t m (pic e a f b t)

-------------------------------


newtype Picture = Picture { unPicture :: Scene } deriving Monoid

instance Affine Picture where
  type Vec Picture = V2 Float
  type Normal Picture = Float
  rotateOn t (Picture s) = Picture (transformScene m s) where
    m = V4 (V4 (cos t) (-sin t) 0 0) (V4 (sin t) (cos t) 0 0) (V4 0 0 1 0) (V4 0 0 0 1)
  translate (V2 x y) (Picture s) = Picture $ transformScene (translation .~ V3 x y 0 $ eye4) s
  scale (V2 x y) (Picture s) = Picture (transformScene m s) where
    m = V4 (V4 x 0 0 0) (V4 0 y 0 0) (V4 0 0 1 0) (V4 0 0 0 1)

instance Figure Picture where
  primitive m v = Picture $ primitive m $ map v2ToV3 v
  color col (Picture s) = Picture (color col s)
  line = primitive LineStrip
  polygon = primitive TriangleFan
  polygonOutline = primitive LineLoop
  circle r = polygon $ map (^*r) $ unit_circle 33
  circleOutline r = polygonOutline $ map (^*r) $ unit_circle 33

v2ToV3 :: Num a => V2 a -> V3 a
v2ToV3 (V2 x y) = V3 x y 0

bitmap :: Bitmap -> Picture
bitmap bmp = Picture $ Scene
  $ \_ _ f _ _ -> f bmp TriangleStrip
    (V.fromList [V3 (-w/2) (-h/2) 0 `positionUV` V2 0 0
        , V3 (w/2) (-h/2) 0 `positionUV` V2 1 0
        , V3 (-w/2) (h/2) 0 `positionUV` V2 0 1
        , V3 (w/2) (h/2) 0 `positionUV` V2 1 1]) where
  V2 w h = fmap fromIntegral $ Bitmap.size bmp

toward :: Vec3 -> Picture -> Scene
toward n@((^/norm n) -> V3 x y z) (Picture s) = transformScene
  (m33_to_m44 $ fromQuaternion $ axisAngle (V3 (-y) x 0) $ acos $ z / norm n)
  s
