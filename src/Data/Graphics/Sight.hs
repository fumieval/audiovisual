{-# LANGUAGE Rank2Types #-}
module Data.Graphics.Sight where
import Data.Graphics.Scene
import Data.Monoid
import Linear
import Data.BoundingBox as X
import Control.Lens

newtype Sight = Sight { unSight
  :: forall r.
  X.Box V2 Float
  -> r
  -> (r -> r -> r)
  -> (X.Box V2 Float -> M44 Float -> Bool -> Scene -> r)
  -> r
  }

instance Monoid Sight where
  mempty = Sight $ \_ e _ _ -> e
  mappend (Sight x) (Sight y) = Sight $ \b e a f -> a (x b e a f) (y b e a f)

viewPicture :: Picture -> Sight
viewPicture (Picture s) = Sight $ \box@(X.Box (V2 x0 y0) (V2 x1 y1)) _ _ f -> f box (ortho x0 x1 y1 y0 (-1) 1) False s

viewScene :: Float -- ^ FOV
  -> Float -- ^ Near plane
  -> Float -- ^ Far plane
  -> Scene -- ^ The scene
  -> Sight
viewScene fov near far = fromPerspective $ \box -> perspective fov (let V2 w h = box ^. X.size 0 in w/h) near far

fromPerspective :: (X.Box V2 Float -> M44 Float) -> Scene -> Sight
fromPerspective mat s = Sight $ \box _ _ f -> f box (mat box) True s
