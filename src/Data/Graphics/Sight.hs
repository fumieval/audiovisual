{-# LANGUAGE Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Call.Scene
-- Copyright   :  (c) Fumiaki Kinoshita 2014
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Data.Graphics.Sight where

import Data.Graphics.Scene
import Data.BoundingBox (Box(..))
import qualified Data.BoundingBox as Box
import Linear
import Control.Lens

newtype Sight = Sight { unSight
  :: forall r. Monoid r
  => Box V2 Float
  -> (Box V2 Float -> M44 Float -> Bool -> Scene -> r)
  -> r
  }

instance Monoid Sight where
  mempty = Sight $ \_ _ -> mempty
  mappend (Sight x) (Sight y) = Sight $ \b e -> x b e `mappend` y b e

viewPicture :: Picture -> Sight
viewPicture (Picture s) = Sight $ \box@(Box (V2 x0 y0) (V2 x1 y1)) f -> f box (ortho x0 x1 y1 y0 (-1) 1) False s

viewScene :: Float -- ^ FOV
  -> Float -- ^ Near plane
  -> Float -- ^ Far plane
  -> Scene -- ^ The scene
  -> Sight
viewScene fov near far = fromPerspective $ \box -> perspective fov (let V2 w h = box ^. Box.size 0 in w/h) near far

fromPerspective :: (Box V2 Float -> M44 Float) -> Scene -> Sight
fromPerspective mat s = Sight $ \box f -> f box (mat box) True s
