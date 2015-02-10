{-# LANGUAGE TypeFamilies, DeriveDataTypeable #-}
module Data.Graphics.Class where
import Data.Typeable
import Linear

data PrimitiveMode = LineStrip | TriangleFan | TriangleStrip | LineLoop deriving (Enum, Eq, Ord, Read, Show, Typeable)

class Affine a where
  type Vec a :: *
  type Normal a :: *
  rotateOn :: Normal a -> a -> a
  scale :: Vec a -> a -> a
  translate :: Vec a -> a -> a

class Affine a => Figure a where
  primitive :: PrimitiveMode -> [Vec a] -> a
  color :: V4 Float -> a -> a
  line :: [Vec a] -> a
  polygon :: [Vec a] -> a
  polygonOutline :: [Vec a] -> a
  circle :: Normal a -> a
  circleOutline :: Normal a -> a

opacity :: Figure a => Float -> a -> a
opacity p = color (V4 1 1 1 p)

data BlendMode = Normal
    | Inverse
    | Add
    | Multiply
    | Screen
    deriving (Enum, Eq, Ord, Read, Show, Typeable)
