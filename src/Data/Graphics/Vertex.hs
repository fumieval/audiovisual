{-# LANGUAGE DeriveDataTypeable #-}
module Data.Graphics.Vertex where
import Foreign.Storable
import Foreign.Ptr
import Data.Typeable
import Control.Applicative
import Linear

type Vec2 = V2 Float
type Vec3 = V3 Float

data Vertex = Vertex { vPos :: {-# UNPACK #-} !Vec3
  , vUV :: {-# UNPACK #-} !Vec2
  , vNormal :: {-# UNPACK #-} !Vec3 }
  deriving (Show, Eq, Ord, Read, Typeable)

align1 :: Int
align1 = sizeOf (vPos undefined)

align2 :: Int
align2 = align1 + sizeOf (vUV undefined)

instance Storable Vertex where
  sizeOf _ = sizeOf (undefined :: Vec3) + sizeOf (undefined :: Vec2) + sizeOf (undefined :: Vec3)
  {-# INLINE sizeOf #-}
  alignment _ = 0
  {-# INLINE alignment #-}
  peek ptr = Vertex
    <$> peek (castPtr ptr)
    <*> peek (castPtr $ ptr `plusPtr` align1)
    <*> peek (castPtr $ ptr `plusPtr` align2)
  {-# INLINE peek #-}
  poke ptr (Vertex v t n) = do
    poke (castPtr ptr) v
    poke (castPtr ptr `plusPtr` align1) t
    poke (castPtr ptr `plusPtr` align2) n
  {-# INLINE poke #-}

positionUV :: Vec3 -> Vec2 -> Vertex
positionUV v p = Vertex v p (V3 0 0 1)

positionOnly :: Vec3 -> Vertex
positionOnly v = Vertex v zero (V3 0 0 1)
