{-# LANGUAGE FlexibleContexts, ConstraintKinds #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Call.Util.Deck
-- Copyright   :  (c) Fumiaki Kinoshita 2014
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Polyphonic sampler
--
-----------------------------------------------------------------------------
module Audiovisual.Sampler where
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import Control.Monad.ST
import Control.Monad.State.Strict
import Data.Audio
import Foreign.Storable

data Sampler a = Sampler [(Sample a, Time)]

empty :: Sampler a
empty = Sampler []

playback :: (Num a, Storable a, MonadState (Sampler a) m) => Time -> Int -> m (V.Vector a)
playback dt n = do
  Sampler vs <- get
  let (vs'', r) = runST $ do
        v <- MV.new n
        vs' <- forM vs $ \(s0@(Sample d (Source s)), t0) -> do
          if d > t0 then return []
            else do
              forM_ [0..n-1] $ \i -> do
                z <- MV.unsafeRead v i
                MV.unsafeWrite v i $ z + s (t0 + f * fromIntegral i)
              return [(s0, t0 + dt)]
        v' <- V.unsafeFreeze v
        return (vs', v')
  put $ Sampler $ concat vs''
  return r
  where
    f = dt / fromIntegral n

play :: MonadState (Sampler a) m => Sample a -> m ()
play s = modify $ \(Sampler xs) -> Sampler $ (s, 0) : xs
