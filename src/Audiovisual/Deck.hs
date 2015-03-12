{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
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
-- The deck for a single stream
--
-----------------------------------------------------------------------------
module Audiovisual.Deck (Deck(..), empty, source, pos, playing, playback) where
import Control.Lens
import Control.Monad.State.Strict
import Data.Audio
import Foreign.Storable
import qualified Data.Vector.Storable as V

data Deck a = Deck
  { _src :: Source a
  , _pos :: !Time
  , _playing :: !Bool }

empty :: Num a => Deck a
empty = Deck (Source $ const 0) 0 False

source :: Lens' (Deck a) (Source a)
source f s = f (_src s) <&> \a -> s { _src = a }

pos :: Lens' (Deck a) Time
pos f s = f (_pos s) <&> \a -> s { _pos = a }

playing :: Lens' (Deck a) Bool
playing f s = f (_playing s) <&> \a -> s { _playing = a }

playback :: (Num a, Storable a, MonadState (Deck a) m) => Time -> Int -> m (V.Vector a)
playback dt n = do
  Source s <- use source
  pl <- use playing
  t0 <- use pos
  if pl
    then do
      pos += dt
      return $ V.fromList $ take n $ map s [t0,t0 + dt / fromIntegral n..]
    else return $ V.replicate n 0
