{-# LANGUAGE ConstraintKinds, FlexibleContexts, BangPatterns, DataKinds #-}
module Prefabricated.Text where
import Prelude hiding (putStr)
import Data.Graphics.Bitmap (Bitmap(..))
import Data.Graphics.Font
import Data.Graphics.Class
import Data.Graphics.Scene
import Data.Extensible
import Control.Lens
import Control.Monad.State.Class
import Control.Monad.Trans
import Control.Monad.Free
import Control.Object
import Data.Monoid
import Linear
import Control.DeepSeq
import Control.Applicative

renderer :: (Applicative m, MonadIO m) => Font -> Float -> Object (Request Char (Bitmap, V2 Float, V2 Float)) m
renderer font size = flyweight (liftIO . renderChar font size)

type Console = League '[(,) Char, (->) Picture]

typewriter :: MonadIO m => Float -> (Char -> m (Bitmap, V2 Float, V2 Float))
  -> Object Console m
typewriter l req = stateful (match (go <?!$ uses _2 <?!$ Nil) . getLeague) (V2 0 0, mempty) where
  go ('\3', cont) = do
    put (V2 0 0, mempty)
    return cont
  go ('\r', cont) = return cont
  go ('\n', cont) = do
    _1 . _y += l
    _1 . _x .= 0
    return cont
  go (ch, cont) = do
    (pos, pic) <- get
    (!bmp@(Bitmap img _ _), !ofs, !adv) <- lift $ req ch
    return $! rnf img
    put (pos + adv, pic <> translate (pos + ofs) (bitmap bmp))
    return cont

putStr :: String -> Free Console ()
putStr [] = return ()
putStr (c:cs) = wrap $ liftL (c, putStr cs)

clear :: Free Console ()
clear = liftF $ liftL ('\3', ())

simple :: MonadIO m => Font -> Float -> m (String -> Picture)
simple font size = liftIO $ do
  r <- new (renderer font size :: Object (Request Char (Bitmap, V2 Float, V2 Float)) IO)
  t <- new $ iterative (typewriter (size * 1.2) ((r.-) . request) :: Object Console IO)
  return $ \s -> Picture $ applyVFX $ EmbedIO $ do
    t .- putStr s :: IO ()
    p <- t .- liftF (liftL id)
    t .- clear
    return $! unPicture p
