{-# LANGUAGE ConstraintKinds, FlexibleContexts, BangPatterns #-}
module Prefabricated.Text where
import Prelude hiding (putStr)
import Data.Graphics.Bitmap (Bitmap(..))
import Data.Graphics.Font
import Data.Graphics.Class
import Data.Graphics.Scene
import Data.Graphics.Sight
import Control.Lens
import Control.Monad.Objective
import Control.Monad.Operational.Mini
import Control.Monad.State.Class
import Control.Monad.Trans
import Control.Object
import Data.Functor.PushPull
import Data.Functor.Request
import Data.Monoid
import Linear
import Control.DeepSeq

renderer :: MonadIO m => Font -> Float -> Object (Request Char (Bitmap, V2 Float, V2 Float)) m
renderer font size = flyweight (liftIO . renderChar font size)

typewriter :: MonadIO m => Float -> (Char -> m (Bitmap, V2 Float, V2 Float)) -> Object (ReifiedProgram (PushPull Char Picture)) m
typewriter l req = sequential $ stateful go (V2 0 0, mempty) where
  go (Push '\3' cont) = do
    put (V2 0 0, mempty)
    return cont
  go (Push '\r' cont) = return cont
  go (Push '\n' cont) = do
    _1 . _y += l
    _1 . _x .= 0
    return cont
  go (Push ch cont) = do
    (pos, pic) <- get
    (!bmp@(Bitmap img _ _), !ofs, !adv) <- lift $ req ch
    return $! rnf img
    put (pos + adv, pic <> translate (pos + ofs) (bitmap bmp))
    return cont
  go (Pull cont) = uses _2 cont

putStr :: String -> ReifiedProgram (PushPull Char Picture) ()
putStr [] = return ()
putStr (c:cs) = Push c () :>>= const (putStr cs)

clear :: ReifiedProgram (PushPull Char Picture) ()
clear = Push '\3' () :>>= return

simple :: MonadIO m => Font -> Float -> m (String -> Picture)
simple font size = liftIO $ do
  r <- new (renderer font size :: Object (Request Char (Bitmap, V2 Float, V2 Float)) IO)
  t <- new (typewriter (size * 1.2) ((r.-) . request) :: Object (ReifiedProgram (PushPull Char Picture)) IO)
  return $ \s -> Picture $ applyVFX $ EmbedIO $ do
    t .- putStr s :: IO ()
    p <- t .- (Pull id :>>= return)
    t .- clear
    return $! unPicture p
