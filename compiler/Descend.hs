module Descend where

-- Inpsired by Neil Mitchell's Uniplate library.

import Control.Monad
import MonadInstances

class Descend a where
  descendM :: Monad m => (a -> m a) -> a -> m a

descend :: Descend a => (a -> a) -> a -> a
descend f a = runIdentity (descendM (return . f) a)

extract :: Descend a => (a -> [b]) -> a -> [b]
extract f = fst . runWriter . descendM (\a -> writeMany (f a) >> return a)

universe :: Descend a => a -> [a]
universe a = a : extract universe a

bottomup :: Descend a => (a -> a) -> a -> a
bottomup f = f . descend (bottomup f)

topdown :: Descend a => (a -> a) -> a -> a
topdown f = descend (topdown f) . f
