module MonadInstances where

import Control.Monad

-- Identity Monad

newtype Identity a = I { runIdentity :: a }

instance Monad Identity where
  return a = I a
  I a >>= f = f a

instance Functor Identity where
  fmap = liftM

instance Applicative Identity where
  pure = return
  (<*>) = ap

-- Writer Monad

data Writer w a = W [w] a

instance Monad (Writer w) where
  return a = W [] a
  W w0 a0 >>= f = case f a0 of W w1 a1 -> W (w0 ++ w1) a1

instance Functor (Writer w) where
  fmap = liftM

instance Applicative (Writer w) where
  pure = return
  (<*>) = ap

runWriter :: Writer w a -> ([w], a)
runWriter (W ws a) = (ws, a)

write :: w -> Writer w ()
write w = W [w] ()

writeMany :: [w] -> Writer w ()
writeMany ws = W ws ()

-- Fresh Monad

data Fresh a = Fresh { runFresh :: String -> Int -> (Int, a) }

instance Monad Fresh where
  return a = Fresh (\s i -> (i, a))
  m >>= f  = Fresh (\s i -> case runFresh m s i of
                              (j, a) -> runFresh (f a) s j)

instance Functor Fresh where
  fmap = liftM

instance Applicative Fresh where
  pure = return
  (<*>) = ap

fresh :: Fresh String
fresh = Fresh (\s i -> (i+1, s ++ show i))

next :: Fresh Int
next = Fresh (\s i -> (i+1, i))
