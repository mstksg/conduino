{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}

module Data.Conduino.Combinators (
  -- * Sources
  -- ** Pure
  -- *** Infinite
    unfold
  , iterate
  , repeat
  -- *** Finite
  , unfoldMaybe
  , unfoldEither
  , iterateMaybe
  , iterateEither
  , sourceList
  , replicate
  -- ** Monadic
  -- *** Infinite
  , repeatM
  -- *** Finite
  , repeatMaybeM
  , repeatEitherM
  , replicateM
  -- * Pipes
  , map
  , mapM
  -- * Sinks
  , drop
  , foldr
  , foldl
  , sinkList
  , fold
  ) where

import           Control.Monad hiding      (mapM, replicateM)
import           Control.Monad.Trans.Class
import           Data.Conduino
import           Data.Either
import           Data.Foldable hiding      (foldr, foldl, fold)
import           Data.Maybe
import           Data.Void
import           Prelude hiding            (map, iterate, mapM, replicate, repeat, foldr, drop, foldl)

unfoldEither
    :: (s -> Either a (o, s))
    -> s
    -> Pipe i o u m a
unfoldEither f = go
  where
    go z = case f z of
      Left  r       -> pure r
      Right (x, z') -> yield x *> go z'

unfoldMaybe
    :: (s -> Maybe (o, s))
    -> s
    -> Pipe i o u m ()
unfoldMaybe f = unfoldEither (maybe (Left ()) Right . f)

unfold
    :: (s -> (o, s))
    -> s
    -> Pipe i o u m a
unfold f = go
  where
    go z = yield x *> go z'
      where
        (x, z') = f z

iterateEither
    :: (o -> Either a o)
    -> o
    -> Pipe i o u m a
iterateEither f = unfoldEither (fmap (join (,)) . f)

iterateMaybe
    :: (o -> Maybe o)
    -> o
    -> Pipe i o u m ()
iterateMaybe f = unfoldMaybe (fmap (join (,)) . f)

iterate
    :: (o -> o)
    -> o
    -> Pipe i o u m u
iterate f = unfold (join (,) . f)

sourceList :: Foldable t => t a -> Pipe i a u m ()
sourceList = traverse_ yield

repeat :: o -> Pipe i o u m u
repeat = forever . yield

replicate :: Int -> o -> Pipe i o u m ()
replicate n = replicateM_ n . yield

repeatEitherM
    :: Monad m
    => m (Either a o)
    -> Pipe i o u m a
repeatEitherM x = go
  where
    go = lift x >>= \case
      Left r  -> pure r
      Right y -> yield y *> go

repeatMaybeM
    :: Monad m
    => m (Maybe o)
    -> Pipe i o u m ()
repeatMaybeM = repeatEitherM . fmap (maybe (Left ()) Right)

replicateM
    :: Monad m
    => Int
    -> m o
    -> Pipe i o u m ()
replicateM n x = replicateM_ n $ lift x >>= yield

repeatM
    :: Monad m
    => m o
    -> Pipe i o u m u
repeatM x = go
  where
    go = (yield =<< lift x) *> go

map :: (a -> b) -> Pipe a b u m u
map f = awaitForever (yield . f)

mapM :: Monad m => (a -> m b) -> Pipe a b u m u
mapM f = awaitForever ((yield =<<) . lift . f)

foldr :: (a -> b -> b) -> b -> Pipe a Void u m b
foldr f z = go
  where
    go = await >>= \case
      Nothing -> pure z
      Just x  -> f x <$> go

foldl :: (b -> a -> b) -> b -> Pipe a Void u m b
foldl f = go
  where
    go !z = await >>= \case
      Nothing -> pure z
      Just !x -> go (f z x)

fold :: Monoid a => Pipe a Void u m a
fold = foldl (<>) mempty

sinkList :: Pipe i Void u m [i]
sinkList = foldr (:) []

drop :: Int -> Pipe i o u m ()
drop n = replicateM_ n await

