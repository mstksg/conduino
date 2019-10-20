{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeInType                 #-}

module Data.Conduino (
    Pipe
  , (.|)
  , runPipe
  , awaitEither, await, awaitSurely
  , repeatM, unfoldP, unfoldPForever, iterateP, sourceList
  , awaitForever, mapP, mapMP
  , dropP
  , foldrP, sinkList
  , ZipSink(..)
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Free.Class
import           Control.Monad.Free.TH
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Free        (FreeT(..), FreeF(..))
import           Control.Monad.Trans.Free.Church
import           Data.Conduino.Internal
import           Data.Foldable
import           Data.Void

await :: Pipe i o u m (Maybe i)
await = either Just (const Nothing) <$> awaitEither

awaitSurely :: Pipe i o Void m i
awaitSurely = either id absurd <$> awaitEither

runPipe :: Monad m => Pipe () Void u m a -> m a
runPipe = iterT go . pipeFree
  where
    go = \case
      PAwaitF f _ -> f ()
      PYieldF o _ -> absurd o

-- can this be done without going through FreeT?
(.|)
    :: Monad m
    => Pipe a b u m v
    -> Pipe b c v m r
    -> Pipe a c u m r
Pipe p .| Pipe q = Pipe $ toFT $ compPipe_ (fromFT p) (fromFT q)

compPipe_
    :: forall a b c u v m r. (Monad m)
    => FreeT (PipeF a b u) m v
    -> FreeT (PipeF b c v) m r
    -> FreeT (PipeF a c u) m r
compPipe_ p q = FreeT $ runFreeT q >>= \case
    Pure x             -> pure . Pure $ x
    Free (PAwaitF f g) -> runFreeT p >>= \case
      Pure x'              -> runFreeT $ compPipe_ p  (g x')
      Free (PAwaitF f' g') -> pure . Free $ PAwaitF ((`compPipe_` q) . f')
                                                    ((`compPipe_` q) . g')
      Free (PYieldF x' y') -> runFreeT $ compPipe_ y' (f x')
    Free (PYieldF x y) -> pure . Free $ PYieldF x (compPipe_ p y)
infixr 2 .|

unfoldP :: (b -> Maybe (a, b)) -> b -> Pipe i a u m ()
unfoldP f = go
  where
    go z = case f z of
      Nothing      -> pure ()
      Just (x, z') -> yield x *> go z'

unfoldPForever :: (b -> (a, b)) -> b -> Pipe i a u m r
unfoldPForever f = go
  where
    go z = yield x *> go z'
      where
        (x, z') = f z

iterateP :: (a -> a) -> a -> Pipe i a u m r
iterateP f = unfoldPForever (join (,) . f)

sourceList :: Foldable t => t a -> Pipe i a u m ()
sourceList = traverse_ yield

repeatM :: Monad m => m o -> Pipe i o u m u
repeatM x = go
  where
    go = (yield =<< lift x) *> go

awaitForever :: (i -> Pipe i o u m a) -> Pipe i o u m u
awaitForever f = go
  where
    go = awaitEither >>= \case
      Left x  -> f x *> go
      Right x -> pure x

mapP :: (a -> b) -> Pipe a b u m u
mapP f = awaitForever (yield . f)

mapMP :: Monad m => (a -> m b) -> Pipe a b u m u
mapMP f = awaitForever ((yield =<<) . lift . f)

dropP :: Int -> Pipe i o u m ()
dropP n = replicateM_ n await

foldrP :: (a -> b -> b) -> b -> Pipe a Void u m b
foldrP f z = go
  where
    go = await >>= \case
      Nothing -> pure z
      Just x  -> f x <$> go

sinkList :: Pipe i Void u m [i]
sinkList = foldrP (:) []

newtype ZipSink i u m a = ZipSink { getZipSink :: Pipe i Void u m a }
  deriving Functor

zipSink_
    :: Monad m
    => FreeT (PipeF i Void u) m (a -> b)
    -> FreeT (PipeF i Void u) m a
    -> FreeT (PipeF i Void u) m b
zipSink_ p q = FreeT $ go <$> runFreeT p <*> runFreeT q
  where
    go = \case
      Pure x             -> \case
        Pure x'              -> Pure $ x x'
        Free (PAwaitF f' g') -> Free $ PAwaitF (zipSink_ p . f') (zipSink_ p . g')
        Free (PYieldF x' _ ) -> absurd x'
      Free (PAwaitF f g) -> \case
        Pure _               -> Free $ PAwaitF ((`zipSink_` q) . f) ((`zipSink_` q) . g)
        Free (PAwaitF f' g') -> Free $ PAwaitF (zipSink_ <$> f <*> f') (zipSink_ <$> g <*> g')
        Free (PYieldF x' _ ) -> absurd x'
      Free (PYieldF x _) -> absurd x

altSink_
    :: Monad m
    => FreeT (PipeF i Void u) m a
    -> FreeT (PipeF i Void u) m a
    -> FreeT (PipeF i Void u) m a
altSink_ p q = FreeT $ go <$> runFreeT p <*> runFreeT q
  where
    go = \case
      Pure x             -> \_ -> Pure x
      Free (PAwaitF f g) -> \case
        Pure x'              -> Pure x'
        Free (PAwaitF f' g') -> Free $ PAwaitF (altSink_ <$> f <*> f') (altSink_ <$> g <*> g')
        Free (PYieldF x' _ ) -> absurd x'
      Free (PYieldF x _) -> absurd x

zipSink
    :: Monad m
    => Pipe i Void u m (a -> b)
    -> Pipe i Void u m a
    -> Pipe i Void u m b
zipSink (Pipe p) (Pipe q) = Pipe $ toFT $ zipSink_ (fromFT p) (fromFT q)

altSink
    :: Monad m
    => Pipe i Void u m a
    -> Pipe i Void u m a
    -> Pipe i Void u m a
altSink (Pipe p) (Pipe q) = Pipe $ toFT $ altSink_ (fromFT p) (fromFT q)

-- | '<*>' = distribute input to all, and return result when they finish
--
-- 'pure' = immediately finish
instance Monad m => Applicative (ZipSink i u m) where
    pure = ZipSink . pure
    ZipSink p <*> ZipSink q = ZipSink $ zipSink p q

-- | '<|>' = distribute input to all, and return the first result that
-- finishes
--
-- 'empty' = never finish
instance Monad m => Alternative (ZipSink i u m) where
    empty = ZipSink go
      where
        go = forever await
    ZipSink p <|> ZipSink q = ZipSink $ altSink p q
