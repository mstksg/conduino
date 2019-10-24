{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeInType                 #-}

module Data.Conduino (
    Pipe
  , (.|)
  , runPipe
  , awaitEither, await, awaitSurely, awaitForever, yield
  , mapInput, mapOutput, mapUpRes, trimapPipe
  , ZipSink(..)
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Free        (FreeT(..), FreeF(..))
import           Control.Monad.Trans.Free.Church
import           Data.Conduino.Internal
import           Data.Void

-- | Await input from upstream.  Will block until upstream 'yield's.
--
-- Will return 'Nothing' if the upstream pipe finishes and terminates.
--
-- If the upstream pipe never terminates, then you can use 'awaitSurely' to
-- guarantee a result.
await :: Pipe i o u m (Maybe i)
await = either (const Nothing) Just <$> awaitEither

-- | Await input from upstream where the upstream pipe is guaranteed to
-- never terminate.
awaitSurely :: Pipe i o Void m i
awaitSurely = either absurd id <$> awaitEither

-- | A useful utility function over repeated 'await's.  Will repeatedly
-- 'await' and then continue with the given pipe whenever the upstream pipe
-- yields.
--
-- Can be used to implement many pipe combinators:
--
-- @
-- 'Data.Conduino.Combinators.map' f = 'awaitForever' $ \x -> 'yield' (f x)
-- @
awaitForever :: (i -> Pipe i o u m a) -> Pipe i o u m u
awaitForever = awaitForeverWith pure

-- | 'awaitForever', but with a way to handle the result of the
-- upstream pipe, which will be called when the upstream pipe stops
-- producing.
awaitForeverWith
    :: (u -> Pipe () o u m b)       -- ^ how to handle upstream ending, transitioning to a source
    -> (i -> Pipe i o u m a)        -- ^ how to handle upstream output
    -> Pipe i o u m b
awaitForeverWith f g = go
  where
    go = awaitEither >>= \case
      Left x  -> mapInput (const ()) $ f x
      Right x -> g x *> go

-- | Run a pipe that is both a source and a sink into the effect that it
-- represents.
--
-- Usually you wouild construct this using something like:
--
-- @
-- 'runPipe' $ someSource
--        '.|' somePipe
--        '.|' someOtherPipe
--        '.|' someSink
-- @
--
-- 'runPipe' will produce the result of that sink.
runPipe :: Monad m => Pipe () Void u m a -> m a
runPipe = iterT go . pipeFree
  where
    go = \case
      PAwaitF _ f -> f ()
      PYieldF o _ -> absurd o

-- | The main operator for chaining pipes together.  @pipe1 .| pipe2@ will
-- connect the output of @pipe1@ to the input of @pipe2@.
--
-- "Running" a pipe will draw from @pipe2@, and if @pipe2@ ever asks for
-- input (with 'await' or something similar), it will block until @pipe1@
-- outputs something (or signals termination).
--
-- The structure of a full pipeline usually looks like:
--
-- @
-- 'runPipe' $ someSource
--        '.|' somePipe
--        '.|' someOtherPipe
--        '.|' someSink
-- @
--
-- Where you route a source into a series of pipes, which eventually ends
-- up at a sink.  'runPipe' will then produce the result of that sink.
(.|)
    :: Monad m
    => Pipe a b u m v
    -> Pipe b c v m r
    -> Pipe a c u m r
Pipe p .| Pipe q = Pipe $ toFT $ compPipe_ (fromFT p) (fromFT q)
infixr 2 .|

compPipe_
    :: forall a b c u v m r. (Monad m)
    => RecPipe a b u m v
    -> RecPipe b c v m r
    -> RecPipe a c u m r
compPipe_ p q = FreeT $ runFreeT q >>= \qq -> case qq of
    Pure x             -> pure . Pure $ x
    Free (PAwaitF f g) -> runFreeT p >>= \pp -> case pp of 
      Pure x'              -> runFreeT $ compPipe_ (FreeT (pure pp)) (f x')
      Free (PAwaitF f' g') -> pure . Free $ PAwaitF ((`compPipe_` FreeT (pure qq)) . f')
                                                    ((`compPipe_` FreeT (pure qq)) . g')
      Free (PYieldF x' y') -> runFreeT $ compPipe_ y' (g x')
    Free (PYieldF x y) -> pure . Free $ PYieldF x (compPipe_ p y)

-- | A newtype wrapper over a source (@'Pipe' () 'o' 'Void'@) that gives it an
-- alternative 'Applicative' and 'Alternative' instance.
--
-- '<*>' will pair up each output that the sources produce: if you 'await'
-- a value from downstream, it will wait until both paired sources yield
-- before passing them on together.
--
-- '<|>' will completely exhaust the first source before moving on to the
-- next source.
--
-- This is similar to a ListT type: use 'lift' to lift up actions to yield,
-- as described in
-- <http://www.haskellforall.com/2014/11/how-to-build-library-agnostic-streaming.html>,
-- and '<|>' to chain successive actions one after the other.
newtype ZipSource m a = ZipSource { getZipSource :: Pipe () a Void m () }

zipSource_
    :: forall a b m. Monad m
    => RecPipe () (a -> b) Void m ()
    -> RecPipe () a        Void m ()
    -> RecPipe () b        Void m ()
zipSource_ p q = FreeT $ runFreeT p >>= \pp -> case pp of
    Pure _             -> pure . Pure $ ()    -- choice to short-circuit?
    Free (PAwaitF _ g) -> runFreeT $ g () `zipSource_` q
    Free (PYieldF x y) -> runFreeT q >>= \case
      Pure _               -> pure . Pure $ ()
      Free (PAwaitF _  g') -> runFreeT $ FreeT (pure pp) `zipSource_` g' ()
      Free (PYieldF x' y') -> pure . Free $ PYieldF (x x') (y `zipSource_` y')

instance Functor (ZipSource m) where
    fmap f = ZipSource . mapOutput f . getZipSource

instance Monad m => Applicative (ZipSource m) where
    pure = ZipSource . yield
    ZipSource p <*> ZipSource q = ZipSource . fromRecPipe $
      zipSource_ (toRecPipe p) (toRecPipe q)

instance Monad m => Alternative (ZipSource m) where
    empty = ZipSource $ pure ()
    ZipSource p <|> ZipSource q = ZipSource (p *> q)




-- | A newtype wrapper over a sink (@'Pipe' i 'Void'@) that gives it an
-- alternative 'Applicative' and 'Alternative' instance.
--
-- '<*>' will distribute input over both sinks, and output a final result
-- once both sinks finish.
--
-- '<|>' will distribute input over both sinks, and output a final result
-- as soon as one or the other finishes.
newtype ZipSink i u m a = ZipSink { getZipSink :: Pipe i Void u m a }
  deriving Functor

zipSink_
    :: Monad m
    => RecPipe i Void u m (a -> b)
    -> RecPipe i Void u m a
    -> RecPipe i Void u m b
zipSink_ p q = FreeT $ runFreeT p >>= \pp -> case pp of
    Pure x             -> runFreeT q >>= \case
      Pure x'              -> pure . Pure $ x x'
      Free (PAwaitF f' g') -> pure . Free $
        PAwaitF (zipSink_ (FreeT (pure pp)) . f')
                (zipSink_ (FreeT (pure pp)) . g')
      Free (PYieldF x' _ ) -> absurd x'
    Free (PAwaitF f g) -> runFreeT q >>= \qq -> case qq of
      Pure _               -> pure . Free $
        PAwaitF ((`zipSink_` FreeT (pure qq)) . f)
                ((`zipSink_` FreeT (pure qq)) . g)
      Free (PAwaitF f' g') -> pure . Free $
        PAwaitF (zipSink_ <$> f <*> f') (zipSink_ <$> g <*> g')
      Free (PYieldF x' _ ) -> absurd x'
    Free (PYieldF x _) -> absurd x
                                               
altSink_
    :: Monad m
    => RecPipe i Void u m a
    -> RecPipe i Void u m a
    -> RecPipe i Void u m a
altSink_ p q = FreeT $ runFreeT p >>= \case
    Pure x             -> pure . Pure $ x
    Free (PAwaitF f g) -> runFreeT q >>= \case
      Pure x'              -> pure . Pure $ x'
      Free (PAwaitF f' g') -> pure . Free $ PAwaitF (altSink_ <$> f <*> f') (altSink_ <$> g <*> g')
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
