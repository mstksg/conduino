{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Data.Conduino (
    Pipe
  -- , type (.|)
  , runPipe
  , awaitEither, await, awaitSurely, awaitForever, yield
  -- , mapInput, mapOutput, mapUpRes, trimapPipe
  -- , ZipSink(..)
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Free.Class
import           Control.Monad.Free.TH
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Free        (FreeT(..), FreeF(..))
import           Control.Monad.Trans.Free.Church
import           Data.Coerce
import           Data.Conduino.Internal
import           Data.Foldable
import           Data.Type.Equality
import           Data.Void
import           GHC.TypeLits
import           Unsafe.Coerce

-- | Await input from upstream.  Will block until upstream 'yield's.
--
-- Will return 'Nothing' if the upstream pipe finishes and terminates.
--
-- If the upstream pipe never terminates, then you can use 'awaitSurely' to
-- guarantee a result.
await :: Pipe pt u m (Maybe (InputOf pt))
await = either (const Nothing) Just <$> awaitEither

-- | Await input from upstream where the upstream pipe is guaranteed to
-- never terminate.
awaitSurely :: Pipe pt Void m (InputOf pt)
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
awaitForever :: (InputOf pt -> Pipe pt u m a) -> Pipe pt u m u
awaitForever = awaitForeverWith pure

-- | 'awaitForever', but with a way to handle the result of the
-- upstream pipe, which will be called when the upstream pipe stops
-- producing.
awaitForeverWith
    :: (u -> Pipe pt u m b)       -- ^ how to handle upstream ending, transitioning to a source
    -> (InputOf pt -> Pipe pt u m a)        -- ^ how to handle upstream output
    -> Pipe pt u m b
awaitForeverWith f g = undefined
  where
    -- go = awaitEither >>= \case
    --   Left x  -> mapInput (const ()) $ f x
    --   Right x -> g x *> go

convertPipe
    :: (InputOf pt ~ InputOf qt, OutputOf pt ~ OutputOf qt)
    => Pipe pt u m a
    -> Pipe qt u m a
convertPipe = coerce

-- class Is pt qt where
--     convertPipe :: Pipe pt u m a -> Pipe qt u m a

--     default
--     convertPipe = coerce

-- instance Is Effect (Sink ())

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
runPipe :: Monad m => Pipe Effect u m a -> m a
runPipe = iterT go . pipeFree
  where
    go = \case
      PAwaitF _ f -> f ()
      PYieldF o _ -> absurd o

class (OutputOf pt ~ InputOf qt, InputOf pt ~ InputOf (pt .| qt), OutputOf qt ~ OutputOf (pt .| qt)) => Pipeable pt qt where
    type pt .| qt :: PipeType

instance Pipeable (Source a) (Conduit a b) where
    type Source a .| Conduit a b = Source b

instance Pipeable (Source a) (Sink a) where
    type Source a .| Sink a = Effect

instance Pipeable (Conduit a b) (Sink b) where
    type Conduit a b .| Sink b = Sink a

instance Pipeable (Conduit a b) (Conduit b c) where
    type Conduit a b .| Conduit b c = Conduit a c

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
    :: forall pt qt rt u m v r. (Pipeable pt qt, Monad m)
    => Pipe pt         u m v
    -> Pipe qt         v m r
    -> Pipe (pt .| qt) u m r
Pipe p .| Pipe q = Pipe $ toFT $ compPipe_ (fromFT p) (fromFT q)
infixr 2 .|

compPipe_
    :: forall a b c u v m r. (Monad m)
    => RecPipe a b u m v
    -> RecPipe b c v m r
    -> RecPipe a c u m r
compPipe_ p q = FreeT $ runFreeT q >>= \case
    Pure x             -> pure . Pure $ x
    Free (PAwaitF f g) -> runFreeT p >>= \case
      Pure x'              -> runFreeT $ compPipe_ p  (f x')
      Free (PAwaitF f' g') -> pure . Free $ PAwaitF ((`compPipe_` q) . f')
                                                    ((`compPipe_` q) . g')
      Free (PYieldF x' y') -> runFreeT $ compPipe_ y' (g x')
    Free (PYieldF x y) -> pure . Free $ PYieldF x (compPipe_ p y)

---- | A newtype wrapper over a sink (@'Pipe' i 'Void'@) that gives it an
---- alternative 'Applicative' and 'Alternative' instance.
----
---- '<*>' will distribute input over both sinks, and output a final result
---- once both sinks finish.
----
---- '<|>' will distribute input over both sinks, and output a final result
---- as soon as one or the other finishes.
--newtype ZipSink i u m a = ZipSink { getZipSink :: Pipe i Void u m a }
--  deriving Functor

--zipSink_
--    :: Monad m
--    => RecPipe i Void u m (a -> b)
--    -> RecPipe i Void u m a
--    -> RecPipe i Void u m b
--zipSink_ p q = FreeT $ go <$> runFreeT p <*> runFreeT q
--  where
--    go = \case
--      Pure x             -> \case
--        Pure x'              -> Pure $ x x'
--        Free (PAwaitF f' g') -> Free $ PAwaitF (zipSink_ p . f') (zipSink_ p . g')
--        Free (PYieldF x' _ ) -> absurd x'
--      Free (PAwaitF f g) -> \case
--        Pure _               -> Free $ PAwaitF ((`zipSink_` q) . f) ((`zipSink_` q) . g)
--        Free (PAwaitF f' g') -> Free $ PAwaitF (zipSink_ <$> f <*> f') (zipSink_ <$> g <*> g')
--        Free (PYieldF x' _ ) -> absurd x'
--      Free (PYieldF x _) -> absurd x

--altSink_
--    :: Monad m
--    => RecPipe i Void u m a
--    -> RecPipe i Void u m a
--    -> RecPipe i Void u m a
--altSink_ p q = FreeT $ go <$> runFreeT p <*> runFreeT q
--  where
--    go = \case
--      Pure x             -> \_ -> Pure x
--      Free (PAwaitF f g) -> \case
--        Pure x'              -> Pure x'
--        Free (PAwaitF f' g') -> Free $ PAwaitF (altSink_ <$> f <*> f') (altSink_ <$> g <*> g')
--        Free (PYieldF x' _ ) -> absurd x'
--      Free (PYieldF x _) -> absurd x

--zipSink
--    :: Monad m
--    => Pipe i Void u m (a -> b)
--    -> Pipe i Void u m a
--    -> Pipe i Void u m b
--zipSink (Pipe p) (Pipe q) = Pipe $ toFT $ zipSink_ (fromFT p) (fromFT q)

--altSink
--    :: Monad m
--    => Pipe i Void u m a
--    -> Pipe i Void u m a
--    -> Pipe i Void u m a
--altSink (Pipe p) (Pipe q) = Pipe $ toFT $ altSink_ (fromFT p) (fromFT q)

---- | '<*>' = distribute input to all, and return result when they finish
----
---- 'pure' = immediately finish
--instance Monad m => Applicative (ZipSink i u m) where
--    pure = ZipSink . pure
--    ZipSink p <*> ZipSink q = ZipSink $ zipSink p q

---- | '<|>' = distribute input to all, and return the first result that
---- finishes
----
---- 'empty' = never finish
--instance Monad m => Alternative (ZipSink i u m) where
--    empty = ZipSink go
--      where
--        go = forever await
--    ZipSink p <|> ZipSink q = ZipSink $ altSink p q
