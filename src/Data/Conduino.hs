{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE ViewPatterns               #-}

-- |
-- Module      : Data.Conduino
-- Copyright   : (c) Justin Le 2019
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Base API for 'Pipe'.  See documentation for 'Pipe', '.|', and 'runPipe'
-- for information on usage.
--
-- A "prelude" of useful pipes can be found in "Data.Conduino.Combinators".
--
-- == Why a stream processing library?
-- 
-- A stream processing library is a way to stream processors in a /composable/ way:
-- instead of defining your entire stream processing function as a single
-- recursive loop with some global state, instead think about each "stage" of the process,
-- and isolate each state to its own segment.  Each component can contain its own
-- isolated state:
-- 
-- >>> runPipePure $ sourceList [1..10]
--       .| scan (+) 0
--       .| sinkList
-- [1,3,6,10,15,21,28,36,45,55]
-- 
-- All of these components have internal "state":
-- 
-- *   @sourceList@ keeps track of "which" item in the list to yield next
-- *   @scan@ keeps track of the current running sum
-- *   @sinkList@ keeps track of all items that have been seen so far, as a list
-- 
-- They all work together without knowing any other component's internal state, so
-- you can write your total streaming function without concerning yourself, at
-- each stage, with the entire part.
-- 
-- In addition, there are useful functions to "combine" stream processors:
-- 
-- *   'zipSink' combines sinks in an "and" sort of way: combine two sinks in
--     parallel and finish when all finish.
-- *   'altSink' combines sinks in an "or" sort of way: combine two sinks in
--     parallel and finish when any of them finish
-- *   'zipSource' combines sources in parallel and collate their outputs.
-- 
-- Stream processing libraries are also useful for streaming composition of
-- monadic effects (like IO or State), as well.
--
module Data.Conduino (
    Pipe
  , (.|)
  , runPipe, runPipePure
  , awaitEither, await, awaitWith, awaitSurely, awaitForever, yield
  , mapInput, mapOutput, mapUpRes, trimapPipe
  -- * Wrappers
  , ZipSource(..)
  , unconsZipSource
  , ZipSink(..)
  , zipSink, altSink
  -- * Generators
  , toListT, fromListT
  , pattern PipeList
  , withSource, genSource
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Free        (FreeT(..), FreeF(..))
import           Control.Monad.Trans.Free.Church
import           Data.Conduino.Internal
import           Data.Functor
import           Data.Functor.Identity
import           Data.Void
import           List.Transformer                (ListT(..), Step(..))
import qualified List.Transformer                as LT

-- | Await input from upstream.  Will block until upstream 'yield's.
--
-- Will return 'Nothing' if the upstream pipe finishes and terminates.
--
-- If the upstream pipe never terminates, then you can use 'awaitSurely' to
-- guarantee a result.
--
-- Will always return 'Just' if @u@ is 'Void'.
await :: Pipe i o u m (Maybe i)
await = either (const Nothing) Just <$> awaitEither

-- | 'await', but directly chaining a continuation if the 'await' was
-- succesful.
--
-- The await will always be succesful if @u@ is 'Void'.
--
-- This is a way of writing code in a way that is agnostic to how the
-- upstream pipe terminates.
awaitWith :: (i -> Pipe i o u m u) -> Pipe i o u m u
awaitWith f = awaitEither >>= \case
    Left  r -> pure r
    Right x -> f x

-- | Await input from upstream where the upstream pipe is guaranteed to
-- never terminate.
--
-- A common type error will occur if @u@ (upstream pipe result type) is not
-- 'Void' -- it might be @()@ or some non-'Void' type.  This means that the
-- upstream pipe terminates, so awaiting cannot be assured.
--
-- In that case, either change your upstream pipe to be one that never
-- terminates (which is most likely not possible), or use 'await' instead
-- of 'awaitSurely'.
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

-- | Run a pipe that is both a source and a sink (an "effect") into the
-- effect that it represents.
--
-- Usually you wouild construct this using something like:
--
-- @
-- 'runPipe' $ someSource
--        '.|' somePipe
--        .| someOtherPipe
--        .| someSink
-- @
--
-- 'runPipe' will produce the result of that final sink.
--
-- Some common errors you might receive:
--
-- *  @i@ is not @()@: If you give a pipe where the first parameter
--    ("input") is not @()@, it means that your pipe is not a producer.
--    Pre-compose it (using '.|') with a producer of the type you need.
--
--    For example, if you have a @myPipe :: 'Pipe' 'Int' o u m a@, this is
--    a pipe that is awaiting 'Int's from upstream.  Pre-compose with
--    a producer of 'Int's, like @'Data.Conduino.Combinators.sourceList'
--    [1,2,3] '.|' myPipe@, in order to be able to run it.
--
-- *  @o@ is not 'Void': If you give a pipe where the second parameter
--    ("output") is not 'Void', it means that your pipe is not a consumer.
--    Post-compose it (using '.|') with a consumer of the type you need.
--
--    For example, if you have @myPipe :: 'Pipe' i 'Int' u m a@, this is
--    a pipe that is yielding 'Int's downstream that are going unhandled.
--    Post-compose it a consumer of 'Int's, like @myPipe '.|'
--    'Data.Conduino.foldl' (+) 0@, in order to be able to run it.
--
--    If you just want to ignore all downstream yields, post-compose with
--    'Data.Conduino.Combinators.sinkNull'.
--
runPipe :: Monad m => Pipe () Void u m a -> m a
runPipe = iterT go . pipeFree
  where
    go = \case
      PAwaitF _ f -> f ()
      PYieldF o _ -> absurd o

-- | 'runPipe' when the underlying monad is 'Identity', and so has no
-- effects.
runPipePure :: Pipe () Void Void Identity a -> a
runPipePure = runIdentity . runPipe

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
--        .| someOtherPipe
--        .| someSink
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

-- | A newtype wrapper over a source (@'Pipe' () o 'Void'@) that gives it an
-- alternative 'Applicative' and 'Alternative' instance, matching "ListT
-- done right".
--
-- '<*>' will pair up each output that the sources produce: if you 'await'
-- a value from downstream, it will wait until both paired sources yield
-- before passing them on together.
--
-- '<|>' will completely exhaust the first source before moving on to the
-- next source.
--
-- 'ZipSource' is effectively equivalent to "ListT done right", the true
-- List Monad transformer.  '<|>' is concatentation.  You can use this type
-- with 'lift' to lift a yielding action and '<|>' to sequence yields to
-- implement the pattern described in
-- <http://www.haskellforall.com/2014/11/how-to-build-library-agnostic-streaming.html>,
-- where you can write streaming producers in a polymorphic way, and have
-- it run with pipes, conduit, etc.
--
-- The main difference is that its 'Applicative' instance ("zipping") is
-- different from the traditional 'Applicative' instance for 'ListT'
-- ("all combinations").  Effectively this becomes like a "zipping"
-- 'Applicative' instance for 'ListT'.
--
-- If you want a 'Monad' (or 'Control.Monad.IO.Class.MonadIO') instance,
-- use 'ListT' instead, and convert using 'toListT'/'fromListT' or the
-- 'PipeList' pattern/constructor.
newtype ZipSource m a = ZipSource { getZipSource :: Pipe () a Void m () }

-- | A source is equivalent to a 'ListT' producing a 'Maybe'; this pattern
-- synonym lets you treat it as such.  It essentialyl wraps over 'toListT'
-- and 'fromListT'.
pattern PipeList :: Monad m => ListT m (Maybe a) -> Pipe () a u m ()
pattern PipeList xs <- (toListT->xs)
  where
    PipeList xs = fromListT xs
{-# COMPLETE PipeList #-}

instance Functor (ZipSource m) where
    fmap f = ZipSource . mapOutput f . getZipSource

instance Monad m => Applicative (ZipSource m) where
    pure = ZipSource . yield
    ZipSource (PipeList fs) <*> ZipSource (PipeList xs) = ZipSource . PipeList . fmap Just $
            uncurry ($)
        <$> LT.zip (concatListT fs) (concatListT xs)

concatListT :: Monad m => ListT m (Maybe a) -> ListT m a
concatListT xs = ListT $ next xs >>= \case
    Nil              -> pure Nil
    Cons Nothing  ys -> next (concatListT ys)
    Cons (Just y) ys -> pure $ Cons y (concatListT ys)

instance Monad m => Alternative (ZipSource m) where
    empty = ZipSource $ pure ()
    ZipSource p <|> ZipSource q = ZipSource (p *> q)

instance MonadTrans ZipSource where
    lift = ZipSource . (yield =<<) . lift

-- | A source is essentially equivalent to 'ListT' producing a 'Maybe'
-- result.  This converts it to the 'ListT' it encodes.
--
-- See 'ZipSource' for a wrapper over 'Pipe' that gives the right 'Functor'
-- and 'Alternative' instances.
toListT
    :: Applicative m
    => Pipe () o u m ()
    -> ListT m (Maybe o)
toListT p = ListT $ runFT (pipeFree p)
    (\_ -> pure Nil)
    (\pNext -> \case
        PAwaitF _ g -> pure $ Cons Nothing  (ListT . pNext $ g ())
        PYieldF x y -> pure $ Cons (Just x) (ListT . pNext $ y   )
    )

-- | A source is essentially 'ListT' producing a 'Maybe' result.  This
-- converts a 'ListT' to the source it encodes.
--
-- See 'ZipSource' for a wrapper over 'Pipe' that gives the right 'Functor'
-- and 'Alternative' instances.
fromListT
    :: Monad m
    => ListT m (Maybe o)
    -> Pipe i o u m ()
fromListT = fromRecPipe . go
  where
    go xs = FreeT $ next xs >>= \case
      Nil              -> pure . Pure $ ()
      Cons Nothing  ys -> pure . Free $ PAwaitF (\_ -> pure ()) $ \_ -> go ys
      Cons (Just y) ys -> pure . Free $ PYieldF y (go ys)

---- | A source is essentially equiavlent to 'ListT'.  This converts
---- a 'ListT' to the source it encodes.
----
---- See 'ZipSource' for a wrapper over 'Pipe' that gives the right 'Functor'
---- and 'Alternative' instances.
--fromListT
--    :: Monad m
--    => ListT m o
--    -> Pipe i o u m ()
--fromListT = fromRecPipe . go
--  where
--    go xs = FreeT $ next xs >>= \case
--      Nil       -> pure . Pure $ ()
--      Cons y ys -> pure . Free $ PYieldF y (go ys)

-- | Given a "generator" of @o@ in @m@, return a /source/ that that
-- generator encodes.  Is the inverse of 'withSource'.
--
-- The generator is essentially a church-encoded 'ListT'.
genSource
    :: (forall r. (Maybe (o, m r) -> m r) -> m r)
    -> Pipe i o u m ()
genSource f = Pipe $ FT $ \pDone pFree -> f $ \case
    Nothing      -> pDone ()
    Just (x, xs) -> pFree id (PYieldF x xs)

-- | A source can be "run" by providing a continuation to handle and
-- sequence each of its outputs.  Is ths inverse of 'genSource'.
--
-- This essentially turns a pipe into a church-encoded 'ListT'.
withSource
    :: Pipe () o u m ()
    -> (Maybe (o, m r) -> m r)    -- ^ handler ('Nothing' = done, @'Just' (x, next)@ = yielded value and next action
    -> m r
withSource p f = runFT (pipeFree p)
    (\_ -> f Nothing)
    (\pNext -> \case
        PAwaitF _ g -> pNext $ g ()
        PYieldF x y -> f (Just (x, pNext y))
    )

-- | 'ZipSource' is effectively 'ListT' returning a 'Maybe'.  As such, you
-- can use 'unconsZipSource' to "peel off" the first yielded item, if it
-- exists, and return the "rest of the list".
unconsZipSource
    :: Monad m
    => ZipSource m a
    -> m (Maybe (Maybe a, ZipSource m a))
unconsZipSource (ZipSource (PipeList p)) = next p <&> \case
    Cons x xs -> Just (x, ZipSource (PipeList xs))
    Nil       -> Nothing

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
    Free (PAwaitF f g) -> runFreeT q <&> \case
      Pure x'              -> Pure x'
      Free (PAwaitF f' g') -> Free $ PAwaitF (altSink_ <$> f <*> f') (altSink_ <$> g <*> g')
      Free (PYieldF x' _ ) -> absurd x'
    Free (PYieldF x _) -> absurd x

-- | Distribute input to both sinks, and finishes with the final result
-- once both finish.
--
-- Forms an identity with 'pure'.
zipSink
    :: Monad m
    => Pipe i Void u m (a -> b)
    -> Pipe i Void u m a
    -> Pipe i Void u m b
zipSink (Pipe p) (Pipe q) = Pipe $ toFT $ zipSink_ (fromFT p) (fromFT q)

-- | Distribute input to both sinks, and finishes with the result of
-- the one that finishes first.
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

instance MonadTrans (ZipSink i u) where
    lift = ZipSink . lift
