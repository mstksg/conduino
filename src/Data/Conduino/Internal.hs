{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS_HADDOCK not-home            #-}

-- |
-- Module      : Data.Conduino.Internal
-- Copyright   : (c) Justin Le 2019
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Internal module exposing the internals of 'Pipe', including its
-- underlying representation and base functor.
--
module Data.Conduino.Internal (
    Pipe(..)
  , PipeF(..)
  , awaitEither
  , yield
  , trimapPipe, mapInput, mapOutput, mapUpRes
  , hoistPipe
  , RecPipe
  , toRecPipe, fromRecPipe
  ) where

import           Control.Monad.Except
import           Control.Monad.Free.Class
import           Control.Monad.Free.TH
import           Control.Monad.RWS
import           Control.Monad.Trans.Free        (FreeT(..))
import           Control.Monad.Trans.Free.Church

#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail
#endif

-- | Base functor of 'Pipe'.
--
-- A pipe fundamentally has the ability to await and the ability to yield.
-- The other functionality are implemented.
--
-- *  Lifting effects is implemented by the 'MonadTrans' and 'MonadIO'
--    instances that 'FT' gives.
-- *  /Ending/ with a result is implemented by the 'Applicative' instance's
--   'pure' that 'FT' gives.
-- *  Applicative and monadic sequenceing "after a pipe is done" is
--    implemented by the 'Applicative' and 'Monad' instances that 'FT'
--    gives.
--
-- On top of these we implement 'Data.Conduino..|' and other combinators
-- based on the structure that 'FT' gives.  For some functions, it can be
-- easier to use an alternative encoding, 'RecPipe', which is the same
-- thing but explicitly recursive.
data PipeF i o u a =
      PAwaitF (u -> a) (i -> a)
    | PYieldF o a
  deriving Functor

makeFree ''PipeF

-- | Similar to a conduit from the /conduit/ package.
--
-- For a @'Pipe' i o u m a@, you have:
--
-- *  @i@: Type of input stream (the things you can 'Data.Conduino.await')
-- *  @o@: Type of output stream (the things you 'yield')
-- *  @u@: Type of the /result/ of the upstream pipe (Outputted when
--    upstream pipe terminates)
-- *  @m@: Underlying monad (the things you can 'lift')
-- *  @a@: Result type when pipe terminates (outputted when finished, with
--    'pure' or 'return')
--
-- Some specializations:
--
-- *  If @i@ is @()@, the pipe is a /source/ --- it doesn't need anything
--    to produce items.  It will pump out items on its own, for pipes
--    downstream to receive and process.
--
-- *  If @o@ is 'Void'@, the pipe is a /sink/ --- it will never 'yield'
--    anything downstream.  It will consume items from things upstream, and
--    produce a result (@a@) if and when it terminates.
--
-- *  If @u@ is 'Void', then the pipe's upstream is limitless, and never
--    terminates.  This means that you can use 'Data.Condunio.awaitSurely'
--    instead of 'Data.Conduino.await', to get await a value that is
--    guaranteed to come.  You'll get an @i@ instead of a @'Maybe' i@.
--
-- *  If @a@ is 'Void', then the pipe never terminates --- it will keep on
--    consuming and/or producing values forever.  If this is a sink, it
--    means that the sink will never terminate, and so
--    'Data.Condunio.runPipe' will also never terminate.  If it is
--    a source, it means that if you chain something downstream with
--    'Data.Condunio..|', that downstream pipe can use 'awaitSurely' to
--    gaurantee something being passed down.
--
-- Applicative and Monadic sequencing of pipes chains by exhaustion.
--
-- @
-- do pipeX
--    pipeY
--    pipeZ
-- @
--
-- is a pipe itself, that behaves like @pipeX@ until it terminates, then
-- @pipeY@ until it terminates, then @pipeZ@ until it terminates.  The
-- 'Monad' instance allows you to choose "which pipe to behave like next"
-- based on the terminating result of a previous pipe.
--
-- @
-- do x <- pipeX
--    pipeBasedOn x
-- @
--
-- Usually you would use it by chaining together pipes with
-- 'Data.Condunio..|' and then running the result with
-- 'Data.Condunio.runPipe'.
--
-- @
-- 'Data.Conduino.runPipe' $ someSource
--        'Data.Conduino..|' somePipe
--        .| someOtherPipe
--        .| someSink
-- @
--
-- See 'Data.Condunio..|' and 'Data.Condunio.runPipe' for more information
-- on usage.
--
-- For a "prelude" of commonly used 'Pipe's, see
-- "Data.Condunio.Combinators".
--
newtype Pipe i o u m a = Pipe { pipeFree :: FT (PipeF i o u) m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadTrans
    , MonadFree (PipeF i o u)
    , MonadIO
    , MonadState s
    , MonadReader r
    , MonadWriter w
    , MonadError e
    , MonadRWS r w s
    )

instance MonadFail m => MonadFail (Pipe i o u m) where
#if MIN_VERSION_base(4,13,0)
    fail = lift . fail
#else
    fail = lift . Control.Monad.Fail.fail
#endif

-- | Await on upstream output.  Will block until it receives an @i@
-- (expected input type) or a @u@ if the upstream pipe terminates.
awaitEither :: Pipe i o u m (Either u i)
awaitEither = pAwaitF

-- | Send output downstream.
yield :: o -> Pipe i o u m ()
yield = pYieldF

-- | Map over the input type, output type, and upstream result type.
--
-- If you want to map over the result type, use 'fmap'.
trimapPipe
    :: (i -> j)
    -> (p -> o)
    -> (u -> v)
    -> Pipe j p v m a
    -> Pipe i o u m a
trimapPipe f g h = Pipe . transFT go . pipeFree
  where
    go = \case
      PAwaitF a b -> PAwaitF (a . h) (b . f)
      PYieldF a x -> PYieldF (g a) x

-- | Transform the underlying monad of a pipe.
hoistPipe
    :: (Monad m, Monad n)
    => (forall x. m x -> n x)
    -> Pipe i o u m a
    -> Pipe i o u n a
hoistPipe f = Pipe . hoistFT f . pipeFree

-- | (Contravariantly) map over the expected input type.
mapInput :: (i -> j) -> Pipe j o u m a -> Pipe i o u m a
mapInput f = trimapPipe f id id

-- | Map over the downstream output type.
--
-- If you want to map over the result type, use 'fmap'.
mapOutput :: (p -> o) -> Pipe i p u m a -> Pipe i o u m a
mapOutput f = trimapPipe id f id

-- | (Contravariantly) map over the upstream result type.
mapUpRes :: (u -> v) -> Pipe i o v m a -> Pipe i o u m a
mapUpRes = trimapPipe id id

-- | A version of 'Pipe' that uses explicit, concrete recursion instead of
-- church-encoding like 'Pipe'.  Some functions --- especially ones that
-- combine multiple pipes into one --- are easier to implement in this
-- form.
type RecPipe i o u = FreeT (PipeF i o u)

-- | Convert from a 'Pipe' to a 'RecPipe'.  While most of this library is
-- defined in terms of 'Pipe', it can be easier to write certain low-level
-- pipe combining functions in terms of 'RecPipe' than 'Pipe'.
toRecPipe :: Monad m => Pipe i o u m a -> RecPipe i o u m a
toRecPipe = fromFT . pipeFree

-- | Convert a 'RecPipe' back into a 'Pipe'.
fromRecPipe :: Monad m => RecPipe i o u m a -> Pipe i o u m a
fromRecPipe = Pipe . toFT
