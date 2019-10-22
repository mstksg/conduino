{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}

module Data.Conduino.Internal (
    Pipe(..)
  , PipeF(..)
  , awaitEither
  , yield
  , trimapPipe, mapInput, mapOutput, mapUpRes
  , RecPipe
  , toRecPipe, fromRecPipe
  -- , (.|)
  -- , runPipe
  -- , awaitEither, await, awaitSurely
  -- , repeatM, unfoldP, unfoldPForever, iterateP, sourceList
  -- , awaitForever, mapP, mapMP
  -- , dropP
  -- , foldrP, sinkList
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
import           Data.Foldable
import           Data.Kind
import           Data.Void

data PipeF i o u a =
      PAwaitF (u -> a) (i -> a)
    | PYieldF o a
  deriving Functor

makeFree ''PipeF

data PipeType = Source  Type
              | Conduit Type Type
              | Sink    Type
              | Effect

type family PTInput pt :: Type where
    PTInput ('Source    o) = ()
    PTInput ('Conduit i o) = i
    PTInput ('Sink    i  ) = i
    PTInput  'Effect       = ()

type family PTOutput pt :: Type where
    PTOutput ('Source    o) = o
    PTOutput ('Conduit i o) = o
    PTOutput ('Sink    i  ) = Void
    PTOutput  'Effect       = Void

-- | Similar to Conduit
--
-- *  @i@: Type of input stream
-- *  @o@: Type of output stream
-- *  @u@: Type of the /result/ of the upstream pipe (Outputted when
--    upstream pipe finishes)
-- *  @m@: Underlying monad
-- *  @a@: Result type (Outputted when finished)
--
-- Some specializations:
--
-- *  A pipe is a /source/ if @i@ is '()': it doesn't need anything to go
--    pump out items.  If a pipe is source and @a@ is 'Void', it means that
--    it will produce forever.
--
-- *  A pipe is a /sink/ if @o@ is 'Void': it will never yield anything
--    else downstream.
--
-- *  If a pipe is both a source and a sink, it is an /effect/.
--
-- *  Normally you can ask for input upstream with 'await', which returns
--    'Nothing' if the pipe upstream stops producing.  However, if @u@ is
--    'Void', it means that the pipe upstream will never stop, so you can
--    use 'awaitSurely' to get a guaranteed answer.
newtype Pipe (pt :: PipeType) u m a = Pipe { pipeFree :: FT (PipeF (PTInput pt) (PTOutput pt) u) m a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadTrans
           -- , MonadFree (PipeF i o u)
           )

-- | Await on upstream output.  Will block until it receives an @i@
-- (expected input type) or a @u@ if the upstream pipe terminates.
awaitEither :: (PTInput pt ~ i) => Pipe pt u m (Either u i)
awaitEither = Pipe pAwaitF

-- | Send output downstream.
yield :: (PTOutput pt ~ o) => o -> Pipe pt u m ()
yield = Pipe . pYieldF


-- -- | Await on upstream output.  Will block until it receives an @i@
-- -- (expected input type) or a @u@ if the upstream pipe terminates.
-- awaitEither :: Pipe i o u m (Either u i)
-- awaitEither = pAwaitF

---- | Send output downstream.
--yield :: o -> Pipe i o u m ()
--yield = pYieldF

-- | Map over the input type, output type, and upstream result type.
--
-- If you want to map over the result type, use 'fmap'.
trimapPipe
    :: forall pt qt u v m a. ()
    => (PTInput qt -> PTInput pt)
    -> (PTOutput pt -> PTOutput qt)
    -> (u -> v)
    -> Pipe pt v m a
    -> Pipe qt u m a
trimapPipe f g h = Pipe . transFT go . pipeFree
  where
    go :: PipeF (PTInput pt) (PTOutput pt) v x -> PipeF (PTInput qt) (PTOutput qt) u x
    go = \case
      PAwaitF a b -> PAwaitF (a . h) (b . f)
      PYieldF a x -> PYieldF (g a) x

---- | (Contravariantly) map over the expected input type.
--mapInput :: (i -> j) -> Pipe j o u m a -> Pipe i o u m a
--mapInput f = trimapPipe f id id

---- | Map over the downstream output type.
----
---- If you want to map over the result type, use 'fmap'.
--mapOutput :: (p -> o) -> Pipe i p u m a -> Pipe i o u m a
--mapOutput f = trimapPipe id f id

---- | (Contravariantly) map over the upstream result type.
--mapUpRes :: (u -> v) -> Pipe i o v m a -> Pipe i o u m a
--mapUpRes = trimapPipe id id

---- | A version of 'Pipe' that uses explicit, concrete recursion instead of
---- church-encoding like 'Pipe'.  Some functions --- especially ones that
---- combine multiple pipes into one --- are easier to implement in this
---- form.
--type RecPipe i o u = FreeT (PipeF i o u)


---- | Convert from a 'Pipe' to a 'RecPipe'.  While most of this library is
---- defined in terms of 'Pipe', it can be easier to write certain low-level
---- pipe combining functions in terms of 'RecPipe' than 'Pipe'.
--toRecPipe :: Monad m => Pipe i o u m a -> RecPipe i o u m a
--toRecPipe = fromFT . pipeFree

---- | Convert a 'RecPipe' back into a 'Pipe'.
--fromRecPipe :: Monad m => RecPipe i o u m a -> Pipe i o u m a
--fromRecPipe = Pipe . toFT
