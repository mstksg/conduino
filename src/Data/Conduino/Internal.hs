{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeOperators              #-}

module Data.Conduino.Internal (
    Pipe(..)
  , PipeF(..)
  , awaitEither
  , yield
  , trimapPipe, mapInput, mapOutput, mapUpRes
  , hoistPipe
  , RecPipe
  , toRecPipe, fromRecPipe
  , annotatePipeF
  ) where

import           Control.Monad.Free.Class
import           Control.Applicative
import           Control.Monad.Free.TH
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Free        (FreeT(..))
import           Control.Monad.Trans.Free.Church
import           GHC.Generics

-- | Base functor of 'Pipe'.
data PipeF i o u a =
      PAwaitF (u -> a) (i -> a)
    | PYieldF o a
  deriving Functor

makeFree ''PipeF

annotatePipeF :: PipeF i o u a -> (Const String :*: PipeF i o u) a
annotatePipeF p = case p of
    PAwaitF _ _ -> Const "PAwaitF" :*: p
    PYieldF _ _ -> Const "PYieldF" :*: p

  


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
-- *  A pipe is a /source/ if @i@ is @()@: it doesn't need anything to go
--    pump out items.  If a pipe is source and @a@ is 'Data.Void.Void', it
--    means that it will produce forever.
--
-- *  A pipe is a /sink/ if @o@ is 'Void.Void': it will never yield
--    anything else downstream.
--
-- *  If a pipe is both a source and a sink, it is an /effect/.
--
-- *  Normally you can ask for input upstream with 'Data.Conduino.await',
--    which returns 'Nothing' if the pipe upstream stops producing.
--    However, if @u@ is 'Data.Void.Void', it means that the pipe upstream
--    will never stop, so you can use 'Data.Conduino.awaitSurely' to get
--    a guaranteed answer.
newtype Pipe i o u m a = Pipe { pipeFree :: FT (PipeF i o u) m a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadTrans
           , MonadFree (PipeF i o u)
           , MonadIO
           )

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
