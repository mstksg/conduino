{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeInType                 #-}

module Data.Conduino.Internal (
    Pipe(..)
  , PipeF(..)
  , awaitEither
  , yield
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
import           Data.Foldable
import           Data.Void

data PipeF i o u a =
      PAwaitF (i -> a) (u -> a)
    | PYieldF o a
  deriving Functor

makeFree ''PipeF

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
newtype Pipe i o u m a = Pipe { pipeFree :: FT (PipeF i o u) m a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadTrans
           , MonadFree (PipeF i o u)
           )

type Source o  = Pipe () o
type Sink   i  = Pipe i  Void
type Effect    = Pipe () Void
type Forever p = p Void

awaitEither :: Pipe i o u m (Either i u)
awaitEither = pAwaitF

yield :: o -> Pipe i o u m ()
yield = pYieldF

