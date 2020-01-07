{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Conduino.Lift (
  -- * State
  -- ** Lazy
    stateP, runStateP, evalStateP, execStateP
  -- ** Strict
  , statePS, runStatePS, evalStatePS, execStatePS
  -- * Except
  , exceptP, runExceptP
  -- * Reader
  , readerP, runReaderP
  -- * Writer
  -- ** Lazy
  , writerP, runWriterP, execWriterP
  -- ** Strict
  , writerPS, runWriterPS, execWriterPS
  -- * RWS
  -- ** Lazy
  , rwsP, runRWSP, evalRWSP, execRWSP
  -- ** Strict
  , rwsPS, runRWSPS, evalRWSPS, execRWSPS
  -- * Catch
  , runCatchP, catchCatchP
  ) where

import           Control.Monad.Catch.Pure
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Free
import           Control.Monad.Trans.Free.Church
import           Control.Monad.Trans.RWS           (RWST(..))
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Writer
import           Data.Conduino
import           Data.Conduino.Internal
import           Data.Functor
import qualified Control.Monad.Trans.RWS           as RWS
import qualified Control.Monad.Trans.RWS.Strict    as RWSS
import qualified Control.Monad.Trans.State.Strict  as SS
import qualified Control.Monad.Trans.Writer.Strict as WS

stateP
    :: Monad m
    => (s -> Pipe i o u m (a, s))
    -> Pipe i o u (StateT s m) a
stateP f = do
    s       <- lift get
    (x, s') <- hoistPipe lift (f s)
    x <$ lift (put s')

execStateP
    :: Monad m
    => s
    -> Pipe i o u (StateT s m) a
    -> Pipe i o u m s
execStateP s = fmap snd . runStateP s

evalStateP
    :: Monad m
    => s
    -> Pipe i o u (StateT s m) a
    -> Pipe i o u m a
evalStateP s = fmap fst . runStateP s

statePS
    :: Monad m
    => (s -> Pipe i o u m (a, s))
    -> Pipe i o u (SS.StateT s m) a
statePS f = do
    s       <- lift SS.get
    (x, s') <- hoistPipe lift (f s)
    x <$ lift (SS.put s')

runStatePS
    :: Monad m
    => s
    -> Pipe i o u (SS.StateT s m) a
    -> Pipe i o u m (a, s)
runStatePS = withRecPipe . go
  where
    go s (FreeT p) = FreeT $ SS.runStateT p s <&> \(q, s') ->
      case q of
        Pure x -> Pure (x, s')
        Free l -> Free $ go s' <$> l

execStatePS
    :: Monad m
    => s
    -> Pipe i o u (SS.StateT s m) a
    -> Pipe i o u m s
execStatePS s = fmap snd . runStatePS s

evalStatePS
    :: Monad m
    => s
    -> Pipe i o u (SS.StateT s m) a
    -> Pipe i o u m a
evalStatePS s = fmap fst . runStatePS s

exceptP
    :: Monad m
    => Pipe i o u m (Either e a)
    -> Pipe i o u (ExceptT e m) a
exceptP p = hoistPipe lift p >>= \case
    Left  e -> lift $ throwE e
    Right x -> pure x

runExceptP
    :: Monad m
    => Pipe i o u (ExceptT e m) a
    -> Pipe i o u m (Either e a)
runExceptP = withRecPipe go
  where
    go (FreeT p) = FreeT $ runExceptT p <&> \case
      Left  e        -> Pure $ Left e
      Right (Pure x) -> Pure $ Right x
      Right (Free l) -> Free $ go <$> l

runCatchP
    :: Monad m
    => Pipe i o u (CatchT m) a
    -> Pipe i o u m (Either SomeException a)
runCatchP = withRecPipe go
  where
    go (FreeT p) = FreeT $ runCatchT p <&> \case
      Left  e        -> Pure $ Left e
      Right (Pure x) -> Pure $ Right x
      Right (Free l) -> Free $ go <$> l

catchCatchP
    :: Monad m
    => Pipe i o u (CatchT m) a
    -> (SomeException -> Pipe i o u (CatchT m) a)
    -> Pipe i o u (CatchT m) a
catchCatchP (Pipe p) f = Pipe $ FT $ \return0 handle0 ->
    catch (runFT p return0 handle0)
          (\e -> runFT (pipeFree (f e)) return0 handle0)

readerP
    :: Monad m
    => (r -> Pipe i o u m a)
    -> Pipe i o u (ReaderT r m) a
readerP f = hoistPipe lift . f =<< lift ask

runReaderP
    :: Monad m
    => r
    -> Pipe i o u (ReaderT r m) a
    -> Pipe i o u m a
runReaderP r = hoistPipe (`runReaderT` r)

writerP
    :: (Monad m, Monoid w)
    => Pipe i o u m (a, w)
    -> Pipe i o u (WriterT w m) a
writerP p = do
    (x, w) <- hoistPipe lift p
    x <$ lift (tell w)

runWriterP
    :: (Monad m, Monoid w)
    => Pipe i o u (WriterT w m) a
    -> Pipe i o u m (a, w)
runWriterP = withRecPipe (go mempty)
  where
    go w (FreeT p) = FreeT $ runWriterT p <&> \(r, (w <>)->w') ->
      case r of
        Pure x -> Pure (x, w')
        Free l -> Free $ go w' <$> l

execWriterP
    :: (Monad m, Monoid w)
    => Pipe i o u (WriterT w m) a
    -> Pipe i o u m w
execWriterP = fmap snd . runWriterP

writerPS
    :: (Monad m, Monoid w)
    => Pipe i o u m (a, w)
    -> Pipe i o u (WS.WriterT w m) a
writerPS p = do
    (x, w) <- hoistPipe lift p
    x <$ lift (WS.tell w)

runWriterPS
    :: (Monad m, Monoid w)
    => Pipe i o u (WS.WriterT w m) a
    -> Pipe i o u m (a, w)
runWriterPS = withRecPipe (go mempty)
  where
    go w (FreeT p) = FreeT $ WS.runWriterT p <&> \(r, (w <>)->w') ->
      case r of
        Pure x -> Pure (x, w')
        Free l -> Free $ go w' <$> l

execWriterPS
    :: (Monad m, Monoid w)
    => Pipe i o u (WriterT w m) a
    -> Pipe i o u m w
execWriterPS = fmap snd . runWriterP

rwsP
    :: (Monad m, Monoid w)
    => (r -> s -> Pipe i o u m (a, s, w))
    -> Pipe i o u (RWST r w s m) a
rwsP f = do
    r <- lift RWS.ask
    s <- lift RWS.get
    (x, s', w) <- hoistPipe lift (f r s)
    lift (RWS.tell w)
    x <$ lift (RWS.put s')

runRWSP
    :: (Monad m, Monoid w)
    => r
    -> s
    -> Pipe i o u (RWST r w s m) a
    -> Pipe i o u m (a, s, w)
runRWSP r = withRecPipe . go mempty
  where
    go w s (FreeT p) = FreeT $ runRWST p r s <&> \(q, s', (w <>)->w') ->
      case q of
        Pure x -> Pure (x, s', w')
        Free l -> Free $ go w' s' <$> l

evalRWSP
    :: (Monad m, Monoid w)
    => r
    -> s
    -> Pipe i o u (RWST r w s m) a
    -> Pipe i o u m (a, w)
evalRWSP r s = fmap (\(x,_,w) -> (x,w)) . runRWSP r s

execRWSP
    :: (Monad m, Monoid w)
    => r
    -> s
    -> Pipe i o u (RWST r w s m) a
    -> Pipe i o u m (s, w)
execRWSP r s = fmap (\(_,s',w) -> (s',w)) . runRWSP r s

rwsPS
    :: (Monad m, Monoid w)
    => (r -> s -> Pipe i o u m (a, s, w))
    -> Pipe i o u (RWSS.RWST r w s m) a
rwsPS f = do
    r <- lift RWSS.ask
    s <- lift RWSS.get
    (x, s', w) <- hoistPipe lift (f r s)
    lift (RWSS.tell w)
    x <$ lift (RWSS.put s')

runRWSPS
    :: (Monad m, Monoid w)
    => r
    -> s
    -> Pipe i o u (RWSS.RWST r w s m) a
    -> Pipe i o u m (a, s, w)
runRWSPS r = withRecPipe . go mempty
  where
    go w s (FreeT p) = FreeT $ RWSS.runRWST p r s <&> \(q, s', (w <>)->w') ->
      case q of
        Pure x -> Pure (x, s', w')
        Free l -> Free $ go w' s' <$> l

evalRWSPS
    :: (Monad m, Monoid w)
    => r
    -> s
    -> Pipe i o u (RWSS.RWST r w s m) a
    -> Pipe i o u m (a, w)
evalRWSPS r s = fmap (\(x,_,w) -> (x,w)) . runRWSPS r s

execRWSPS
    :: (Monad m, Monoid w)
    => r
    -> s
    -> Pipe i o u (RWSS.RWST r w s m) a
    -> Pipe i o u m (s, w)
execRWSPS r s = fmap (\(_,s',w) -> (s',w)) . runRWSPS r s
