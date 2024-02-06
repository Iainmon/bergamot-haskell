module MyLib  where
import Control.Monad

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- -- type Identity a = a
-- -- type Reader r a = r -> a
-- -- type Writer w a = (a,w)
-- -- type State s a = s -> (a,s)
-- -- type RWS r w s a = r -> s -> (a,s,w)

-- -- type IdentityT m a = m a

-- -- type MaybeT m a = m (Maybe a)

-- -- type ListT m a = m [a]

-- -- type ReaderT r m a = r -> m a

-- -- type WriterT w m a = m (a,w)

-- -- type ExceptT e m a = m (Either e a)

-- -- type StateT s m a = s -> m (a,s)

-- -- type RWST r w s m a = r -> s -> m (a,s,w)

-- newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

-- instance Functor m => Functor (ReaderT r m) where
--   fmap f (ReaderT rma) = ReaderT $ \r -> fmap f (rma r)

-- instance Applicative m => Applicative (ReaderT r m) where
--   pure = ReaderT . const . pure
--   (<*>) = ap

-- instance Monad m => Monad (ReaderT r m) where
--   (>>=) :: Monad m => ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
--   ReaderT rma >>= f = ReaderT $ \r -> do
--     a <- rma r
--     runReaderT (f a) r
