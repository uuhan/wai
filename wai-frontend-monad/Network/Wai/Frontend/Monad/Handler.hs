{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
module Network.Wai.Frontend.Monad.Handler where

import Network.Wai
import Network.Wai.Frontend.Monad.Internal
import Network.Wai.Frontend.Monad.Conversion
import Network.Wai.Frontend.Monad.Content
import Control.Monad.Trans.Resource (InternalState)
import Control.Monad.Logger
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Catch
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Control

type LogFunc = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

data HandlerEnv site = HandlerEnv
    { heRichRequest :: RichRequest
    , heSite :: site
    , heInternalState :: InternalState
    , heSettings :: HandlerSettings
    }

data HandlerSettings = HandlerSettings
    { hsLogFunc :: LogFunc
    --, hsOnExc -- FIXME
    }

data HandlerResult a = HandlerResult
    { hrValue :: a
    }

class HasSite a where
    type Site a
    site :: Lens' a (Site a)
instance HasSite (HandlerEnv site) where
    type Site (HandlerEnv site) = site
    site = lens heSite (\he site -> he { heSite = site })

handlerToApp :: ToTypedContent a
             => site
             -> HandlerSettings
             -> HandlerT site IO a
             -> Application
handlerToApp site logFunc (HandlerT f) req respond = bracket
    createInternalState
    closeInternalState
    $ \internalState -> do
        let he = HandlerEnv
                { heRichRequest = toRichRequest req
                }
        res <- f he
        error "FIXME"


newtype HandlerT site m a = HandlerT
    { unHandlerT :: HandlerEnv site -> m a
    }
    deriving Functor
instance Monad m => Applicative (HandlerT site m) where
    pure = HandlerT . const . return
    HandlerT f <*> HandlerT x = HandlerT $ \env -> f env `ap` x env
instance Monad m => Monad (HandlerT site m) where
    return = pure
    HandlerT x >>= f = HandlerT $ \env -> x env >>= flip unHandlerT env . f
instance MonadTrans (HandlerT site) where
    lift = HandlerT . const
instance MonadIO m => MonadIO (HandlerT site m) where
    liftIO = lift . liftIO
instance MonadBase b m => MonadBase b (HandlerT site m) where
    liftBase = lift . liftBase
instance Monad m => MonadReader (HandlerEnv site) (HandlerT site m) where
    ask = HandlerT return
    local f (HandlerT g) = HandlerT $ g . f
-- | Note: although we provide a @MonadBaseControl@ instance, @lifted-base@'s
-- @fork@ function is incompatible with the underlying @ResourceT@ system.
-- Instead, if you must fork a separate thread, you should use
-- @resourceForkIO@.
--
-- Using fork usually leads to an exception that says
-- \"Control.Monad.Trans.Resource.register\': The mutable state is being accessed
-- after cleanup. Please contact the maintainers.\"
instance MonadBaseControl b m => MonadBaseControl b (HandlerT site m) where
    type StM (HandlerT site m) a = StM m a
    liftBaseWith f = HandlerT $ \reader' ->
        liftBaseWith $ \runInBase ->
            f $ runInBase . (\(HandlerT r) -> r reader')
    restoreM = HandlerT . const . restoreM

instance MonadThrow m => MonadThrow (HandlerT site m) where
    throwM = lift . monadThrow

instance (MonadIO m, MonadBase IO m, MonadThrow m) => MonadResource (HandlerT site m) where
    liftResourceT f = HandlerT $ liftIO . runInternalState f . heInternalState

instance MonadIO m => MonadLogger (HandlerT site m) where
    monadLoggerLog a b c d = HandlerT $ \he ->
        liftIO $ (hsLogFunc $ heSettings he) a b c (toLogStr d)

instance MonadIO m => MonadLoggerIO (HandlerT site m) where
    askLoggerIO = HandlerT $ return . hsLogFunc . heSettings
