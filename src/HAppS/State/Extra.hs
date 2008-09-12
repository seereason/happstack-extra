module HAppS.State.Extra where

import Control.Exception
import Control.Monad.Trans(MonadIO(liftIO))
import HAppS.State

-- |execute 'action' with the state transaction system running
-- useful for testing state related code interactively in GHCi
withSystemState :: (Component st, Methods st) => Proxy st -> IO a -> IO a
withSystemState entryPoint action =
    do control <- startSystemState entryPoint
       action `finally` shutdownSystem control


-- * these need better names

liftQuery :: (MonadIO m, QueryEvent ev res) => ev -> m res
liftQuery = liftIO . query

liftUpdate :: (MonadIO m, UpdateEvent ev res) => ev -> m res
liftUpdate = liftIO . update
