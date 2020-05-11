module Data.Cache where

import Control.Monad.IO.Class
import Data.IORef
import qualified Data.Map as M
import qualified Data.Text as T

newtype CacheRegistry v = CacheRegistry { getCacheRegistry :: IORef (M.Map T.Text v) }

new :: MonadIO m => m (CacheRegistry v)
new = liftIO $ fmap CacheRegistry $ newIORef M.empty

register :: MonadIO m => T.Text -> v -> CacheRegistry v -> m ()
register t v cache =
  liftIO $ modifyIORef' (getCacheRegistry cache) $ M.insert t v

lookup :: MonadIO m => T.Text -> CacheRegistry v -> m (Maybe v)
lookup t cache =
  liftIO $ fmap (M.lookup t) $ readIORef (getCacheRegistry cache)

clear :: MonadIO m => CacheRegistry v -> m (M.Map T.Text v)
clear cache = liftIO $ do
  let ref = getCacheRegistry cache
  m <- readIORef ref
  writeIORef ref M.empty
  return m