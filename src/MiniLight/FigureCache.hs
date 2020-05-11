module MiniLight.FigureCache where

import Control.Monad.IO.Class
import qualified Data.Cache as Cache
import qualified Data.Map as M
import qualified Data.Text as T
import MiniLight

newtype FigureCache = FigureCache { getFigureCache :: Cache.CacheRegistry Figure }

new :: MonadIO m => m FigureCache
new = fmap FigureCache Cache.new

getOrCreate
  :: MonadIO m => (T.Text -> m Figure) -> T.Text -> FigureCache -> m Figure
getOrCreate alloc key (FigureCache cache) =
  maybe (alloc key) return =<< Cache.lookup key cache

clearAll :: MonadIO m => FigureCache -> m ()
clearAll (FigureCache cache) = do
  m <- Cache.clear cache
  mapM_ freeFigure (M.elems m)
