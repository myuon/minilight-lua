module MiniLight.FigureCache where

import Control.Monad.IO.Class
import qualified Data.Cache as Cache
import qualified Data.Map as M
import qualified Data.Text as T
import MiniLight

newtype FigureCache = FigureCache { getFigureCache :: Cache.CacheRegistry Figure }

new :: MonadIO m => m FigureCache
new = fmap FigureCache Cache.new

size :: MonadIO m => FigureCache -> m Int
size (FigureCache cache) = Cache.size cache

getOrCreate
  :: MonadIO m => (T.Text -> m Figure) -> T.Text -> FigureCache -> m Figure
getOrCreate alloc key (FigureCache cache) = do
  result <- Cache.lookup key cache
  (\f -> maybe f return result) $ do
    fig <- alloc key
    Cache.register key fig cache
    return fig

clearAll :: MonadIO m => FigureCache -> m ()
clearAll (FigureCache cache) = do
  m <- Cache.clear cache
  mapM_ freeFigure (M.elems m)
