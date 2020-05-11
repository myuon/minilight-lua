{-# LANGUAGE OverloadedStrings #-}
module MiniLight.FigureDSL where

import Control.Monad
import qualified Data.Config.Font as Font
import qualified Data.Cache as Cache
import qualified Data.Text as T
import Data.Word (Word8)
import MiniLight
import qualified SDL
import qualified SDL.Vect as Vect
import SDL.Font (Font)
import Foreign.Lua

data FigureDSL
  = Empty
  | Translate (Vect.V2 Int) FigureDSL
  | Clip (Vect.V2 Int) (Vect.V2 Int) FigureDSL
  | Picture FilePath
  | Text (Vect.V4 Word8) T.Text
  deriving (Show, Read)

instance Peekable FigureDSL where
  peek = fmap read . peek

instance Pushable FigureDSL where
  push = push . show

construct
  :: Cache.CacheRegistry Font
  -> Cache.CacheRegistry Figure
  -> FigureDSL
  -> MiniLight (Maybe Figure)
construct tc fc = go
 where
  go dsl = case dsl of
    Empty           -> return $ Just emptyFigure
    Translate p fig -> fmap (fmap (translate p)) $ go fig
    Clip p q fig    -> fmap (fmap (clip (SDL.Rectangle (Vect.P p) q))) $ go fig
    Picture path ->
      fmap Just $ Cache.getOrCreate (picture . T.unpack) (T.pack path) fc
    Text color t -> do
      font <- Cache.getOrCreate
        ( \name -> Font.loadFontFrom
          $ Font.Config (FontDescriptor name (FontStyle False False)) 24 0
        )
        "IPAGothic"
        tc
      fmap Just $ text font color t
