{-# LANGUAGE OverloadedStrings #-}
module MiniLight.FigureDSL where

import Control.Monad
import qualified Data.Config.Font as Font
import qualified Data.Text as T
import Data.Word (Word8)
import MiniLight
import qualified SDL
import qualified SDL.Vect as Vect
import Foreign.Lua
import qualified MiniLight.FigureCache as FC

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

construct :: FC.FigureCache -> FigureDSL -> MiniLight (Maybe Figure)
construct fc dsl = case dsl of
  Empty           -> return $ Just emptyFigure
  Translate p fig -> fmap (fmap (translate p)) $ construct fc fig
  Clip p q fig ->
    fmap (fmap (clip (SDL.Rectangle (Vect.P p) q))) $ construct fc fig
  Picture path ->
    fmap Just $ FC.getOrCreate (picture . T.unpack) (T.pack path) fc
  Text color t -> do
    font <- Font.loadFontFrom
      $ Font.Config (FontDescriptor "IPAGothic" (FontStyle False False)) 24 0
    fmap Just $ text font color t
