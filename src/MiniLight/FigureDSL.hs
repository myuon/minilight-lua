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

construct :: FigureDSL -> MiniLight (Maybe Figure)
construct dsl = case dsl of
  Empty           -> return $ Just emptyFigure
  Translate p fig -> fmap (fmap (translate p)) $ construct fig
  Clip p q fig ->
    fmap (fmap (clip (SDL.Rectangle (Vect.P p) q))) $ construct fig
  Picture path -> fmap Just $ picture path
  Text color t -> do
    font <- Font.loadFontFrom
      $ Font.Config (FontDescriptor "IPAGothic" (FontStyle False False)) 24 0
    fmap Just $ text font color t
