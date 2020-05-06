{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module MiniLight.FigureDSL where

import Control.Monad
import qualified Data.Text as T
import Data.Word (Word8)
import Data.Maybe
import Foreign.Ptr
import Foreign.C.String
import Foreign.Storable
import Foreign.Storable.Generic
import Foreign.Marshal.Alloc
import GHC.Generics (Generic)
import MiniLight
import qualified SDL
import qualified SDL.Vect as Vect
import Unsafe.Coerce

data CMaybe a = CMaybe {
  filled :: Bool,
  value :: a
} deriving Generic

pattern CNone def = CMaybe False def
pattern CJust val = CMaybe True val

data FigureDSL = FigureDSL {
  instruction :: Int,
  translateArg :: CMaybe (Vect.V2 Int),
  clipArg :: CMaybe (Vect.V2 Int, Vect.V2 Int),
  rotateArg :: CMaybe Double,
  pictureArg :: CMaybe CString,
  rectangleOutlineArg :: CMaybe (Vect.V4 Word8, Vect.V2 Int),
  rectangleFilledArg :: CMaybe (Vect.V4 Word8, Vect.V2 Int),
  triangleOutlineArg :: CMaybe (Vect.V4 Word8, Vect.V2 Int),
  recursive :: CMaybe (Ptr FigureDSL)
} deriving Generic

defFigureDSL :: FigureDSL
defFigureDSL = FigureDSL
  { instruction         = 0
  , translateArg        = CNone 0
  , clipArg             = CNone (0,0)
  , rotateArg           = CNone 0
  , pictureArg          = CNone nullPtr
  , rectangleOutlineArg = CNone (0,0)
  , rectangleFilledArg  = CNone (0,0)
  , triangleOutlineArg  = CNone (0,0)
  , recursive           = CNone nullPtr
  }

instance (GStorable a) => GStorable (CMaybe a)
instance (GStorable a) => GStorable (Vect.V2 a)
instance (GStorable a) => GStorable (Vect.V4 a)
instance (GStorable a, GStorable b) => GStorable (a,b)
instance (GStorable a, GStorable b, GStorable c) => GStorable (a,b,c)
instance GStorable FigureDSL

construct :: Ptr FigureDSL -> MiniLight (Maybe Figure)
construct ptr
  | ptr == nullPtr = return Nothing
  | otherwise = do
    fig <- liftIO $ peek ptr
    case fig of
      FigureDSL { instruction = 0 } -> return $ Just emptyFigure
      FigureDSL { instruction = 1, translateArg = CJust v, recursive = CJust ptr }
        -> fmap (fmap (translate v)) $ construct ptr
      FigureDSL { instruction = 2, clipArg = CJust (p, q), recursive = CJust ptr }
        -> fmap (fmap (clip $ SDL.Rectangle (SDL.P p) q)) $ construct ptr
      FigureDSL { instruction = 3, rotateArg = CJust v, recursive = CJust ptr }
        -> fmap (fmap (rotate v)) $ construct ptr
      -- FigureDSL { instruction = 4, textArg = CSome v } -> ...
      FigureDSL { instruction = 5, pictureArg = CJust v } -> do
        path <- liftIO $ peekCString v
        fmap Just $ picture path
      FigureDSL { instruction = 6, rectangleOutlineArg = CJust (p, q) } ->
        fmap Just $ rectangleOutline p q
      FigureDSL { instruction = 7, rectangleFilledArg = CJust (p, q) } ->
        fmap Just $ rectangleFilled p q
      FigureDSL { instruction = 8, triangleOutlineArg = CJust (p, q) } ->
        fmap Just $ triangleOutline p q

translate_ :: Vect.V2 Int -> Ptr FigureDSL -> IO (Ptr FigureDSL)
translate_ v f = do
  p <- malloc
  poke p $ defFigureDSL { instruction  = 1
                        , translateArg = CJust v
                        , recursive    = CJust f
                        }
  return p

clip_ :: Vect.V2 Int -> Vect.V2 Int -> Ptr FigureDSL -> IO (Ptr FigureDSL)
clip_ v w f = do
  p <- malloc
  poke p $ defFigureDSL { instruction = 2
                        , clipArg     = CJust (v, w)
                        , recursive   = CJust f
                        }
  return p

rotate_ :: Double -> Ptr FigureDSL -> IO (Ptr FigureDSL)
rotate_ v f = do
  p <- malloc
  poke p $ defFigureDSL { instruction = 3
                        , rotateArg   = CJust v
                        , recursive   = CJust f
                        }
  return p

picture_ :: CString -> IO (Ptr FigureDSL)
picture_ c = do
  p <- malloc
  poke p $ defFigureDSL { instruction = 5, pictureArg = CJust c }
  return p