{-# LANGUAGE OverloadedStrings #-}
module MiniLight.Lua where

import qualified Control.Monad.Caster as Caster
import Control.Monad.Catch
import Control.Monad.State hiding (state)
import qualified Data.ByteString as BS
import qualified Data.Cache as Cache
import qualified Data.Component.Basic as Basic
import Data.IORef
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TLE
import Data.UnixTime
import qualified Foreign.Lua as Lua
import Foreign.Ptr
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Storable
import Linear
import MiniLight
import MiniLight.FigureDSL
import qualified SDL
import qualified SDL.Vect as Vect
import SDL.Font (Font)

data LuaComponentState = LuaComponentState {
  mousePosition :: IORef (V2 Int),
  mousePressed :: IORef Bool,
  mouseReleased :: IORef Bool,
  figCache :: Cache.CacheRegistry Figure,
  ttfCache :: Cache.CacheRegistry Font,
  luaState :: Lua.State
}

data LuaComponent = LuaComponent {
  expr :: String,
  state :: LuaComponentState,
  updatedAt :: UnixTime,
  counter :: Int
}

data LuaComponentEvent
  = SetExpr String

instance EventType LuaComponentEvent where
  getEventType (SetExpr _) = "set_expr"

instance ComponentUnit LuaComponent where
  figures comp = evalLuaComponent (expr comp) (state comp)

  onSignal ev = execStateT $ do
    get >>= \qc -> liftIO $ do
      writeIORef (mousePressed $ state qc) False
      writeIORef (mouseReleased $ state qc) False
    modify' $ \qc -> qc { counter = counter qc + 1 }
    lift $ Basic.emitBasicSignal ev (Basic.Config { Basic.size = V2 640 480, Basic.position = V2 0 0, Basic.visible = True })

    case asSignal ev of
      Just (SetExpr fs) -> do
        t <- liftIO getUnixTime
        modify' $ \qc -> qc { expr = fs, updatedAt = t }
      _ -> return ()

    case asSignal ev of
      Just (Basic.MouseOver p) -> do
        st <- get
        liftIO $ writeIORef (mousePosition $ state st) p
      Just (Basic.MousePressed _) -> do
        st <- get
        liftIO $ writeIORef (mousePressed $ state st) True
      Just (Basic.MouseReleased _) -> do
        st <- get
        liftIO $ writeIORef (mouseReleased $ state st) True
      _ -> return ()

  useCache c1 c2 = updatedAt c1 == updatedAt c2 && counter c1 == counter c2

newLuaComponent :: IO LuaComponent
newLuaComponent = do
  p   <- newIORef 0
  fc  <- Cache.new
  tc  <- Cache.new
  lua <- Lua.newstate
  mp  <- newIORef False
  mr  <- newIORef False

  let state = LuaComponentState
        { mousePosition = p
        , figCache      = fc
        , ttfCache      = tc
        , luaState      = lua
        , mousePressed  = mp
        , mouseReleased = mr
        }

  Lua.runWith lua $ do
    Lua.openlibs
    loadLib state

  return $ LuaComponent
    { expr      = ""
    , state     = state
    , updatedAt = UnixTime 0 0
    , counter   = 0
    }

evalLuaComponent
  :: (HasLightEnv env, MonadIO m, MonadMask m)
  => String
  -> LuaComponentState
  -> LightT env m [Figure]
evalLuaComponent content state
  | content == "" = return []
  | otherwise = do
    let lua = luaState state
    result <- liftIO $ Lua.runWith lua $ Lua.try $ do
      st <- Lua.dostring $ TLE.encodeUtf8 $ T.pack content
      case st of
        Lua.OK -> Lua.callFunc "onDraw" ()
        _      -> Lua.throwException $ "Invalid status: " ++ show st

    case result of
      Left  err -> Caster.err err >> return []
      Right rs  -> liftMiniLight $ fmap catMaybes $ mapM
        (construct (ttfCache state) (figCache state))
        rs

reload
  :: (HasLoaderEnv env, HasLightEnv env, HasLoopEnv env, MonadIO m, MonadMask m)
  => T.Text
  -> LightT env m ()
reload path = do
  fs <- liftIO $ readFile (T.unpack path)
  path @@! SetExpr fs

loadLib :: LuaComponentState -> Lua.Lua ()
loadLib state = Lua.requirehs "minilight" $ do
  Lua.create
  Lua.addfunction "picture"           minilight_picture
  Lua.addfunction "translate"         minilight_translate
  Lua.addfunction "text"              minilight_text
  Lua.addfunction "useMouseMove"      minilight_useMouseMove
  Lua.addfunction "useMousePressed"   minilight_useMousePressed
  Lua.addfunction "useMouseReleased"  minilight_useMouseReleased
  Lua.addfunction "newState_bool"     minilight_newStateBool
  Lua.addfunction "readState_bool"    minilight_readStateBool
  Lua.addfunction "writeState_bool"   minilight_writeStateBool
  Lua.addfunction "newState_string"   minilight_newStateString
  Lua.addfunction "readState_string"  minilight_readStateString
  Lua.addfunction "writeState_string" minilight_writeStateString
  Lua.addfunction "newState_number"   minilight_newStateNumber
  Lua.addfunction "readState_number"  minilight_readStateNumber
  Lua.addfunction "writeState_number" minilight_writeStateNumber
 where
  minilight_picture :: BS.ByteString -> Lua.Lua FigureDSL
  minilight_picture cs = return $ Picture $ T.unpack $ TLE.decodeUtf8 cs

  minilight_translate :: Int -> Int -> FigureDSL -> Lua.Lua FigureDSL
  minilight_translate x y fig = return $ Translate (Vect.V2 x y) fig

  minilight_text :: BS.ByteString -> (Int, Int, Int, Int) -> Lua.Lua FigureDSL
  minilight_text cs (r, g, b, a) = return $ Text
    ( Vect.V4 (fromIntegral r)
              (fromIntegral g)
              (fromIntegral b)
              (fromIntegral a)
    )
    (TLE.decodeUtf8 cs)

  minilight_useMouseMove :: Lua.Lua (Int, Int)
  minilight_useMouseMove = do
    Vect.V2 x y <- liftIO $ readIORef $ mousePosition state
    return (x, y)

  minilight_useMousePressed :: Lua.Lua Bool
  minilight_useMousePressed = liftIO $ readIORef $ mousePressed state

  minilight_useMouseReleased :: Lua.Lua Bool
  minilight_useMouseReleased = liftIO $ readIORef $ mouseReleased state

  minilight_newStateBool :: Bool -> Lua.Lua (Ptr Bool)
  minilight_newStateBool def = liftIO $ do
    p <- malloc
    poke p def
    return p

  minilight_readStateBool :: Ptr Bool -> Lua.Lua Bool
  minilight_readStateBool = liftIO . peek

  minilight_writeStateBool :: Ptr Bool -> Bool -> Lua.Lua ()
  minilight_writeStateBool p v = liftIO $ poke p v

  minilight_newStateString :: String -> Lua.Lua (Ptr CString)
  minilight_newStateString def = liftIO $ do
    p  <- malloc
    cs <- newCString def
    poke p cs
    return p

  minilight_readStateString :: Ptr CString -> Lua.Lua String
  minilight_readStateString p = liftIO $ peekCString =<< peek p

  minilight_writeStateString :: Ptr CString -> String -> Lua.Lua ()
  minilight_writeStateString p v = liftIO $ do
    peek p >>= free
    cs <- newCString v
    poke p cs

  minilight_newStateNumber :: Lua.Number -> Lua.Lua (Ptr Double)
  minilight_newStateNumber (Lua.Number def) = liftIO $ do
    p <- malloc
    poke p def
    return p

  minilight_readStateNumber :: Ptr Double -> Lua.Lua Lua.Number
  minilight_readStateNumber p = liftIO $ fmap Lua.Number $ peek p

  minilight_writeStateNumber :: Ptr Double -> Lua.Number -> Lua.Lua ()
  minilight_writeStateNumber p (Lua.Number v) = liftIO $ poke p v
