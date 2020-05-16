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
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TLE
import Data.UnixTime
import qualified Foreign.Lua as Lua
import qualified Foreign.Lua.Types.Peekable as Lua
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
import Paths_minilight_lua

data LuaValue
  = LuaNil
  | LuaBoolean Bool
  | LuaString T.Text
  | LuaNumber Double
  | LuaTable Table
  deriving (Read, Show)

newtype Table = Table [(LuaValue, LuaValue)]
  deriving (Read, Show)

instance Lua.Peekable LuaValue where
  peek i = do
    ltype <- Lua.ltype i
    case ltype of
      Lua.TypeNil -> return LuaNil
      Lua.TypeBoolean -> fmap LuaBoolean $ Lua.toboolean i
      Lua.TypeNumber -> fmap (\(Just (Lua.Number v)) -> LuaNumber v) $ Lua.tonumber i
      Lua.TypeString -> fmap (\(Just v) -> LuaString $ TLE.decodeUtf8 v) $ Lua.tostring i
      Lua.TypeTable -> fmap LuaTable $ Lua.peek i

instance Lua.Pushable LuaValue where
  push val = case val of
    LuaNil -> Lua.pushnil
    LuaBoolean b -> Lua.pushboolean b
    LuaString t -> Lua.pushstring $ TLE.encodeUtf8 t
    LuaNumber d -> Lua.pushnumber $ Lua.Number d
    LuaTable t -> Lua.push t

instance Lua.Peekable Table where
  peek i = fmap Table $ Lua.peekKeyValuePairs i

instance Lua.Pushable Table where
  push (Table kv) = do
    Lua.newtable
    mapM_ (\(k,v) -> Lua.push k *> Lua.push v *> Lua.rawset (-3)) kv

data LuaComponentState = LuaComponentState {
  mousePosition :: IORef (V2 Int),
  mousePressed :: IORef Bool,
  mouseReleased :: IORef Bool,
  figCache :: Cache.CacheRegistry Figure,
  ttfCache :: Cache.CacheRegistry Font,
  luaState :: Lua.State,
  numberStates :: IORef (M.Map String (Ptr Double)),
  boolStates :: IORef (M.Map String (Ptr Bool)),
  stringStates :: IORef (M.Map String (Ptr CString)),
  tableStates :: IORef (M.Map String (Ptr CString)),

  -- Luaでの更新判定用
  updatedAtRef :: IORef UnixTime
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

        qc <- get
        liftIO $ writeIORef (updatedAtRef $ state qc) $ updatedAt qc
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
  ns  <- newIORef M.empty
  bs  <- newIORef M.empty
  ss  <- newIORef M.empty
  ts  <- newIORef M.empty
  u   <- newIORef $ UnixTime 0 0

  let state = LuaComponentState
        { mousePosition = p
        , figCache      = fc
        , ttfCache      = tc
        , luaState      = lua
        , mousePressed  = mp
        , mouseReleased = mr
        , numberStates  = ns
        , boolStates    = bs
        , stringStates  = ss
        , tableStates   = ts
        , updatedAtRef  = u
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
loadLib state = do
  Lua.requirehs "minilight_raw" $ do
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
    Lua.addfunction "newState_table"    minilight_newStateTable
    Lua.addfunction "readState_table"   minilight_readStateTable
    Lua.addfunction "writeState_table"  minilight_writeStateTable

  Lua.requirehs "minilight" $ do
    lib <- liftIO $ getDataFileName "src/lib.lua"
    st  <- Lua.dofile lib
    case st of
      Lua.OK -> return ()
      _      -> Lua.throwException $ "Invalid status (lib): " ++ show st
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

  minilight_newStateBool :: Int -> Bool -> Lua.Lua (Ptr Bool)
  minilight_newStateBool index def = liftIO $ do
    m   <- readIORef $ boolStates state
    uat <- readIORef $ updatedAtRef state
    let key = show uat ++ "-" ++ show index
    case m M.!? key of
      Just k  -> return k
      Nothing -> do
        p <- malloc
        poke p def
        writeIORef (boolStates state) $ M.insert key p m
        return p

  minilight_readStateBool :: Ptr Bool -> Lua.Lua Bool
  minilight_readStateBool = liftIO . peek

  minilight_writeStateBool :: Ptr Bool -> Bool -> Lua.Lua ()
  minilight_writeStateBool p v = liftIO $ poke p v

  minilight_newStateString :: Int -> String -> Lua.Lua (Ptr CString)
  minilight_newStateString index def = liftIO $ do
    m   <- readIORef $ stringStates state
    uat <- readIORef $ updatedAtRef state
    let key = show uat ++ "-" ++ show index
    case m M.!? key of
      Just k  -> return k
      Nothing -> do
        p  <- malloc
        cs <- newCString def
        poke p cs
        writeIORef (stringStates state) $ M.insert key p m
        return p

  minilight_readStateString :: Ptr CString -> Lua.Lua String
  minilight_readStateString p = liftIO $ peekCString =<< peek p

  minilight_writeStateString :: Ptr CString -> String -> Lua.Lua ()
  minilight_writeStateString p v = liftIO $ do
    peek p >>= free
    cs <- newCString v
    poke p cs

  minilight_newStateNumber :: Int -> Lua.Number -> Lua.Lua (Ptr Double)
  minilight_newStateNumber index (Lua.Number def) = liftIO $ do
    m   <- readIORef $ numberStates state
    uat <- readIORef $ updatedAtRef state
    let key = show uat ++ "-" ++ show index
    case m M.!? key of
      Just k  -> return k
      Nothing -> do
        p <- malloc
        poke p def
        writeIORef (numberStates state) $ M.insert key p m
        return p

  minilight_readStateNumber :: Ptr Double -> Lua.Lua Lua.Number
  minilight_readStateNumber p = liftIO $ fmap Lua.Number $ peek p

  minilight_writeStateNumber :: Ptr Double -> Lua.Number -> Lua.Lua ()
  minilight_writeStateNumber p (Lua.Number v) = liftIO $ poke p v

  minilight_newStateTable :: Int -> Table -> Lua.Lua (Ptr CString)
  minilight_newStateTable index def = liftIO $ do
    m   <- readIORef $ tableStates state
    uat <- readIORef $ updatedAtRef state
    let key = show uat ++ "-" ++ show index
    case m M.!? key of
      Just k  -> return k
      Nothing -> do
        p  <- malloc
        cs <- newCString $ show def
        poke p cs
        writeIORef (tableStates state) $ M.insert key p m
        return p

  minilight_readStateTable :: Ptr CString -> Lua.Lua Table
  minilight_readStateTable p = liftIO $ do
    cs <- peek p
    fmap read $ peekCString cs

  minilight_writeStateTable :: Ptr CString -> Table -> Lua.Lua ()
  minilight_writeStateTable p val = liftIO $ do
    peek p >>= free
    cs <- newCString $ show val
    poke p cs
