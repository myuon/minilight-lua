{-# LANGUAGE OverloadedStrings #-}
module MiniLight.Lua where

import qualified Control.Monad.Caster as Caster
import Control.Monad.Catch
import Control.Monad.State hiding (state)
import qualified Data.ByteString as BS
import qualified Data.Component.Basic as Basic
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TLE
import qualified Foreign.Lua as Lua
import GHC.Generics (Generic)
import Linear
import MiniLight
import MiniLight.FigureDSL
import qualified SDL

data LuaComponentState = LuaComponentState {
  mousePosition :: V2 Int
} deriving (Eq, Show)

data LuaComponent = LuaComponent {
  expr :: String,
  state :: LuaComponentState,
  counter :: Int
}

data LuaComponentEvent
  = SetExpr String

instance EventType LuaComponentEvent where
  getEventType (SetExpr _) = "set_expr"

instance ComponentUnit LuaComponent where
  figures comp = evalLuaComponent (expr comp) (state comp)

  onSignal ev = execStateT $ do
    lift $ Basic.emitBasicSignal ev (Basic.Config { Basic.size = V2 640 480, Basic.position = V2 0 0, Basic.visible = True })

    case asSignal ev of
      Just (SetExpr fs) ->
        modify $ \qc -> qc { expr = fs, counter = counter qc + 1 }
      _ -> return ()

    case asSignal ev of
      Just (Basic.MouseOver p) ->
        modify $ \qc -> qc { state = (state qc) { mousePosition = p }, counter = counter qc + 1 }
      _ -> return ()

  useCache c1 c2 = counter c1 == counter c2

newLuaComponent :: LuaComponent
newLuaComponent = LuaComponent
  { expr    = ""
  , state   = LuaComponentState {mousePosition = 0}
  , counter = 0
  }

evalLuaComponent
  :: (HasLightEnv env, MonadIO m, MonadMask m)
  => String
  -> LuaComponentState
  -> LightT env m [Figure]
evalLuaComponent content state
  | content == "" = return []
  | otherwise = do
    result <- liftIO $ Lua.run $ Lua.try $ do
      Lua.openlibs
      st <- Lua.dostring $ TLE.encodeUtf8 $ T.pack content
      case st of
        Lua.OK -> Lua.callFunc "onDraw" ()
        _      -> Lua.throwException $ "Invalid status: " ++ show st

    case result of
      Left  err -> Caster.err err >> return []
      Right rs  -> liftMiniLight $ fmap catMaybes $ mapM construct rs

reload
  :: (HasLoaderEnv env, HasLightEnv env, HasLoopEnv env, MonadIO m, MonadMask m)
  => T.Text
  -> LightT env m ()
reload path = do
  fs <- liftIO $ readFile (T.unpack path)
  path @@! SetExpr fs
