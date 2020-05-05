{-# LANGUAGE OverloadedStrings #-}
module MiniLight.Lua where

import Control.Monad.Catch
import Control.Monad.State hiding (state)
import qualified Data.Component.Basic as Basic
import Linear
import MiniLight

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
evalLuaComponent content state = undefined
