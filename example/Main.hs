{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent.MVar
import MiniLight
import MiniLight.Lua

mainFile = "example/main.lua"

main :: IO ()
main = runLightT $ runMiniloop defConfig initial return
 where
  initial = do
    comp <- registerComponent mainFile newLuaComponent
    reload mainFile

    return comp
