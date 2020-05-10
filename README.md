# minilight-lua

A binding library of [minilight](https://github.com/myuon/minilight) for Lua language.

*NB: This package is in the very early stage.*

## What's this?

- [minilight](http://hackage.haskell.org/package/minilight) is a SDL2-based graphics library, equipped with component system.
- [Lua](https://www.lua.org) is a lightweight interpreted language.

With this library, you can write a minilight component in Lua language.

## Getting Started

See [example](https://github.com/myuon/minilight-lua/tree/master/example) directory. `Main.hs` is an entrypoint for minilight engine.

```hs
mainFile = "example/main.lua"

main :: IO ()
main = runLightT $ runMiniloop
  (defConfig { hotConfigReplacement = Just "example", appConfigFile = Just "" })
  initial
  (const mainloop)
 where
  initial = do
    comp <- registerComponent mainFile newLuaComponent
    reload mainFile

    return ()

  mainloop :: MiniLoop ()
  mainloop = do
    ref <- view _events
    evs <- liftIO $ tryReadMVar ref

    let notifys = case evs of
          Just evs -> mapMaybe asNotifyEvent evs
          _        -> []
    unless (null notifys) $ reload mainFile
```

Some notes here:

- When you pass `hotConfigReplacement` field, minilight will watch the given directory and emits *file changed/created/delete* events during the mainloop.
- For `registerComponent` you need to pass the filename like `example/main.lua`. The path is relative where you run `cabal run`.
- In `mainloop`, watches any events and `reload` the component. The `reload` function will load the lua file again and swap the component dynamically (code swapping).

```lua
local minilight = require("minilight")

function onDraw()
    print("[LUA OUTPUT] hello")

    return {
        minilight.translate(50, 50, minilight.picture("example/example.png")),
        minilight.translate(100, 100, minilight.text("こんにちは世界",
                                                     {0, 0, 0, 0})),
        minilight.translate(30, 50,
                            minilight.text("Hello, World!", {255, 0, 0, 0}))
    }
end

_G.onDraw = onDraw
```

For lua part:

- Require `minilight` library. This will be implicitly loaded by minilight-lua.
- You need to export `onDraw : () -> Array<minilight.Figure>` function globally. This function will be called from Haskell and the returned array will be rendered in the component.
