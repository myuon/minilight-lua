local minilight = require("minilight")

function onDraw()
    print("[LUA OUTPUT] hello")

    local p = minilight.useMouseMove()
    print(p[1], p[2])

    return {
        minilight.translate(50, 50, minilight.picture("example/example.png")),
        minilight.translate(100, 100, minilight.text("こんにちは世界",
                                                     {0, 0, 0, 0})),
        minilight.translate(30, 50,
                            minilight.text("Hello, World!", {255, 0, 0, 0}))
    }
end

_G.onDraw = onDraw
