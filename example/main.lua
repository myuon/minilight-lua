local minilight = require("minilight")

function onDraw()
    return {
        minilight.translate(50, 50, minilight.picture("example/example.png")),
        minilight.translate(100, 100, minilight.text("Hello, World!"))
    }
end

_G.onDraw = onDraw
