local minilight = require("minilight")
minilight.init()

function onDraw()
    print("[LUA OUTPUT] hello")

    local p = minilight.useMouseMove()
    local history = minilight.useState({})
    local clicked = minilight.useState(0)

    local pressed = minilight.useMousePressed()
    if (pressed) then
        clicked.write(clicked.value + 1)
        table.insert(history.value, clicked.value)

        print(history.value[#history.value])
    end

    return {
        minilight.translate(50, 50, minilight.picture("example/example.png")),
        minilight.translate(100, 100, minilight.text("こんにちは世界",
                                                     {0, 0, 0, 0})),
        minilight.translate(30, 50,
                            minilight.text("Hello, World!", {255, 0, 0, 0})),
        minilight.translate(300, 80, minilight.text(
                                "(" .. p[1] .. "," .. p[2] .. ")", {0, 0, 0, 0})),
        minilight.translate(100, 150, minilight.text(
                                "You've clicked " .. clicked.value .. " times!",
                                {0, 0, 0, 0}))
    }
end

_G.onDraw = onDraw
