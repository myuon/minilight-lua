local minilight = require("minilight")
local index = 0

function useState(def)
    index = index + 1

    if (type(def) == "number") then
        local ref = minilight.newState_number(index, def)

        return {
            value = minilight.readState_number(ref),
            write = function(v)
                return minilight.writeState_number(ref, v)
            end
        }
    elseif (type(def) == "string") then
        local ref = minilight.newState_string(index, def)

        return {
            value = minilight.readState_string(ref),
            write = function(v)
                return minilight.writeState_string(ref, v)
            end
        }
    elseif (type(def) == "bool") then
        local ref = minilight.newState_bool(index, def)

        return {
            value = minilight.readState_bool(ref),
            write = function(v)
                return minilight.writeState_bool(ref, v)
            end
        }
    else
        error("Type(" .. type(def) .. ") is not supported!")
    end
end

function onDraw()
    print("[LUA OUTPUT] hello")

    local p = minilight.useMouseMove()
    local clicked = useState(0)

    local pressed = minilight.useMousePressed()
    if (pressed) then clicked.write(clicked.value + 1) end

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
