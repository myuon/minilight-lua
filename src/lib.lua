local minilight_raw = require("minilight_raw")
local mod = minilight_raw

function mod.init() mod.useState_index = 0 end

function mod.useState(def)
    mod.useState_index = mod.useState_index + 1
    local index = mod.useState_index

    if (type(def) == "number") then
        local ref = minilight_raw.newState_number(index, def)

        return {
            value = minilight_raw.readState_number(ref),
            write = function(v)
                return minilight_raw.writeState_number(ref, v)
            end
        }
    elseif (type(def) == "string") then
        local ref = minilight_raw.newState_string(index, def)

        return {
            value = minilight_raw.readState_string(ref),
            write = function(v)
                return minilight_raw.writeState_string(ref, v)
            end
        }
    elseif (type(def) == "bool") then
        local ref = minilight_raw.newState_bool(index, def)

        return {
            value = minilight_raw.readState_bool(ref),
            write = function(v)
                return minilight_raw.writeState_bool(ref, v)
            end
        }
    elseif (type(def) == "table") then
        local ref = minilight_raw.newState_table(index, def)

        return {
            value = minilight_raw.readState_table(ref),
            write = function(v)
                return minilight_raw.writeState_table(ref, v)
            end
        }
    else
        error("Type(" .. type(def) .. ") is not supported!")
    end
end

return mod
