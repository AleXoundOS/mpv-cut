function shutdown()
    print("caught shutdown event")
    if f ~= nil then
        print("closing file")
        f:close()
    else
        print("file was not opened!!!!!")
    end
end

function init()
    if f == nil then
        -- first try to open existing file
        f = io.open(mp.get_property("path") .. ".sh", "r+")
        if f == nil then
            -- create new file
            f = io.open(mp.get_property("path") .. ".sh", "w+")
        end

        mp.register_event("shutdown", shutdown)
    end

    if hslibinit == nil then
        -- load and initialize library
        local fn = debug.getinfo(1).source:match("@?(.*/)") .. "libmpv-cut.so"
        hslibinit = package.loadlib(fn, "luaopen_lualibhelper")
        hslibinit()
    end
end

function add(side)
    init()
    local timepos = mp.get_property("time-pos")
    local retCode = hsAdd( f, mp.get_property("filename")
                         , string.byte(side, 1), timepos )
    if retCode == 0 then
        mp.osd_message("add " .. side .. ":" .. timepos)
    elseif retCode == 3 then
        mp.osd_message(side .. ":" .. timepos .. " already added")
    elseif retCode == 1 then
        mp.osd_message("cannot parse existing script file")
    else
        mp.osd_message( "unknown error adding " .. side .. ":" .. timepos ..
                        " for " .. mp.get_property("filename") )
    end
end

function nav(direction)
    init()
    retCode, s, t = hsNav( f, mp.get_property("time-pos")
                            , string.byte(direction, 1) )
    print(retCode)
    print(s)
    print(t)
    if retCode == 0 then
        if s ~= nil then
            mp.osd_message(string.char(s) .. ": " .. t)
            mp.set_property("time-pos", t)
        else
            mp.osd_message("no " .. direction .. " timestamp")
        end
    elseif retCode == 1 then
        mp.osd_message("cannot parse existing script file")
    else
        mp.osd_message( "unknown error while navigating " .. direction ..
                        " in file " .. mp.get_property("filename") )
    end
end

-- add A--> timestamp
mp.add_forced_key_binding("a", function() add("A") end)

-- add -->B timestamp
mp.add_forced_key_binding("b", function() add("B") end)

-- add -->X<-- timestamp
mp.add_forced_key_binding("Ctrl+x", function() add("X") end)

-- navigate to next timestamp
mp.add_forced_key_binding("Ctrl+]", function() nav("forward") end)

-- navigate to previous timestamp
mp.add_forced_key_binding("Ctrl+[", function() nav("backward") end)
