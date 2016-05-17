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
    end

    if hslibinit == nil then
        -- load and initialize library
        local fn = debug.getinfo(1).source:match("@?(.*/)") .. "libmpv-cut.so"
        hslibinit = package.loadlib(fn, "luaopen_lualibhelper")
        hslibinit()
    end
end

mp.register_event("shutdown", shutdown)

-- add A--> timestamp
mp.add_forced_key_binding("a", function()
    init()
    mp.osd_message("A")
    hsAdd( f, mp.get_property("filename")
         , string.byte("A", 1), mp.get_property("time-pos") )
end)

-- add -->B timestamp
mp.add_forced_key_binding("b", function()
    init()
    mp.osd_message("B")
    hsAdd( f, mp.get_property("filename")
         , string.byte("B", 1), mp.get_property("time-pos") )
end)

-- navigate to previous timestamp
mp.add_forced_key_binding("Ctrl+[", function()
    init()
    retCode, s, t = hsNav(f, mp.get_property("time-pos"), string.byte("b", 1))
    print(retCode)
    print(s)
    print(t)
    if retCode == 0 then
        if s ~= 0 then
            mp.osd_message(string.char(s) .. ": " .. t)
            mp.set_property("time-pos", t)
        else
            mp.osd_message("no previous timestamp")
        end
    else
        mp.osd_message("previous -> error " .. retCode)
    end
end)

-- navigate to next timestamp
mp.add_forced_key_binding("Ctrl+]", function()
    init()
    retCode, s, t = hsNav(f, mp.get_property("time-pos"), string.byte("f", 1))
    print(retCode)
    print(s)
    print(t)
    if retCode == 0 then
        if s ~= 0 then
            mp.osd_message(string.char(s) .. ": " .. t)
            mp.set_property("time-pos", t)
        else
            mp.osd_message("no next timestamp")
        end
    else
        mp.osd_message("next -> error " .. retCode)
    end
end)
