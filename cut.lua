function shutdown()
    print("caught shutdown event")
    if f ~= nil then
        print("closing file")
        f:close()
    else
        print("file was not opened!!!!!")
    end
end

function init(mode)
    if f == nil then
        if hslibinit == nil then
            -- load and initialize library
            local fn = debug.getinfo(1).source:match("@?(.*/)") .. "libmpv-cut.so"
            hslibinit = package.loadlib(fn, "luaopen_lualibhelper")
            hslibinit()
        end

        -- first try to open existing file
        f = io.open(mp.get_property("path") .. ".sh", "r+")
        if f == nil and mode ~= "ro" then
            -- create new file
            f = io.open(mp.get_property("path") .. ".sh", "w+")

            -- choose extension depending on media type: video or audio only
            local ext = "mkv"
            local audioOnly = 0
            if mp.get_property("video-format") == nil then
                ext = ""
                audioOnly = 1
            end

            -- write config (filename, extension) for the current playback file
            hsCfg(f, mp.get_property("filename"), ext, audioOnly)
        end

        if f == nil then
            return -1
        end

        mp.register_event("shutdown", shutdown)
    end
end

function add(side)
    init()
    local timepos = mp.get_property("time-pos")
    local retCode = hsAdd(f, string.byte(side, 1), timepos)
    if retCode == 0 then
        mp.osd_message("add " .. side .. ":" .. timepos)
    elseif retCode == 3 then
        mp.osd_message(side .. ":" .. timepos .. " already added")
    elseif retCode == 1 then
        mp.osd_message("cannot parse existing script file")
    else
        mp.osd_message( "unknown error in adding " .. side .. ":" .. timepos ..
                        " for " .. mp.get_property("filename") )
    end
end

function nav(direction)
    if init("ro") == -1 then
        mp.osd_message("script doesn't exist for this file")
        return
    end
    local timepos = mp.get_property("time-pos")
    -- return code, position (f_irst, o_nly, l_ast, n_one of these), side, time
    local retCode, p, s, t = hsNav(f, timepos, string.byte(direction, 1))
    print(retCode)
    print(p)
    print(s)
    print(t)
    if retCode == 0 then
        if p ~= nil then
            if p == string.byte('o') then
                strPos = " (only)"
            elseif p == string.byte('f') then
                strPos = " (first)"
            elseif p == string.byte('l') then
                -- the supplied time is the only timestamp in script
                strPos = " (last)"
            else
                strPos = ""
            end
            mp.osd_message(string.char(s) .. ": " .. t .. strPos)
            mp.set_property("time-pos", t)
        else
            mp.osd_message(timepos .. ": no " .. direction .. " timestamps")
        end
    elseif retCode == 1 then
        mp.osd_message("cannot parse existing script file")
    else
        mp.osd_message( "unknown error while navigating " .. direction ..
                        " in file " .. mp.get_property("filename") )
    end
end

function del()
    init()
    local timepos = mp.get_property("time-pos")
    local retCode = hsDel(f, timepos)

    if retCode == 0 then
        mp.osd_message("delete " .. timepos)
    elseif retCode == 3 then
        mp.osd_message(timepos .. " does not exist")
    else
        mp.osd_message( "unknown error in deleting " .. timepos ..
                        " for " .. mp.get_property("filename") )
    end
end

function cut(mode)
    print(mp.get_property("path") .. ".sh")
    x, y, retCode = os.execute( "bash \"" .. mp.get_property("path") .. ".sh\" "
                              .. mode )
    if retCode == 0 then
        mp.osd_message("successfully cut")
    elseif retCode == 127 then
        mp.osd_message("script for cutting does not exist")
    elseif retCode == 1 and mode ~= "overwrite" then
        mp.osd_message("one or more cutted files already exist")
    else
        mp.osd_message( "unknown FFmpeg error return code ("
                      .. tostring(retCode) .. ")")
    end
end

-- add A--> timestamp
mp.add_forced_key_binding("a", function() add("A") end)

-- add -->B timestamp
mp.add_forced_key_binding("b", function() add("B") end)

-- add -->X<-- timestamp
mp.add_forced_key_binding("Ctrl+x", function() add("X") end)
mp.add_forced_key_binding('0x18', function() add("X") end) -- Ctrl+x for console

-- navigate to next timestamp
mp.add_forced_key_binding("\'", function() nav("forward") end)

-- navigate to previous timestamp
mp.add_forced_key_binding(";", function() nav("backward") end)

-- delete existing timestamp
mp.add_forced_key_binding("Ctrl+d", function() del() end)
mp.add_forced_key_binding('0x4', function() del() end) -- Ctrl+d for console

-- cut pieces
mp.add_forced_key_binding("C", function() cut("-n") end)

-- cut pieces overwriting existing in filesystem
mp.add_forced_key_binding("Ctrl+C", function() cut("-y") end)
