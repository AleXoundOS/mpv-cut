function shutdown()
    print("caught shutdown event")
    if f ~= nil then
        print("closing file")
        f:close()
    else
        print("file was not opened!!!!!")
    end
end

mp.register_event("shutdown", shutdown)

-- add -->A mark
mp.add_forced_key_binding("a", function()
    mp.osd_message("A")
end)

-- add -->B mark
mp.add_forced_key_binding("b", function()
--local name = mp.get_property("filename")
    if f ~= nil then
        f = io.open(mp.get_property("path") .. ".timetable", "a")
    end
  --local time_pos = mp.get_property("time-pos")
  ----local time_pos = mp.get_property("playback-time")
  --f:write(time_pos .. "\n")
  --mp.osd_message("saved position " .. time_pos)
  --mp.set_property("time-pos", 10.0)
  --f:close()
    mp.osd_message("B")
end)

-- navigate to previous mark
mp.add_forced_key_binding("Ctrl+[", function()
    mp.osd_message("previous")
end)

-- navigate to next mark
mp.add_forced_key_binding("Ctrl+]", function()
    mp.osd_message("next")
end)
