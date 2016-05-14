mylibinit = package.loadlib("./libmpv-cut.so", "luaopen_lualibhelper")
mylibinit()

f = io.open("script.sh", "w+")
--f = io.open("test_file", "w+")
--while true
--do
    --f:write("123\n")
    --f:flush()
print(hsAdd(f, "video_filename.flv", string.byte("A", 1), "0.734533"))
print(hsAdd(f, "video_filename.flv", string.byte("B", 1), "0.834533"))
print(hsAdd(f, "video_filename.flv", string.byte("A", 1), "1.734533"))
    --io.read()
--end
f:close()
