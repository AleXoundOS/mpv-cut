mylibinit = package.loadlib("./libmpv-cut.so", "luaopen_lualibhelper")
mylibinit()

--f = io.open("test_file", "r+") -- should be so
f = io.open("test_file", "w+")
f:write("123\n")
f:flush()
while true
do
    print(hsAdd(f, 1))
    io.read()
end
f:close()
