mylibinit = package.loadlib("./libmpv-cut.so", "luaopen_lualibhelper")
mylibinit()

hs_init()

f = io.open("test_file", "w")
f:write("123\n")
--f:flush()
print(hsAdd(f, 1))
f:close()

hs_exit()
