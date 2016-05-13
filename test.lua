mylibinit = package.loadlib("./libmpv-cut.so", "luaopen_lualibhelper")
mylibinit()

f = io.open("syntax_highlight.html", "r+")
--f = io.open("test_file", "w+")
--while true
--do
    --f:write("123\n")
    --f:flush()
print(hsAdd(f, "filename", string.byte("A", 1), "0.734533"))
    --io.read()
--end
f:close()
