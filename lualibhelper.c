#include <stdio.h>
#include <stdint.h>
#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>
#include "mpv-cut_stub.h"

/** taken from liolib.c *******************************************************/
typedef luaL_Stream LStream;
#define tolstream(L)    ((LStream *)luaL_checkudata(L, 1, LUA_FILEHANDLE))
#define isclosed(p)     ((p)->closef == NULL)

static FILE *tofile (lua_State *L) {
    LStream *p = tolstream(L);
    if (isclosed(p))
        luaL_error(L, "attempt to use a closed file");
    lua_assert(p->f);
    return p->f;
}
////////////////////////////////////////////////////////////////////////////////

int l_add(lua_State *L)
{
    FILE *fp = tofile(L); ///< file must be 1st argument

    fprintf(fp, "oh yeah\n");

    printf("hsAdd returned %d\n", h_add(fp));

    uint8_t side;
    int number = luaL_checkint(L, 2);
    if (number >= 0 || number < (1 << (sizeof(side)*8)))
        side = number;
    else
        luaL_error(L, "side value exceeds limits");
    printf("got side = %hhu\n", side);

    lua_pushnumber(L, side);

    return 1;
}

int hs_init_lua(lua_State *L)
{
    hs_init(NULL, NULL);
    return 0;
}

int hs_exit_lua(lua_State *L)
{
    hs_exit();
    return 0;
}

int luaopen_lualibhelper(lua_State *L)
{
    lua_pushcfunction(L, hs_init_lua);
    lua_setglobal(L, "hs_init");
    lua_pushcfunction(L, hs_exit_lua);
    lua_setglobal(L, "hs_exit");

    lua_pushcfunction(L, (int (*)(lua_State*)) l_add);
    lua_setglobal(L, "hsAdd");

    return 0;
}
