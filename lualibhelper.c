#include <stdio.h>
#include <stdint.h>
#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>
#include "MPV_Cut_stub.h"

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

    /** getting filename of video source */
    const char *filename = luaL_checkstring(L, 2);
    if (!filename)
        luaL_error(L, "cannot parse time string");

    /** getting side */
    char side = '\0';
    int number = luaL_checkint(L, 3);
    if (number >= 0 || number < (1 << (sizeof(side)*8)))
        side = number;
    else
        luaL_error(L, "side value exceeds limits");

    /** getting time in string format */
    const char *time = luaL_checkstring(L, 4);
    if (!time)
        luaL_error(L, "cannot parse time string");

    int retCode = h_add(fp, (HsPtr *) filename, side, (HsPtr *) time);

    lua_pushnumber(L, retCode);

    return 1;
}

int l_nav(lua_State *L)
{
    FILE *fp = tofile(L); ///< file must be 1st argument

    /** getting input time in string format */
    const char *inTime = luaL_checkstring(L, 2);
    if (!inTime)
        luaL_error(L, "cannot parse time string");

    /** getting direction */
    char direction;
    int number = luaL_checkint(L, 3);

    if (number >= 0 || number < (1 << (sizeof(direction)*8)))
        direction = number;
    else
        luaL_error(L, "side value exceeds limits");

    char pos = '\0';
    char side = '\0';
    char *outTime = NULL;
    int retCode = h_nav(fp, (HsPtr *) inTime, direction, &pos, &side, &outTime);

    lua_pushnumber(L, retCode);
    if (retCode == 0) {
        if (pos != '\0') {
            lua_pushnumber(L, pos);
            lua_pushnumber(L, side);
            lua_pushstring(L, outTime);
            return 4;
        }
        else {
            lua_pushnil(L);
            return 2;
        }
    }
    else
        return 1;
}

int l_del(lua_State *L)
{
    FILE *fp = tofile(L); ///< file must be 1st argument

    /** getting input time in string format */
    const char *inTime = luaL_checkstring(L, 2);
    if (!inTime)
        luaL_error(L, "cannot parse time string");

    int retCode = h_del(fp, (HsPtr *) inTime);

    lua_pushnumber(L, retCode);

    return 1;
}

static void lib_enter(void) __attribute__((constructor));
static void lib_enter(void)
{
    //static char *argv[] = { "libmpv-cut.so", 0 }, **argv_ = argv;
    //static int argc = 1;
    //hs_init(&argc, &argv_);
    hs_init(NULL, NULL);
}

static void lib_exit(void) __attribute__((destructor));
static void lib_exit(void)
{
    hs_exit();
}

int luaopen_lualibhelper(lua_State *L)
{
    lua_pushcfunction(L, (int (*)(lua_State*)) l_add);
    lua_setglobal(L, "hsAdd");
    lua_pushcfunction(L, (int (*)(lua_State*)) l_nav);
    lua_setglobal(L, "hsNav");
    lua_pushcfunction(L, (int (*)(lua_State*)) l_del);
    lua_setglobal(L, "hsDel");

    return 0;
}
