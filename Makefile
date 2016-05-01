all: libmpv-cut.so

mpv-cut_stub.h: mpv-cut.hs
	ghc $^ -o $@.o -c -Wall -no-hs-main

libmpv-cut.so: mpv-cut.hs lualibhelper.o
	ghc $^ -o $@ -Wall -no-hs-main -shared -fPIC -dynamic -lHSrts-ghc7.10.3

lualibhelper.o: lualibhelper.c mpv-cut_stub.h
	gcc $< -o $@ -c -Wall -I/usr/include/lua5.2 -I/usr/lib/ghc-7.10.3/include

clean:
	rm -f -v *.hi
	rm -f -v *.o
	rm -f -v *.so
	rm -f -v *_stub.h
