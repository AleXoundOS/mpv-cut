all: libmpv-cut.so

#OPTIM=-O2
OPTIM=

mpv-cut_stub.h: mpv-cut.hs
	ghc $^ -c -Wall -no-hs-main ${OPTIM}

libmpv-cut.so: mpv-cut.hs lualibhelper.o
	ghc $^ -o $@ -Wall -no-hs-main ${OPTIM} -shared -fPIC -dynamic -lHSrts-ghc7.10.3

lualibhelper.o: lualibhelper.c mpv-cut_stub.h
	gcc $< -o $@ -c -Wall ${OPTIM} -I/usr/include/lua5.2 -I/usr/lib/ghc-7.10.3/include

clean:
	rm -f -v *.hi
	rm -f -v *.o
	rm -f -v *.so
	rm -f -v *_stub.h
