all: libmpv-cut.so

#OPTIM=-O2
OPTIM=

MPV_Cut_stub.h: MPV_Cut.hs
	ghc $^ -c -Wall -no-hs-main ${OPTIM} -fPIC -dynamic

libmpv-cut.so: MPV_Cut.hs lualibhelper.o
	ghc $^ -o $@ -Wall -no-hs-main ${OPTIM} -shared -fPIC -dynamic -lHSrts-ghc7.10.3

lualibhelper.o: lualibhelper.c MPV_Cut_stub.h
	gcc $< -o $@ -c -Wall -fPIC ${OPTIM} -I/usr/include/lua5.2 -I/usr/lib/ghc-7.10.3/include

clean:
	rm -f -v *.hi
	rm -f -v *.o
	rm -f -v *.so
	rm -f -v *_stub.h
