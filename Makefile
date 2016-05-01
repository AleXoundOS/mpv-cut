all: libmpv-cut.so

test_stub.h: test.hs
	ghc $^ -o libmpv-cut.o -c -Wall -no-hs-main

libmpv-cut.so: test.hs lualibhelper.o
	ghc $^ -o $@ -Wall -no-hs-main -shared -fPIC -dynamic -lHSrts-ghc7.10.3

lualibhelper.o: test_stub.h mylib.c
	gcc mylib.c -o $@ -c -Wall -I/usr/include/lua5.2 -I/usr/lib/ghc-7.10.3/include
	@#gcc -Wall -shared -fPIC -I/usr/include/lua5.2 -I/usr/lib/ghc-7.10.3/include libmpv-cut.so -Wl,-rpath="." -o mylib.so mylib.c

clean:
	rm -f -v *.hi
	rm -f -v *.o
	rm -f -v *.so
	rm -f -v *_stub.h
