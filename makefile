run:
	dune exec -- ./bin/main.exe

autobuild:
	dune build --watch

clean:
	dune clean

build:
	dune build