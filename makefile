build:
	dune build src/tldr.exe

clean:
	rm -rf _build
	rm -rf ./tldr

install:
	cp ./_build/default/src/tldr.exe ./tldr
	chmod 755 tldr
