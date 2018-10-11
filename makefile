build:
	dune build src/tldr.exe
	cp ./_build/default/src/tldr.exe ./tldr
	chmod 755 tldr

clean:
	rm -rf _build
	rm -rf ./tldr
