build:
		dune build tldr.exe
		cp ./_build/default/tldr.exe ./tldr
	  chmod 755 tldr

clean:
		rm -rf _build
		rm -rf ./tldr
