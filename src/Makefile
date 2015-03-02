.PHONY: all clean debug parser doc

all:
	ocamlbuild -no-links  -lflag nums.cmxa main.native 

debug:
	ocamlbuild  -no-links  -lflag nums.cma main.d.byte

parser:
	menhir --external-tokens Tokens -b parser _parser.mly

doc:
	ocamlbuild -no-links main.docdir/index.html
clean:
	ocamlbuild -clean
