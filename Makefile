all:
	ocamlbuild -use-menhir -menhir 'menhir --explain' -libs str,unix main.native

clean:
	ocamlbuild -clean main.native

install:
	cp main.native /usr/bin/adaquery

uninstall:
	rm /usr/bin/adaquery
