all:
	ocamlbuild -use-ocamlfind Board_tools.cma 
	ocamlbuild -use-ocamlfind Board_tools.cmxa 

install:
	ocamlfind install hardcaml-board-tools META \
		_build/Board_tools.cma _build/Board_tools.cmi \
		_build/Board_tools.cmxa _build/Board_tools.a

uninstall:
	ocamlfind remove hardcaml-board-tools

clean:
	ocamlbuild -clean
	rm -fr *~
