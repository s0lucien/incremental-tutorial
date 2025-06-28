.PHONY: all
all:
	@dune build solutions/main.exe exercises/main.exe

deps:
	sudo apt update
	sudo apt install -y libgmp-dev pkg-config
	@opam install async incremental sexp_pretty incr_map

server: all
	./_build/default/exercises/main.exe server -port 8080 -print-stats