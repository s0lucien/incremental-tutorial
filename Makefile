.PHONY: all
all:
	@dune build solutions/main.exe exercises/main.exe

deps:
	@opam install async incremental sexp_pretty incr_map