.PHONY: build test fmt coverage doc clean

build:
    opam exec -- dune build

test:
    opam exec -- dune runtest

fmt:
    opam exec -- dune fmt

coverage:
    opam exec -- dune runtest --instrument-with bisect_ppx
    opam exec -- bisect-ppx-report html --coverage-dir _coverage
    open _coverage/index.html

doc:
    opam exec -- dune build @doc

clean:
    opam exec -- dune clean
