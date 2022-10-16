# Off-side ruled syntax parsing

Parsing an off-side ruled syntax with OCamlLex and Menhir.

## Getting Started

### Prerequisites

- [opam](https://opam.ocaml.org/)

### Installation

```bash
git clone https://github.com/sano-jin/python-in-ocaml
cd python-in-ocaml
opam install .
dune build
```

## Usage

run `./run example/sample1.py`

- syntax.ml
  - The definition of the syntax.
- lexer.mll
  - A file to pass to the ocamllex.
- parser.mly
  - A file to pass to the ocamlyacc.
