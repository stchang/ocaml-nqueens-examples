- to avoid stack overflow error for nqueens 8:

export OCAMLRUNPARAM=l=100M
ocamlc nqueens_lazy_proper.ml (or whatever file you want to run)
time ocamlrun a.out


- to load file in interpreter use toplevel directive #use:
#use "nqueens_eager.ml"
(the # is part of the directive and is not the same as the # prompt)
(http://caml.inria.fr/pub/docs/manual-ocaml/manual023.html)
