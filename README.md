For non-regular ocaml users, here are directions to run the examples:

(* to avoid stack overflow error for nqueens 8 *)
export OCAMLRUNPARAM=l=100M

- from cmd line:
ocamlc nqueens_eager.ml
time ocamlrun a.out

- to load in interpreter:
#use "nqueens_eager.ml";;
(the # is part of the #use directive and is not the same as the # prompt)
(http://caml.inria.fr/pub/docs/manual-ocaml/manual023.html)
