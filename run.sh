rm *.cmi *.cmo a.out
ocamlc hw1.mli hw1.ml hw1_reduction.mli hw1_reduction.ml hw2_unify.mli hw2_unify.ml t.ml
./a.out > res.txt
