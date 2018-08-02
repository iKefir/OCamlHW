rm a.out
ocamlc hw1.mli hw1.ml hw1_reduction.mli hw1_reduction.ml hw2_unify.mli hw2_unify.ml hw2_inference.mli hw2_inference.ml t.ml
rm *.cmi *.cmo
./a.out > res.txt
