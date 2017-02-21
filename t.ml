open Hw1;;

print_string("----FIRST PART----\n\n")

print_string("int_of_peano\n");;
print_int (int_of_peano (S (S (Z))));;
print_string("\n\n");;

print_string("inc\n");;
print_int (int_of_peano (inc (peano_of_int 2)));;
print_string("\n\n");;

print_string("add\n");;
print_int (int_of_peano (add (peano_of_int 2) (peano_of_int 5)));;
print_string("\n\n");;

print_string("sub\n");;
print_int (int_of_peano (sub (peano_of_int 5) (peano_of_int 2)));;
print_string("\n");;

print_int (int_of_peano (sub (peano_of_int 2) (peano_of_int 5)));;
print_string("\n\n");;

print_string("mul\n");;
print_int (int_of_peano (mul (peano_of_int 5) (peano_of_int 2)));;
print_string("\n\n");;

print_string("div\n");;
print_int (int_of_peano (div (peano_of_int 15) (peano_of_int 2)));;
print_string("\n");;

print_int (int_of_peano (div (peano_of_int 2) (peano_of_int 5)));;
print_string("\n\n");;

print_string("power\n");;
print_int (int_of_peano (power (peano_of_int 15) (peano_of_int 2)));;
print_string("\n");;

print_int (int_of_peano (power (peano_of_int 2) (peano_of_int 5)));;
print_string("\n\n");;
(*
print_string("\n\n---SECOND PART----\n\n")
*)
(*
print_string (Hw1.string_of_lambda (Hw1.lambda_of_string "\\x.\\y.x"));;
*)