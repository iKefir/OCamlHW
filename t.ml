open Hw1;;
open Hw1_reduction;;

print_string("------FIRST HW----\n\n");;

print_string("----FIRST PART----\n\n");;

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

print_string("\n\n---SECOND PART----\n\n");;

print_string("rev\n");;
print_int_list (rev (6 :: 7 :: []));;
print_string("\n");;

print_int_list (rev (rev (6 :: 7 :: [])));;
print_string("\n\n");;

print_string("merge_sort\n");;
print_int_list (merge_sort (6 :: 7 :: []));;
print_string("\n");;

print_int_list (merge_sort (6 :: 7 :: 1 :: 2 :: 3 :: 19 :: []));;
print_string("\n\n");;

print_string("\n\n----THIRD PART----\n\n");;

print_string("string of lambda\n");;
print_string(string_of_lambda (App ((Var "b"), (Abs ("x", Abs ("y", Var "a"))))));;
print_string("\n\n");;

print_string("lambda of string\n");;
print_string (string_of_lambda (lambda_of_string "\\x.\\y.x"));;
print_string("\n\n");;

print_string (string_of_lambda (lambda_of_string "b \\x.\\y.a"));;
print_string("\n\n");;

print_string(debug_string_of_lambda (lambda_of_string ("a b c d e f g")));;
print_string("\n\n");;

print_string("b \\x.\\y.a");;
print_string("\n");;
print_string(string_of_lambda(lambda_of_string "b \\x.\\y.a"));;
print_string("\n");;
print_string(string_of_lambda(lambda_of_string (string_of_lambda (lambda_of_string "b \\x.\\y.a"))));;
print_string("\n\n");;

print_string (string_of_lambda(lambda_of_string (string_of_lambda (lambda_of_string "(b (\\x.(\\y.a)))"))));;
print_string("\n\n");;

print_string("-----SECOND HW----\n\n");;

let print_bool bl = print_string(string_of_bool bl);;

print_string("free_to_subst\n");;
print_string("a in x instead x\n");;
print_bool (free_to_subst (lambda_of_string "a") (lambda_of_string "x") "x");;
print_string("\n");;
print_string("a in b instead x\n");;
print_bool (free_to_subst (lambda_of_string "a") (lambda_of_string "b") "x");;
print_string("\n");;
print_string("\\x.\\y.x in \\x.\\y.x instead x\n");;
print_bool (free_to_subst (lambda_of_string "\\x.\\y.x") (lambda_of_string "\\x.\\y.x") "x");;
print_string("\n");;
print_string("\\x.\\y.x in (\\x.\\y.x) x instead x\n");;
print_bool (free_to_subst (lambda_of_string "\\x.\\y.x") (lambda_of_string "(\\x.\\y.x) x") "x");;
print_string("\n");;
print_string("\\a.\\b.a in \\x.\\y.x instead x\n");;
print_bool (free_to_subst (lambda_of_string "\\a.\\b.a") (lambda_of_string "\\x.\\y.x") "x");;
print_string("\n");;
print_string("x in (\\x.\\y.v) x instead v\n");;
print_bool (free_to_subst (lambda_of_string "x") (lambda_of_string "(\\x.\\y.v) x") "v");;
print_string("\n");;
print_string("\\y.z in \\x.v instead v\n");;
print_bool (free_to_subst (lambda_of_string "\\y.z") (lambda_of_string "\\x.v") "v");;
print_string("\n\n");;

print_string("free_vars\n");;
print_string_list(free_vars (lambda_of_string ("\\x.\\y.x y")));;
print_string("\n");;
print_string_list(free_vars (lambda_of_string ("\\x.\\y.x y z")));;
print_string("\n");;
print_string_list(free_vars (lambda_of_string ("\\x.\\y.z \\z.y r")));;
print_string("\n\n");;

print_string("is_alpha_equivalent\n");;
print_bool (is_alpha_equivalent (lambda_of_string "\\x.\\y.x") (lambda_of_string "\\x.\\y.x"));;
print_string("\n");;
print_bool (is_alpha_equivalent (lambda_of_string "\\y.\\x.y") (lambda_of_string "\\x.\\y.x"));;
print_string("\n");;
print_bool (is_alpha_equivalent (lambda_of_string "y x") (lambda_of_string "x y"));;
print_string("\n");;
print_bool (is_alpha_equivalent (lambda_of_string "(\\x.\\y.\\x.x) a") (lambda_of_string "(\\x.\\y.\\z.x) a"));;
print_string("\n");;
print_bool (is_alpha_equivalent (lambda_of_string "\\x.\\subst_arg0.x subst_arg0") (lambda_of_string "\\y.\\subst_arg0.y subst_arg0"));;
print_string("\n");;
print_bool (is_alpha_equivalent (lambda_of_string "\\x.\\y.x") (lambda_of_string "b \\x.\\y.x"));;
print_string("\n\n");;

print_string("is_normal_form\n");;
print_bool(is_normal_form (lambda_of_string "a"));;
print_string("\n");;
print_bool(is_normal_form (lambda_of_string "a b c"));;
print_string("\n");;
print_bool(is_normal_form (lambda_of_string "\\x.x"));;
print_string("\n");;
print_bool(is_normal_form (lambda_of_string "(\\x.x) b"));;
print_string("\n");;
print_bool(is_normal_form (lambda_of_string "a (\\x.x) b c d"));;
print_string("\n");;
print_bool(is_normal_form (lambda_of_string "a \\x.x b c d"));;
print_string("\n\n");;
