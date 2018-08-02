open Hw1;;
open Hw1_reduction;;
open Hw2_unify;;
open Hw2_inference;;

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
print_int_list (rev ([6; 7]));;
print_string("\n");;

print_int_list (rev (rev ([6; 7])));;
print_string("\n\n");;

print_string("merge_sort\n");;
print_int_list (merge_sort ([6; 7]));;
print_string("\n");;

print_int_list (merge_sort ([6; 7; 1; 2; 3; 19]));;
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

let print_lambda lambda = print_string(string_of_lambda lambda);;

print_string("normal_beta_reduction\n");;
print_lambda(normal_beta_reduction (lambda_of_string "a"));;
print_string("\n");;
print_lambda(normal_beta_reduction (lambda_of_string "(\\x.x) a"));;
print_string("\n");;
print_lambda(normal_beta_reduction (lambda_of_string "(\\x.y) a"));;
print_string("\n");;
print_lambda(normal_beta_reduction (lambda_of_string "(\\x.\\y.x) y"));;
print_string("\n\n");;

print_string("free_vars\n");;
print_string_list(free_vars (lambda_of_string ("(\\x.\\y.x) y")));;
print_string("\n");;
print_string("\n\n");;

print_string("------THIRD HW----\n\n");;
print_string("system_to_equation\n");;

let print_term term = print_string (string_of_term term);;

let print_eq eq = print_string (string_of_eq eq);;

let print_system sys = print_string (string_of_system sys);;

let eq1 = (Var "a", Var "b");;
let eq2 = (Fun ("r", [Var "a"]), Fun ("g", [Var "b"]));;
let eq3 = (Fun ("fun_name1", [Fun ("fun_name2", [Fun ("fun_name3", [Var "kk"])])]), Fun ("fun_name4", [Fun ("fun_name5", [Fun ("fun_name6", [Var "gg"])])]));;
print_eq(system_to_equation [eq1; eq2]);;
print_string("\n");;
print_eq(system_to_equation [eq1; eq2; eq3]);;
print_string("\n");;
let sys = [eq1];;
let sys = [system_to_equation sys];;
let sys = [system_to_equation sys];;
print_eq(system_to_equation sys);;
print_string("\n\n");;

print_string("apply_substitution\n");;
let subst = [("a", Var "kek"); ("b", Fun ("very_smart_function", [Var "kek"; Var "kek"; Var "kek"]))];;

print_term(apply_substitution subst (Var "a"));;
print_string("\n");;
print_term(apply_substitution subst (Var "b"));;
print_string("\n");;
print_term(apply_substitution subst (Var "c"));;
print_string("\n\n");;

print_string("check_solution\n");;
let smart_sys = [(Var "kek", Var "a")];;
print_bool(check_solution subst smart_sys);;
print_string("\n");;
let smarter_sys = [(Var "b", Fun ("very_smart_function", [Var "kek"; Var "kek"; Var "kek"]))];;
print_bool(check_solution subst smarter_sys);;
print_string("\n");;
let smart_but_wrong_sys = [(Var "r", Fun ("r", [Var "r"]));(Fun ("e", [Var "a"]), Fun ("e", [Var "a"; Var "b"]))];;
print_bool(check_solution subst smart_but_wrong_sys);;
print_string("\n\n");;


let print_solved_system opt = match opt with
    | None -> print_string("No solution, sorry(\n")
    | Some sys -> print_string(String.concat "\n" (List.map (fun el -> match el with (var, term) -> var ^ " = " ^ (string_of_term term)) sys));;


print_string("solve_system\n");;
let eq1 = (Fun("A", [Var "b"]), Var "a");;
let eq2 = (Fun("B", [Var "c"; Var "a"]), Fun("B", [Fun("C", []); Var "a"]));;
let eq3 = (Fun ("fun", [Var "a"]), Fun ("fun", [Var "f"]));;
let eq4 = (Var "d", Fun("other", [Var "e"]));;
let sys = [eq1; eq1; eq2; eq3; eq4];;
let res = solve_system sys;;
print_solved_system(res);;
print_string("\n\n");;
let apply_subst_to_sys subst sys = List.map (fun elem -> match elem with (t1, t2) -> (apply_substitution subst t1, apply_substitution subst t2)) sys;;
let check_correctness sys res = match res with
    | None -> print_string("Nothing to subst\n\n")
    | Some subst -> print_system(apply_subst_to_sys subst sys);
                    print_string("\n\n");;

check_correctness sys res;;
(* TODO: good tests, not this something*)
print_string("-----FOURTH HW----\n\n");;

let print_s_type s_t = print_string(string_of_s_type s_t);;

let lambda1 = Abs ("f", Abs ("x", App (Var "f", App (Var "f", Var "x"))));;

let check_infer_type lambda = match infer_simp_type lambda with
    | None -> print_string("Failed to infer\n\n")
    | Some (l, res) -> print_string("Substs:\n");
                       List.iter (fun (key, s_t) -> print_string(key ^ " = " ^ (string_of_s_type s_t) ^ "\n")) l;
                       print_string("Inferred type:\n");
                       print_s_type res;
                       print_string("\n\n");;

check_infer_type lambda1;;
print_string("\n");;
check_infer_type (lambda_of_string "\\a.\\b.\\c.\\d.\\e.\\f.\\g.(a b c d e f g)");;
