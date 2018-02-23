open Hw1;;

(* a instead str in b *)
let subst a b str =
	let rec subst_help a b str = match b with
    	| Var x -> if (x = str) then a else Var x
    	| App (x, y) -> App (subst_help a x str, subst_help a y str)
    	| Abs (x, lambda) -> if (x = str) then Abs (x, lambda) else Abs (x, subst_help a lambda str)

	in match b with
		| Abs (x, lambda) -> (subst_help a b str)
		| _ -> b;;

(* a instead str in b *)
let free_to_subst a b str =

    let rec new_intersection lambda str used_list = match lambda with
        | Var x -> ((x = str) && (not (List.mem x used_list)))
        | App (x, y) -> ((new_intersection x str used_list) || (new_intersection y str used_list))
        | Abs (x, lambda) -> (new_intersection lambda str (x::used_list)) in

    let rec free_subst_help b used_list has_intersections = match b with
        | Var x ->
            (
                (
                    (x = str) &&
                    (
                        (
                            (not (List.mem x used_list)) && (not has_intersections)
                        ) || (
                            (List.mem x used_list)
                        )
                    )
                ) || (
                    not(x = str)
                )
            )
        | App (x, y) -> ((free_subst_help x used_list has_intersections) && (free_subst_help y used_list has_intersections))
        | Abs (x, lambda) -> (free_subst_help lambda (x::used_list) (has_intersections || (new_intersection a x [])))

    in free_subst_help b [] false;;

let free_vars a =
	let rec free_vars_help a used_list = match a with
		| Var x -> if (List.mem x used_list) then [] else x::[]
		| App (x, y) -> ((free_vars_help x used_list) @ (free_vars_help y used_list))
		| Abs (x, lambda) -> (free_vars_help lambda (x::used_list))

	in free_vars_help a [];;

let is_alpha_equivalent a b =
    let ctr = ref 0 in
    let next() = ctr := !ctr + 1 in
    let get_subst_arg() = Var ("subst_arg" ^ (string_of_int !ctr)) in

    let rec alpha_eq_help a b = match (a, b) with
    | (Var ax, Var bx) -> ax = bx
    | (App (a1, a2), App (b1, b2)) -> ((alpha_eq_help a1 b1) && (alpha_eq_help a2 b2))
    | (Abs (ax, alambda), Abs (bx, blambda)) ->
        if ((free_to_subst (get_subst_arg()) alambda ax) && (free_to_subst (get_subst_arg()) blambda bx)) then
            (alpha_eq_help (subst (get_subst_arg()) alambda ax) (subst (get_subst_arg()) blambda bx))
        else    (next();
                alpha_eq_help a b)
    | _ -> false

in alpha_eq_help a b;;

let rec is_normal_form lambda = match lambda with
    | App (Abs (tmp0, tmp1), tmp2) -> false
    | Var tmp0 -> true
    | App (tmp0, tmp1) -> (is_normal_form tmp0) && (is_normal_form tmp1)
    | Abs (tmp0, tmp1) -> is_normal_form tmp1;;

(* (\x.\y.x) (\y.\x.(\y.y) y) -> \y.(\y1.\x.(\y1.y1) y1) *)

(* let normal_beta_reduction a = match a with
	| App (Abs (x, lambda) other_lambda) -> if (free_to_subst other_lambda lambda x) then (subst other_lambda lambda x)

	| _ -> expr2 *)
