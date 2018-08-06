open Hw1;;

(* a instead str in b *)
let rec subst a b str = match b with
    | Var x -> if (x = str) then a else Var x
    | App (x, y) -> App (subst a x str, subst a y str)
    | Abs (x, lambda) -> if (x = str) then Abs (x, lambda) else Abs (x, subst a lambda str);;

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


(* not finished yet *)
module String_set = Set.Make(String);;

(* let differ_vars lambda =
    let ctr = ref 0 in
    let next() = ctr := !ctr + 1 in
    let get_subst_arg() = Var ("subst_arg" ^ (string_of_int !ctr)) in

    let rec differ_vars_help lambda set = match lambda with
        | Var tmp -> if (Set.mem tmp0 set) let new_var = get_subst_arg() in (Var new_var, Set.add new_var set) else (Var tmp, Set.add tmp set)
        | App (l1, l2) -> let new_l1, new_set1 = differ_vars_help l1 set in let new_l2, new_set2 = differ_vars_help l2, new_set1 in (App (new_l1, new_l2), new_set2)
        | Abs (var, lambda) -> Abs (var, differ_vars_help lambda)

    in differ_vars_help lambda String_set.empty;; *)

let rec is_normal_form lambda = match lambda with
    | App (Abs (tmp0, tmp1), tmp2) -> false
    | Var tmp -> true
    | App (tmp0, tmp1) -> (is_normal_form tmp0) && (is_normal_form tmp1)
    | Abs (tmp0, tmp1) -> is_normal_form tmp1;;

(* (\x.\y.x) (\y.\x.(\y.y) y) -> \y.(\y1.\x.(\y1.y1) y1) *)

let rec helper_normal_beta_reduction lambda = match lambda with
    | App (Abs (x, lambda1), lambda2) -> ((subst lambda2 lambda1 x), true)
    | Var a -> (Var a, false)
    | App (a, b) -> let a1, did_reduction1 = helper_normal_beta_reduction a in
                        if (did_reduction1 = false)
                        then let b1, did_reduction2 = helper_normal_beta_reduction b in
                            (App (a1, b1), did_reduction2)
                        else (App (a1, b), did_reduction1)
    | Abs (a, b) -> let b1, did_reduction1 = helper_normal_beta_reduction b in
                        (Abs (a, b1), did_reduction1);;

let normal_beta_reduction lambda = let res, did_reduction = helper_normal_beta_reduction lambda in res;;
