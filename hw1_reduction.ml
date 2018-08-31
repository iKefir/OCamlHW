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
                            List.mem x used_list
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
    let get_subst_arg() = next(); Var ("subst_arg" ^ (string_of_int !ctr)) in

    let rec help a b = match (a, b) with
        | (Var ax, Var bx) -> ax = bx
        | (App (a1, a2), App (b1, b2)) -> ((help a1 b1) && (help a2 b2))
        | (Abs (ax, alambda), Abs (bx, blambda)) ->
            let n_arg = get_subst_arg() in
                if ((free_to_subst n_arg alambda ax) && (free_to_subst n_arg blambda bx)) then
                    (help (subst n_arg alambda ax) (subst n_arg blambda bx))
                else (help a b)
        | _ -> false

    in help a b;;

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

module V_set = Set.Make(String);;
module V_map = Map.Make(String);;

let normal_beta_reduction_informative lambda =
    let rec subst_with_rename a b str bound_set =
        let new_name () = "var_name" ^ string_of_int(Random.int 1000000000) in

        let generate_unique_name b_set =
            let tmp = ref (new_name()) in
                while (V_set.mem !tmp b_set) do (tmp := new_name()) done;
                !tmp in

        let free_set = V_set.of_list (free_vars a) in

        (* \x.p (\v1.\x.p x) x   <-  x instead p *)

        let rec help lambda b_set rename_map = match lambda with
            | Var x -> if (x = str)
                       then a
                       else if (V_map.mem x rename_map)
                            then Var (V_map.find x rename_map)
                            else Var x
            | App (x, y) -> App (help x b_set rename_map, help y b_set rename_map)
            | Abs (x, l) -> if (x = str)
                            then Abs (x, l)
                            else if (V_set.mem x free_set || V_set.mem x b_set)
                                 then if (V_map.mem x rename_map)
                                      then Abs ((V_map.find x rename_map), help l b_set rename_map)
                                      else let n_x = generate_unique_name (V_set.union free_set b_set) in
                                          Abs (n_x, help l (V_set.add n_x b_set) (V_map.add x n_x rename_map))
                                 else Abs (x, help l (V_set.add x b_set) rename_map) in

        Random.self_init ();
        help b bound_set (V_map.empty) in


    let rec helper lambda b_set = match lambda with
        | App (Abs (x, lambda1), lambda2) -> ((subst_with_rename lambda2 lambda1 x b_set), true)
        | Var a -> (Var a, false)
        | App (a, b) -> let a1, did_reduction1 = helper a b_set in
                            if (did_reduction1 = false)
                            then let b1, did_reduction2 = helper b b_set in
                                (App (a1, b1), did_reduction2)
                            else (App (a1, b), did_reduction1)
        (* nado esli abstrakcia po odnoi iz svobodnih peremennih - renamit ee *)
        | Abs (a, b) -> let b1, did_reduction1 = helper b (V_set.add a b_set) in
                            (Abs (a, b1), did_reduction1) in

    helper lambda (V_set.empty);;

let normal_beta_reduction lambda =
    let res, did_reduction = normal_beta_reduction_informative lambda in res;;

module Int = struct
   type t = int
   let compare x y = if x < y then -1 else if x > y then 1 else 0 end;;
module I_map = Map.Make(Int);;

let reduce_to_normal_form lambda =
    let free_map = ref (I_map.empty) in

    let transform_to_de_bruijn lambda =
        let free_set = (V_set.of_list (free_vars lambda)) in

        let generate_new_name () = "var_name" ^ string_of_int (Random.int 1000000000) in

        let new_name b_set =
            let candidate = ref (generate_new_name ()) in
                while V_set.mem !candidate b_set do candidate := generate_new_name() done;
                !candidate in

        (* launched only when var in stack *)
        let rec get_number num var b_stack =
            if var = Stack.top b_stack then num
            else get_number (num + 1) var (Stack.pop b_stack; b_stack) in

        let rec help lambda b_set b_stack = match lambda with
            | Var name -> if V_set.mem name b_set then Var (string_of_int (get_number 1 name (Stack.copy b_stack)))
                          else let new_ind = ref (Stack.length b_stack + 1) in
                              while (I_map.mem !new_ind !free_map) do new_ind := !new_ind + 1 done;
                              free_map := I_map.add !new_ind name !free_map;
                              Var (string_of_int !new_ind)
            | App (l1, l2) -> App (help l1 b_set (Stack.copy b_stack), help l2 b_set b_stack)
            | Abs (name, l) -> Abs (new_name free_set, help l (V_set.add name b_set) (let b_copy = Stack.copy b_stack in Stack.push name b_copy; b_copy)) in

        help lambda (V_set.empty) (Stack.create ()) in

    let transform_to_lambda db_lambda =
        let generate_new_name () = "var_name" ^ string_of_int (Random.int 1000000000) in

        let new_name b_set =
            let candidate = ref (generate_new_name ()) in
                while V_set.mem !candidate b_set do candidate := generate_new_name() done;
                !candidate in

        let rec get_name num b_stack =
            if num = 1 then Stack.top b_stack
            else get_name (num - 1) (Stack.pop b_stack; b_stack) in

        let rec help db_lambda b_set b_stack = match db_lambda with
            | Var name -> let name_parsed = int_of_string name in
                              if name_parsed > Stack.length b_stack then
                                  if (I_map.mem name_parsed !free_map) then Var (I_map.find name_parsed !free_map)
                                  else Var "bad"
                              else Var (get_name name_parsed (Stack.copy b_stack))
            | App (db_l1, db_l2) -> App (help db_l1 b_set (Stack.copy b_stack), help db_l2 b_set b_stack)
            | Abs (name, db_l) -> let n_name = if (V_set.mem name b_set) then new_name b_set else name in
                                      Abs (n_name, help db_l (V_set.add n_name b_set) (let b_copy = Stack.copy b_stack in Stack.push n_name b_copy; b_copy)) in

        help db_lambda (V_set.empty) (Stack.create ()) in

    let rec shift should_replace_top_val num depth full_depth db_lambda =
        match db_lambda with
        | Var name -> let name_parsed = int_of_string name in
                          if (depth < name_parsed)
                          then
                              let new_ind = ref (name_parsed + num) in
                              if (full_depth < name_parsed)
                              then
                                  (let mapped_name = if (I_map.mem name_parsed !free_map) then (I_map.find name_parsed !free_map) else "bad" in
                                  free_map := I_map.remove name_parsed !free_map;
                                  while (I_map.mem !new_ind !free_map) do new_ind := !new_ind + 1 done;
                                  free_map := I_map.add !new_ind mapped_name !free_map);
                              Var (string_of_int !new_ind)
                          else if (should_replace_top_val && depth = name_parsed) then Var "0"
                               else db_lambda
        | App (db_l1, db_l2) -> App (shift should_replace_top_val num depth full_depth db_l1, shift should_replace_top_val num depth full_depth db_l2)
        | Abs (name, db_l) -> Abs (name, shift should_replace_top_val num (depth + 1) (full_depth + 1) db_l) in

    let rec subst db_lambda1 db_lambda2 depth full_depth = match db_lambda2 with
        | Var name -> if int_of_string name = 0 then (shift false depth 0 full_depth db_lambda1) else db_lambda2
        | App (db_l1, db_l2) -> App (subst db_lambda1 db_l1 depth full_depth, subst db_lambda1 db_l2 depth full_depth)
        | Abs (name, db_l) -> Abs (name, subst db_lambda1 db_l (depth + 1) full_depth) in

    let rec b_reduction db_lambda depth = match db_lambda with
        | App (Abs (name, l1), l2) -> (subst l2 (shift true (-1) 1 (depth + 1) l1) 0 depth, true)
        | Var name -> (db_lambda, false)
        | App (db_l1, db_l2) -> let res1, did_reduction1 = b_reduction db_l1 depth in
                                    if (did_reduction1 = false)
                                    then let res2, did_reduction2 = b_reduction db_l2 depth in
                                        (App (res1, res2), did_reduction2)
                                    else (App (res1, db_l2), true)
        | Abs (name, db_l) -> let res, did_reduction = b_reduction db_l (depth + 1) in
                                (Abs (name, res), did_reduction) in

    let db_lambda = ref (transform_to_de_bruijn lambda) in
        let succ = ref true in
            while !succ do
                let (res, success) = b_reduction !db_lambda 0 in
                    db_lambda := res;
                    succ := success;
            done;
            transform_to_lambda !db_lambda;;
