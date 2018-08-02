open Hw1;;

type simp_type = S_Elem of string | S_Arrow of simp_type * simp_type

type hm_lambda = HM_Var of string | HM_Abs of string * hm_lambda | HM_App of hm_lambda * hm_lambda | HM_Let of string * hm_lambda * hm_lambda
type hm_type = HM_Elem of string | HM_Arrow of hm_type * hm_type | HM_ForAll of string * hm_type

let rec string_of_s_type s_t = match s_t with
    | S_Elem name -> name
    | S_Arrow (s_t1, s_t2) -> "(" ^ (string_of_s_type s_t1) ^ " -> " ^ (string_of_s_type s_t2) ^ ")";;

module T_map = Map.Make(String);;

let infer_simp_type lambda =
    let type_map = ref T_map.empty in

    let ctr = ref 0 in

    let new_type () = ctr := !ctr + 1; Hw2_unify.Var ("var_type" ^ string_of_int(!ctr)) in

    let bind_new_type var = let tmp = new_type () in type_map := T_map.add var tmp !type_map; tmp in

    let rec make_system lambda = match lambda with
        | Var var ->      let v_type = if (T_map.mem var !type_map) then T_map.find var !type_map
                                       else bind_new_type var in
                              ([], v_type)
        | App (l1, l2) -> let (system1, t1) = make_system l1 in
                              let (system2, t2) = make_system l2 in
                                  let app_type = new_type() in
                                      ((t1, Hw2_unify.Fun ("->", [t2; app_type])) :: system1 @ system2, app_type)
        | Abs (var, l) -> let (system, t) = make_system l in
                              let v_type = if (T_map.mem var !type_map) then T_map.find var !type_map
                                           else bind_new_type var in
                                  (system, Hw2_unify.Fun ("->", [v_type; t])) in

    let rec translate_to_s_type term = match term with
        | Hw2_unify.Var t_name -> S_Elem t_name
        | Hw2_unify.Fun ("->", [t1; t2]) -> S_Arrow (translate_to_s_type t1, translate_to_s_type t2)
        | other -> print_string("Problem with translation to s type of term" ^ Hw2_unify.string_of_term (other)); S_Elem "Wrong" in

    let system, t = make_system lambda in
        match Hw2_unify.solve_system system with
            | None -> None
            | Some subst -> let t_list = List.map (fun (var, t) -> (var, translate_to_s_type t)) subst in
                                let solution = Hw2_unify.apply_substitution subst t in
                                    Some (t_list, translate_to_s_type solution);;

let algorithm_w x = failwith "Not implemented";;
