open Hw1;;

type simp_type = S_Elem of string | S_Arrow of simp_type * simp_type

type hm_lambda = HM_Var of string | HM_Abs of string * hm_lambda | HM_App of hm_lambda * hm_lambda | HM_Let of string * hm_lambda * hm_lambda
type hm_type = HM_Elem of string | HM_Arrow of hm_type * hm_type | HM_ForAll of string * hm_type

exception Inference_error of string
exception Algorithm_error of string

let rec string_of_s_type s_t = match s_t with
    | S_Elem name -> name
    | S_Arrow (s_t1, s_t2) -> "(" ^ (string_of_s_type s_t1) ^ " -> " ^ (string_of_s_type s_t2) ^ ")";;

let rec string_of_hm_type h_t = match h_t with
    | HM_Elem name -> name
    | HM_Arrow (h_t1, h_t2) -> "(" ^ (string_of_hm_type h_t1) ^ " -> " ^ (string_of_hm_type h_t2) ^ ")"
    | HM_ForAll (name, h_t) -> "[forall " ^ name ^ "." ^ (string_of_hm_type h_t) ^ "]";;

let rec string_of_hm_lambda h_l = match h_l with
    | HM_Var name -> name
    | HM_App (h_l1, h_l2) -> "(" ^ (string_of_hm_lambda h_l1) ^ " " ^ (string_of_hm_lambda h_l2) ^ ")"
    | HM_Abs (name, h_l) -> "\\" ^ name ^ "." ^ (string_of_hm_lambda h_l)
    | HM_Let (name, h_l1, h_l2) -> "let " ^ name ^ " = " ^ (string_of_hm_lambda h_l1) ^ " in " ^ (string_of_hm_lambda h_l2)

module T_map = Map.Make(String);;
module T_set = Set.Make(String);;

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

let debug = false;;

let algorithm_w lambda =
    let ctr = ref 0 in

    let new_type () = ctr := !ctr + 1; HM_Elem ("var_type" ^ string_of_int(!ctr)) in

    let rec transform_to_term t = match t with
        | HM_Elem name -> Hw2_unify.Var name
        | HM_Arrow (t1, t2) -> Hw2_unify.Fun("->", [transform_to_term t1; transform_to_term t2])
        | HM_ForAll _ -> raise (Algorithm_error "You doing something wrong") in

    let rec transform_to_type t = match t with
        | Hw2_unify.Var name -> HM_Elem name
        | Hw2_unify.Fun ("->", [t1; t2]) -> HM_Arrow (transform_to_type t1, transform_to_type t2)
        | _ -> raise (Algorithm_error "You doing something wrong") in

    let apply_substitution subst t =
        let rec subst_in_type binded t = match t with
            | HM_Elem v -> if ((T_map.mem v subst) && not (T_set.mem v binded)) then T_map.find v subst else HM_Elem v
            | HM_Arrow (t1, t2) -> HM_Arrow (subst_in_type binded t1, subst_in_type binded t2)
            (* looks like should subst just free vars *)
            | HM_ForAll (v, t) -> HM_ForAll (v, subst_in_type (T_set.add v binded) t) in

        subst_in_type (T_set.empty) t in

    (* maybe should add case forall a1. forall a1. a1 *)
    let rec choose_var_type acc var v_t = match v_t with
        | HM_ForAll (al, t) -> let bt = new_type() in
                                   choose_var_type (T_map.add al bt acc) var t
        | _ -> (apply_substitution acc v_t) in

    let assign_var_type context var =
        if T_map.mem var context then choose_var_type (T_map.empty) var (T_map.find var context)
        else raise (Inference_error ("Can't infer type of " ^ var)) in

    (* s2 applied first *)
    let subst_compose s1 s2 =
        let new_s2 = T_map.map (fun el -> apply_substitution s1 el) s2 in
            T_map.union (fun key el1 el2 -> Some el1) new_s2 s1 in

    let subst_to_context subst context =
        T_map.map (fun t -> apply_substitution subst t) context in

    let unify t1 t2 =
        (* if debug then (
            let tt1, tt2 = transform_to_term t1, transform_to_term t2 in
                print_string((Hw2_unify.string_of_term tt1) ^ "\n");
                print_string((Hw2_unify.string_of_term tt2) ^ "\n\n\n")
                ); *)
        match Hw2_unify.solve_system [(transform_to_term t1, transform_to_term t2)] with
            | None -> raise (Inference_error ("Can't unify " ^ string_of_hm_type t1 ^ " and " ^ string_of_hm_type t2))
            | Some solution -> let tmp = ref T_map.empty in List.iter (fun (key, term) -> tmp := T_map.add key (transform_to_type term) !tmp) solution; !tmp in

    let rec free_vars_lambda hm_lambda b_set = match hm_lambda with
        | HM_Var name -> if not (T_set.mem name b_set) then T_set.singleton name else T_set.empty
        | HM_App (h_l1, h_l2) -> T_set.union (free_vars_lambda h_l1 b_set) (free_vars_lambda h_l2 b_set)
        | HM_Abs (name, h_l) -> free_vars_lambda h_l (T_set.add name b_set)
        | HM_Let (name, h_l1, h_l2) -> T_set.union (free_vars_lambda h_l1 b_set) (free_vars_lambda h_l2 (T_set.add name b_set)) in

    (* launch with b_set = T_set.empty *)
    let rec free_vars t b_set = match t with
        | HM_Elem name -> if not (T_set.mem name b_set) then T_set.singleton name else T_set.empty
        | HM_Arrow (t1, t2) -> T_set.union (free_vars t1 b_set) (free_vars t2 b_set)
        | HM_ForAll (name, t) -> free_vars t (T_set.add name b_set) in

    let free_vars_in_context context =
        T_map.fold (fun key t acc -> T_set.union acc (free_vars t T_set.empty)) context (T_set.empty) in

    let cl_in_context t context =
        let free_in_t = free_vars t T_set.empty in
            let free = T_set.diff free_in_t (T_set.inter free_in_t (free_vars_in_context context)) in
                T_set.fold (fun free_var acc -> HM_ForAll (free_var, acc)) free t in

    (* subst : String, hm_type Map *)
    (* context : String, hm_type Map *)
    let rec inner_w (context, lambda) = match lambda with
        | HM_Var var ->           let t = assign_var_type context var in
                                      if debug then print_string("Var\n" ^ var ^ "\n" ^ (string_of_hm_type t) ^ "\n\n");
                                      (T_map.empty, t)
        | HM_App (l1, l2) ->      let (subst1, t1) = inner_w (context, l1) in
                                      let (subst2, t2) = inner_w (subst_to_context subst1 context, l2) in
                                          let n_t = new_type() in
                                              if debug then (print_string("App\n" ^ (string_of_hm_lambda l1) ^ "\n" ^ (string_of_hm_lambda l2) ^ "\n");
                                                            print_string((string_of_hm_type t1) ^ "\n" ^ (string_of_hm_type t2) ^ "\n");
                                                            print_string("Subst1 content:\n" ^ (String.concat "\n" (List.map (fun (key, s_t) -> (key ^ " = " ^ (string_of_hm_type s_t))) (T_map.bindings subst1))) ^ "\n");
                                                            print_string("Subst2 content:\n" ^ (String.concat "\n" (List.map (fun (key, s_t) -> (key ^ " = " ^ (string_of_hm_type s_t))) (T_map.bindings subst2))) ^ "\n")
                                                            );
                                              let new_subst = unify (apply_substitution subst2 t1) (HM_Arrow (t2, n_t)) in
                                                  if debug then print_string("New subst content:\n" ^ (String.concat "\n" (List.map (fun (key, s_t) -> (key ^ " = " ^ (string_of_hm_type s_t))) (T_map.bindings new_subst))) ^ "\n");
                                                  let res_subst = subst_compose new_subst (subst_compose subst1 subst2) in
                                                      if debug then print_string("Res subst content:\n" ^ (String.concat "\n" (List.map (fun (key, s_t) -> (key ^ " = " ^ (string_of_hm_type s_t))) (T_map.bindings res_subst))) ^ "\n\n");
                                                      (res_subst, apply_substitution res_subst n_t)
        (* TODO place where can be wrong *)
        (* figure out if I should check whether var is already in context *)
        | HM_Abs (var, l) ->      let n_t = new_type() in
                                      let (subst1, t1) = inner_w ((T_map.add var n_t context), l) in
                                          if debug then (print_string("Abs\n" ^ (string_of_hm_lambda l) ^ "\n" ^ (string_of_hm_type t1) ^ "\nSubst contents\n");
                                                        print_string((String.concat "\n" (List.map (fun (key, s_t) -> (key ^ " = " ^ (string_of_hm_type s_t))) (T_map.bindings subst1))) ^ "\n\n")
                                                        );
                                          (subst1, HM_Arrow ((apply_substitution subst1 n_t), t1))
        (* nu nafig *)
        | HM_Let (var, l1, l2) -> let (subst1, t1) = inner_w (context, l1) in
                                      (* if debug then print_string("Let first part\n" ^ (string_of_hm_type t1) ^ "\n\n"); *)
                                      let s1_context = subst_to_context subst1 context in
                                          let var_t = cl_in_context t1 s1_context in
                                              (* TODO place where can be wrong *)
                                              (* figure out if I should check whether var is already in context *)
                                              if debug then print_string("Let\n" ^ var ^ " : " ^ (string_of_hm_type var_t) ^ "\n");
                                              let (subst2, t2) = inner_w (T_map.union (fun key el1 el2 ->Some el2) s1_context (T_map.singleton var var_t), l2) in (* apply subst1 to context without var, union this with (var : for_all a1. ... for_all ak.t1, where a1, .., ak are free_vars of t1 and not free_vars of subst1 applied to context), l2 *)
                                                  if debug then (print_string("Let\n" ^ (string_of_hm_lambda l1) ^ "\n" ^ (string_of_hm_lambda l2) ^ "\n");
                                                                print_string((string_of_hm_type t1) ^ "\n" ^ (string_of_hm_type t2) ^ "\n\n")
                                                                );
                                                  (subst_compose subst2 subst1, t2) in

    if debug then print_string((string_of_hm_lambda lambda) ^ "\n");

    try (
        let free_var_types = ref T_map.empty in
        (T_set.iter (fun el -> free_var_types := (T_map.add el (new_type()) !free_var_types)) (free_vars_lambda lambda T_set.empty));
        match inner_w (!free_var_types, lambda) with (subst, t) -> Some (T_map.bindings subst, t)
    )
    with | (Inference_error e) | (Algorithm_error e) -> print_string(e ^ "\n"); None;;
