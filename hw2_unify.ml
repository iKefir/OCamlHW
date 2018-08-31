type algebraic_term = Var of string | Fun of string * (algebraic_term list)


let rec string_of_term t = match t with
    | Var a -> a
    | Fun (f, lst) -> f ^ "(" ^ (String.concat ", " (List.map (string_of_term) lst)) ^ ")";;


let string_of_eq (t1, t2) = string_of_term t1 ^ " = " ^ string_of_term t2;;


let string_of_system s = String.concat "\n" (List.map string_of_eq s);;


let system_to_equation sys =
    let rec get_term_f_names term = match term with
        | Var _ -> []
        | Fun (f, lst) -> f :: List.concat (List.map get_term_f_names lst) in

    let get_eq_f_names (term1, term2) = List.append (get_term_f_names term1) (get_term_f_names term2) in

    let get_sys_f_names () = List.concat (List.map get_eq_f_names sys) in

    let ctr = ref 0 in

    let give_name () = ctr := !ctr + 1; "fun_name" ^  (string_of_int !ctr) in

    let new_f_name () =
        let try_name = ref (give_name ()) in
        let f_names = get_sys_f_names () in
        while (List.mem !try_name f_names) do try_name := give_name () done;
        !try_name in

    let f_name = new_f_name () in
    let rec help lst = match lst with
        | [] -> ([], [])
        | (term1, term2) :: other -> match (help other) with | (l1, l2) -> (term1 :: l1, term2 :: l2) in

    match help sys with
        | (l1, l2) -> (Fun (f_name, l1), Fun (f_name, l2));;


let apply_substitution_with_result subst term =
    let success = ref false in
    let convinient = Hashtbl.create (List.length subst) in

    List.iter (fun (key, value) -> Hashtbl.add convinient key value) subst;

    let rec subst_in_term t = match t with
        | Var v -> if (Hashtbl.mem convinient v) then (success := true; Hashtbl.find convinient v) else Var v
        | Fun (f, lst) -> let res = (List.map subst_in_term lst) in Fun (f, res) in

    (!success, subst_in_term term);;


let apply_substitution subst term =
    match apply_substitution_with_result subst term with (res, t) -> t


let rec zip l1 l2 = match l1, l2 with
    | [], _ -> []
    | _, [] -> []
    | (x::xs), (y::ys) -> (x, y) :: (zip xs ys);;


let rec check_equal eq = match eq with
    | (Var a, Var b) -> a = b
    | (Var _, Fun _) | (Fun _, Var _) -> false
    | (Fun (a, la), Fun (b, lb)) -> if a <> b then false
                                    else if (List.length la <> List.length lb) then false
                                         else List.fold_left (&&) true (List.map check_equal (zip la lb));;

let check_solution subst sys =
    let eq = system_to_equation sys in
    match eq with (t1, t2) -> let substed_eq = (apply_substitution subst t1, apply_substitution subst t2) in

    check_equal substed_eq;;


type action =
    | Ok
    | Delete
    | Incompatible
    | ChangeTo of algebraic_term * algebraic_term
    | Extract
    | Subst of string * algebraic_term
    | Broken


let debug = false;;

exception Not_solved;;

let solve_system sys =
    let trans1 eq = match eq with
        | (Var _, Var _) -> Ok
        | (func, Var v) -> ChangeTo (Var v, func)
        | _ -> Ok in

    let trans2 eq = if (check_equal eq) then Delete
                    else Ok in

    let trans3 eq = match eq with
        | (Fun (a, la), Fun (b, lb)) -> if (a = b && List.length la = List.length lb) then Extract
                                        else ((if debug then print_string("transformation 3\n")); Incompatible)
        | _ -> Ok in

    let rec check_inclusion var term = match term with
        | Var a -> a = var
        | Fun (f, l) -> List.fold_left (||) false (List.map (fun el -> check_inclusion var el) l) in

    let trans4 eq = match eq with
        | (Var v, term) -> if (check_inclusion v term) then ((if debug then print_string("transformation 4\n")); Incompatible)
                           else Subst (v, term)
        | _ -> Ok in

    let operate_eq eq =
        match trans1 eq with
            | ChangeTo (t1, t2) -> ChangeTo (t1, t2)
            | Ok -> (match trans2 eq with
                        | Delete -> Delete
                        | Ok -> (match trans3 eq with
                                    | Incompatible -> Incompatible
                                    | Extract -> Extract
                                    | Ok -> trans4 eq
                                    | _ -> Broken
                                )
                        | _ -> Broken
                    )
            | _ -> Broken in

    let subst_to_que subst que =
        let success = ref false in
        (Queue.of_seq(
            Seq.map
            (fun (t1, t2) -> let rt1 = apply_substitution_with_result [subst] t1 in
                                 let rt2 = apply_substitution_with_result [subst] t2 in
                                     match rt1, rt2 with (res1, term1), (res2, term2) ->
                                        success := res1 || res2;
                                        (term1, term2)
            )
            (Queue.to_seq que)
        ), !success) in

    let operate_sys sys =
        let eqs_without_accident = ref 0 in
        let que = ref (Queue.of_seq (List.to_seq sys)) in

        while (!eqs_without_accident < Queue.length !que && !eqs_without_accident >= 0) do
            if debug then (
                print_string("eqs_without_accident: ");
                print_int(!eqs_without_accident);
                print_string("\nqueue length: ");
                print_int(Queue.length !que);
                print_string("\n");
                print_string(string_of_system(List.of_seq(Queue.to_seq !que)));
                print_string("\n")
                );
            let hd = Queue.pop !que in
                let res = operate_eq hd in
                    match res with
                        | Ok ->                 if debug then print_string("Ok\n\n");
                                                eqs_without_accident := !eqs_without_accident + 1;
                                                (Queue.push hd !que)

                        | Delete ->             if debug then print_string("Delete\n\n");
                                                eqs_without_accident := 0

                        | Incompatible ->       if debug then print_string("Incompatible\n\n");
                                                eqs_without_accident := -1

                        | ChangeTo (t1, t2) ->  if debug then print_string("ChangeTo\n\n");
                                                eqs_without_accident := 0;
                                                (Queue.push (t1, t2) !que)

                        | Extract ->            if debug then print_string("Extract\n\n");
                                                eqs_without_accident := 0;
                                                (match hd with
                                                    | (Fun (a, la), Fun (b, lb)) -> List.iter (fun el -> Queue.push el !que) (zip la lb)
                                                    | _ -> ()
                                                )

                        | Subst (var, t) ->     if debug then print_string("Subst\n\n");
                                                (match (subst_to_que (var, t) !que) with
                                                    | (t_que, success) -> if success then eqs_without_accident := 0
                                                                          else eqs_without_accident := !eqs_without_accident + 1;
                                                                          que := t_que
                                                );
                                                Queue.push (Var var, t) !que

                        | Broken ->             if debug then print_string("Broken\n\n");
                                                eqs_without_accident := -2
        done;

        if debug then (
            print_string("\nqueue length: ");
            print_int(Queue.length !que);
            print_string("\n");
            print_string(string_of_system(List.of_seq(Queue.to_seq !que)));
            print_string("\n")
            );
        if (!eqs_without_accident < 0) then None
        else try Some (List.of_seq (Seq.map (fun el -> match el with | (Var v, term) -> (v, term) | (anything, term) -> raise Not_solved) (Queue.to_seq !que)))
             with e -> if debug then print_string("Not solved :("); None in

    operate_sys sys;;
