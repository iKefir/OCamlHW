type peano = Z | S of peano;;
type lambda = Var of string | Abs of string * lambda | App of lambda * lambda;;

let rec peano_of_int x = match x with
    | 0 -> Z
    | _ -> S (peano_of_int (x - 1));;

let rec int_of_peano p = match p with
    | Z -> 0
    | S x -> 1 + int_of_peano x;;

let inc x = S x;;

let dec x = match x with
    | Z -> Z
    | S x -> x;;

let rec add x y = match y with
    | Z -> x
    | S y -> add (S x) y;;

let rec sub x y = match (x, y) with
    | (x, Z) -> x
    | (Z, y) -> Z
    | (S x, S y) -> sub x y;;

let rec mul x y = match (x, y) with
    | (x, Z) -> Z
    | (Z, y) -> Z
    | (x, S y) -> add x (mul x y);;

let rec div x y = match ((sub (S x) y), y) with
    | (Z, y) -> Z
    | (x, Z) -> failwith "Division by zero"
    | (xx, y) -> inc (div (dec xx) y);;

let rec power x y = match (x, y) with
    | (x, Z) -> S Z
    | (Z, y) -> Z
    | (x, S y) -> mul x (power x y);;

let rec append x y = match x with
    | [] -> y
    | head :: tail -> head :: (append tail y);;

let rec print_list = function
    | [] -> ()
    | head::tail -> print_int head ; print_string " " ; print_list tail;;

let rev x =
    let rec reverse acc list = match list with
        | [] -> acc
        | head :: tail -> reverse (head :: acc) tail in
    reverse [] x;;

let split x = 
    let rec split_def x y z = match x with
        | [] -> (y, z)
        | head :: tail -> (split_def (tail) (z) (head::y)) in
    split_def x [] [];;

let rec merge_sort x = match x with
    | head :: [] -> x
    | _ -> let rec merge a b = match (a, b) with
        | ([], []) -> []
        | ([], bhead :: btail) -> b
        | (ahead :: atail, []) -> a
        | (ahead :: atail, bhead :: btail) -> match ((<) ahead bhead) with
            | true -> ahead :: (merge atail b)
            | false -> bhead :: (merge a btail) in
                let (alist, blist) = split x in
        merge (merge_sort alist) (merge_sort blist);;

let rec debug_string_of_lambda x = match x with
    | Var a -> "Var " ^ a
    | App (a, b) -> "App (" ^ (debug_string_of_lambda a) ^ " " ^ (debug_string_of_lambda b) ^ ")"
    | Abs (a, b) -> "Abs (\\" ^ a ^ ".(" ^ (debug_string_of_lambda b) ^ "))";;
                     
let rec string_of_lambda x = match x with
    | Var a -> a
    | App (a, b) -> "(" ^ (string_of_lambda a) ^ " " ^ (string_of_lambda b) ^ ")"
    | Abs (a, b) -> "(\\" ^ a ^ "." ^ (string_of_lambda b) ^ ")";;


let lambda_of_string s = 
    let s = s ^ ";" in
    let pos = ref 0 in
    let get() = s.[!pos] in
    let next () = if !pos < String.length s - 1 then pos := !pos + 1 in
    let eat x = if get() <> x then failwith "Exception" else next() in

    let rec parse_string_help acc = match get() with
            | ' ' -> acc
            | '.' -> acc
            | '(' -> acc
            | ')' -> acc
            | ';' -> acc
            | _ -> let cur_char = get() in next(); 
                    parse_string_help (acc ^ (String.make (1) (cur_char))) in 

    let parse_string() = parse_string_help ("") in

    let parse_ident() =
        let l = (parse_string()) in
        Var (l) in

    let rec parse_abs() = 
        eat '\\';
        let v = parse_string() in eat '.';
        let l = parse_lambda() in 
        Abs (v, l)

        and big_parse res = 
        if ((!pos = String.length s - 1) || get() = ')')
            then res
            else big_parse_sec res

        and big_parse_sec res = 
            next();
            big_parse (App (res, parse_lambda()))

        and parse_lambda() = match (get ()) with
        | '\\' -> big_parse (parse_abs())
        | '(' ->
            eat '(';
            let s = parse_lambda () in
            eat ')';
            big_parse s
        | _ -> big_parse (parse_ident()) in

    parse_lambda();;

(* a instead str in b *)
let rec subst a b str = match b with
    | Var x -> if (x = str) then a else Var x
    | App (x, y) -> App (subst a x str, subst a y str)
    | Abs (x, lambda) -> Abs (x, subst a lambda str)

(* a instead str in b *)
let rec free_subst a b str = 

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

let rec alpha_eq a b = 
    let ctr = ref 0 in
    let next() = ctr := !ctr + 1 in
    let get_subst_arg() = Var ("subst_arg" ^ (string_of_int !ctr)) in

    let rec alpha_eq_help a b = match (a, b) with
    | (Var ax, Var bx) -> ax = bx
    | (App (a1, a2), App (b1, b2)) -> ((alpha_eq_help a1 b1) && (alpha_eq_help a2 b2))
    | (Abs (ax, alambda), Abs (bx, blambda)) -> 
        if ((free_subst (get_subst_arg()) alambda ax) && (free_subst (get_subst_arg()) blambda bx)) then
            (alpha_eq_help (subst (get_subst_arg()) alambda ax) (subst (get_subst_arg()) blambda bx)) 
        else    (next();
                alpha_eq_help a b)
    | _ -> false

in alpha_eq_help a b;;





















