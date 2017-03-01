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
                     
let rec string_of_lambda x = match x with
    | Var a -> a
    | App (a, b) -> (string_of_lambda a) ^ " " ^ (string_of_lambda b)
    | Abs (a, b) -> "\\" ^ a ^ "." ^ (string_of_lambda b);;


let lambda_of_string s = 
    let s = s ^ ";" in
    let pos = ref 0 in
    let get() = s.[!pos] in
    let next () = if !pos < String.length s - 1 then pos := !pos + 1 in
    let eat x = if get() <> x then failwith "Exception" else next() in

    let parse_string() =
        String.make (1) (get()) in

    let parse_ident() =
        let l = (parse_string()) in
        Var (l) in

    let rec parse_lambda() = match (get ()) with
        | '\\' -> 
        let rec big_parse res = 
        if ((!pos = String.length s - 1))
            then res
            else App (res, big_parse (parse_lambda())) in
            big_parse (
                                let parse_abs() = 
                                eat '\\';
                                let v = parse_string() in eat '.';
                                let l = parse_lambda() in 
                                Abs (v, l) in
                                parse_abs()
                            )
        | '(' ->
            eat '(';
            let smres = parse_lambda () in
            eat ')';
            let rec big_parse res = 
                if ((!pos = String.length s - 1))
                then res
                else App (res, big_parse (parse_lambda()))
            in big_parse smres
        | _ -> let rec big_parse res = 
                if ((!pos = String.length s - 1))
                then res
                else App (res, big_parse (parse_lambda()))
            in big_parse(parse_ident()) in

    parse_lambda();;





















