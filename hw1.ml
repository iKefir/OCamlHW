type peano = Z | S of peano;;
type 'a list = Nil | Cons of ('a * 'a list)
type lambda = Var of string | Abs of string * lambda | App of lambda * lambda;;

let rec peano_of_int x = match x with
    0 -> Z
    | _ -> S (peano_of_int (x - 1));;

let rec int_of_peano p = match p with
    Z -> 0
    | S x -> 1 + int_of_peano x;;

let inc x = S x;;

let dec x = match x with
    Z -> Z
    | S x -> x;;

let rec add x y = match y with
    Z -> x
    | S y -> add (S x) y;;

let rec sub x y = match (x, y) with
    (x, Z) -> x
    | (Z, y) -> Z
    | (S x, S y) -> sub x y;;

let rec mul x y = match (x, y) with
    (x, Z) -> Z
    | (Z, y) -> Z
    | (x, S y) -> add x (mul x y);;

let rec div x y = match ((sub (S x) y), y) with
    (Z, y) -> Z
    | (x, Z) -> failwith "Division by zero"
    | (xx, y) -> inc (div (dec xx) y);;

let rec power x y = match (x, y) with
      (x, Z) -> S Z
    | (Z, y) -> Z
    | (x, S y) -> mul x (power x y);;

let rec append x y = match x with
    Nil -> y
    | Cons (head, tail) -> Cons (head, (append tail y));;
(*
let rec rev x = match x with
    Nil -> Nil
    | Cons (head, tail) -> match Cons (head, tail) with
*)

let rev x = failwith "Not implemented";;
let merge_sort x = failwith "Not implemented";;
                     
let string_of_lambda x = failwith "Not implemented";;
let lambda_of_string x = failwith "Not implemented";;