type peano = Z | S of peano

val peano_of_int: int -> peano
val int_of_peano: peano -> int

val inc: peano -> peano
val dec: peano -> peano
val add: peano -> peano -> peano
val sub: peano -> peano -> peano
val mul: peano -> peano -> peano
val div: peano -> peano -> peano
val power: peano -> peano -> peano

val append: 'a list -> 'a list -> 'a list
val print_int_list: int list -> unit
val print_string_list: string list -> unit

val rev: 'a list -> 'a list
val merge_sort: 'a list -> 'a list
val split: 'a list -> 'a list * 'a list

type lambda = Var of string | Abs of string * lambda | App of lambda * lambda

val string_of_lambda: lambda -> string
val debug_string_of_lambda: lambda -> string
val lambda_of_string: string -> lambda
