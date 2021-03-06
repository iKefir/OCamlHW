type algebraic_term = Var of string | Fun of string * (algebraic_term list)

val string_of_term: algebraic_term -> string

val string_of_eq: (algebraic_term * algebraic_term) -> string

val string_of_system: (algebraic_term * algebraic_term) list -> string

val check_equal: (algebraic_term * algebraic_term) -> bool

(* По списку уравнений вернуть одно уравнение *)
val system_to_equation: (algebraic_term * algebraic_term) list -> (algebraic_term * algebraic_term)

(* Применить подстановку к терму *)
val apply_substitution: (string * algebraic_term) list -> algebraic_term -> algebraic_term

(* Проверить решение *)
val check_solution: (string * algebraic_term) list -> (algebraic_term * algebraic_term) list -> bool

(* Решить систему; если решения нет -- вернуть None *)
val solve_system: (algebraic_term * algebraic_term) list -> (string * algebraic_term) list option
