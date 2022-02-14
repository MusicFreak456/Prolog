type t

val substitute: string -> Syntax.Term.t -> t -> t

val empty: t
val is_empty: t -> bool

val get_substitution: string -> t -> Syntax.Term.t option
val apply: t -> Syntax.Term.t -> Syntax.Term.t
