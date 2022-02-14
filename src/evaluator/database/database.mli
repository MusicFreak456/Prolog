type t

val init: unit -> t
val add: t -> Syntax.clause -> unit
val get: t -> Syntax.Term.t -> Syntax.clause list
