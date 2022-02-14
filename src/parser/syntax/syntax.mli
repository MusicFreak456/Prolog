module Term : sig
  type t = 
    | Variable of string
    | Symbol of string * t list

  val return: string -> t
  val bind: t -> (string -> t) -> t
  val to_string: t -> string
  val get_vars: t -> string list
end

type query = 
  | DbQuery of Term.t list
  | DbOpen  of string
type clause = Term.t * Term.t list
type program = clause list
