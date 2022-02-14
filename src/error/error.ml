type runtime_error =
| UnboundPredicate of Syntax.Term.t

type error =
| ParsingError   of Lexing.position * string
| CannotOpenFile of string * string
| RuntimeError   of runtime_error

exception PrologException of error

let throw error =
  raise (PrologException error)

let string_of_runtime_error runtime_error =
  match runtime_error with
  | UnboundPredicate term ->
    begin match term with
    | Syntax.Term.Symbol(s,args) ->
      let arg_len = List.length args in
      "Unknown predicate: " ^ s ^ "/" ^ string_of_int arg_len ^ "\n"
    | Syntax.Term.Variable _ -> assert false
    end

let string_of_error error = 
  match error with
  | ParsingError(position, msg) -> 
    let line = position.pos_lnum |> string_of_int in
    let column = position.pos_cnum - position.pos_bol |> string_of_int in
    "Parsing error at " ^ line ^ ":" ^ column  ^ " : " ^ msg
  | CannotOpenFile (filename, msg) ->
    "Cannot open file \"" ^ filename ^ "\": " ^ msg ^ "\n"
  | RuntimeError runtime_error -> string_of_runtime_error runtime_error
