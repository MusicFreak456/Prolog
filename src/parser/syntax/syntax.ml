module Term = struct
  type t = 
    | Variable of string
    | Symbol of string * t list

  let return variable = Variable variable

  let rec bind term cont =
    match term with
    | Variable x -> cont x
    | Symbol(s, args) -> Symbol(s, List.map (fun t -> bind t cont) args)

  let list_sugar term =
    let rec list_to_string_acc acc term =
    match term with
    | Symbol("nil", _) -> List.rev acc
    | Symbol("cons", [x; tail]) -> 
      list_to_string_acc (x::acc) tail
    | _ -> assert false in
    list_to_string_acc [] term

  let rec to_string term =
    match term with
    | Variable name -> name
    | Symbol("cons",_) as list -> 
      let list = list_sugar list in
      "[" ^ String.concat "," (List.map to_string list) ^ "]"
    | Symbol(s, []) -> s
    | Symbol(s, args) -> s ^ 
      "(" ^ String.concat "," (List.map to_string args) ^ ")"

  let rec get_vars term =
    match term with
    | Variable var -> [var]
    | Symbol(_, args) ->
      List.concat_map get_vars args
end
  
type query = 
  | DbQuery of Term.t list
  | DbOpen  of string
type clause = Term.t * Term.t list
type program = clause list
