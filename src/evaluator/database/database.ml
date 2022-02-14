open Syntax

type t = ((string * int), Syntax.clause list) Hashtbl.t

let init () = Hashtbl.create 20

let add database clause =
  match fst clause with
  | Term.Symbol(s, args) ->
    let len = List.length args in
    let key = (s, len) in
    begin match Hashtbl.find_opt database key with
    | None   -> Hashtbl.add database key [clause]
    | Some l -> Hashtbl.replace database key (clause :: l)
    end
  | Variable _ -> assert false

let get database term =
  match term with
  | Term.Symbol(s, args) ->
    let len = List.length args in
    let key = (s, len) in
    begin match Hashtbl.find_opt database key with
    | Some l -> l
    | None -> []
    end
  | Term.Variable _ -> assert false
