open Syntax.Term

let rec occurs_in term variable frame = 
  match term with
  | Variable some_variable -> 
    begin match Frame.get_substitution some_variable frame with
    | None -> String.equal some_variable variable
    | Some substitution -> occurs_in substitution variable frame
    end
  | Symbol(_, arguments) ->
    Helpers.list_fold_left_cps 
      (fun acc argument cont -> 
        if occurs_in argument variable frame
          then true 
          else cont(acc))
      false arguments Fun.id

let rec maybe_add variable value frame =
  match Frame.get_substitution variable frame with
  | Some substitution -> unify substitution value frame
  | None ->
    begin match value with
    | Variable some_variable ->
      begin match Frame.get_substitution some_variable frame with
      | None -> Some (Frame.substitute variable value frame)
      | Some substitution -> maybe_add variable substitution frame
      end
    | Symbol(_,_) ->
      if occurs_in value variable frame then None
      else Some (Frame.substitute variable value frame)
    end

and unify t1 t2 frame =
  if t1 = t2 then Some frame else

  match t1, t2 with
  | Variable var, t | t, Variable var ->
    maybe_add var t frame
    
  | Symbol(s, a), Symbol(s', a') when
    not @@ String.equal s s' || List.length a <> List.length a' -> None

  | Symbol(_, a), Symbol(_, a') ->
    let ziped_args = List.combine a a' in
    Helpers.list_fold_left_cps maybe_unify (Some frame) ziped_args Fun.id

and maybe_unify frame_opt (t, t') cont =
  match frame_opt with
  | None -> None
  | Some frame -> cont @@ unify t t' frame
