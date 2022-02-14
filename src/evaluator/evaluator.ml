let fresh_variable =
  let counter = ref 0 in
  fun () -> 
    incr counter;
    !counter |> string_of_int

let refresh_clause clause =
  let (let*) = Syntax.Term.bind in
  let return  = Syntax.Term.return in
  let variable_map = Hashtbl.create 20 in
  let refresh_term term = 
    let* variable = term in
    begin match Hashtbl.find_opt variable_map variable with
    | None ->
      let new_name = fresh_variable () in
      Hashtbl.add variable_map variable new_name;
      return new_name
    | Some name -> return name
    end in
  let refreshed_head = refresh_term (fst clause) in
  let refreshed_body = List.map refresh_term (snd clause) in
  (refreshed_head, refreshed_body)

let database = Database.init ()

let rec resolve_goal_in_context goal frame =
  let candidates = Database.get database goal in
  if List.length candidates = 0 
  then Error.throw (Error.RuntimeError (Error.UnboundPredicate goal))
  else Seq.flat_map (fun clause () ->
    let refreshed_clause = refresh_clause clause in
    let clause_head = fst refreshed_clause in
    let unifier = Unifier.unify clause_head goal frame in
    match unifier with
    | None -> Seq.Nil
    | Some frame ->
      let clause_body = snd refreshed_clause in
      resolve_query clause_body (Seq.return frame) ()
  ) (candidates |> List.to_seq)

and resolve_goal goal stream_in () =
  match stream_in () with
  | Seq.Nil -> Seq.Nil
  | Seq.Cons(frame, stream_in) ->
    Seq.append 
      (resolve_goal_in_context goal frame) 
      (resolve_goal goal stream_in) ()
    
and resolve_query query start_stream = 
  List.fold_left 
  (fun stream goal -> resolve_goal goal stream)
  start_stream query

let eval_query query =
  match query with
  | Syntax.DbOpen filename ->
    let program = Prolog_parser.parse_program_from_file filename in
    List.iter (fun clause -> Database.add database clause) program;
    Seq.return Frame.empty
  | Syntax.DbQuery query -> resolve_query query (Seq.return Frame.empty)
