let rec user_input prompt action =
  match LNoise.linenoise prompt with
  | None -> ()
  | Some input -> 
    action input; 
    user_input prompt action

let vars_to_string vars frame =
  String.concat "\r\n" (List.map (fun variable ->
    match Frame.get_substitution variable frame with
    | None -> variable ^ " is free"
    | Some substitution ->
      variable ^ ": " ^ 
      (substitution |> Frame.apply frame |> Syntax.Term.to_string)
  ) vars)

let print_results stream query =
  match query with
  | Syntax.DbQuery query ->
    let vars = List.concat_map Syntax.Term.get_vars query 
      |> List.sort_uniq String.compare in
    Helpers.seq_iter_cps (fun frame cont ->
    if Frame.is_empty frame || vars = [] 
    then print_endline "true." 
    else begin
      print_newline();
      print_string @@ vars_to_string vars frame;
      let input = read_line () in
      match input with
      | ";" -> cont ()
      | "." -> ()
      | _ -> () 
    end
    ) stream (fun _ -> print_endline "\nfalse.") ()
  | Syntax.DbOpen _ -> print_endline "true."

let print_error error =
  print_string "\x1b[1;31m";
  print_string @@ Error.string_of_error error;
  print_endline "\x1b[0m"

let history_filename = ".prolog_history"

let main_loop () = 
  user_input "prolog > " (fun input ->
  try
    LNoise.history_add input |> ignore;
    LNoise.history_save ~filename:history_filename |> ignore;
    let query = Prolog_parser.parse_query_from_string input in
    let results = Evaluator.eval_query query in
    print_results results query
  with Error.PrologException error -> print_error error)

let _ = 
  LNoise.history_load ~filename:history_filename |> ignore;
  LNoise.history_set  ~max_length:100 |> ignore;
  ["\n\x1b[38;5;87m" ^ "Welcome to Prolog!" ^ "\x1b[0m\n"; 
   "Type \"[<filepath>].\" to load a program";
   "or type a query starting with \"?-\"";
   "or press ctrl+d to exit.\n"] 
  |> List.iter print_endline;
  main_loop ()
