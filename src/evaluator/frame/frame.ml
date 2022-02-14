module FrameMap = Map.Make(String)

type t = Syntax.Term.t FrameMap.t

let substitute variable value frame = 
  FrameMap.add variable value frame

let empty = FrameMap.empty

let is_empty frame = FrameMap.is_empty frame

let get_substitution variable frame = 
  FrameMap.find_opt variable frame

let (let*) = Syntax.Term.bind
let return = Syntax.Term.return

let rec apply frame term = 
  let* variable = term in
  match get_substitution variable frame with
  | Some substitution ->
    apply frame substitution
  | None -> return variable
