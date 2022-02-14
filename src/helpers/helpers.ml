let rec seq_iter_cps f xs cont () =
  match xs () with
  | Seq.Nil         -> cont ()
  | Seq.Cons(x, xs) ->
    f x (fun () -> seq_iter_cps f xs cont ())

let rec list_fold_left_cps f acc xs cont =
  match xs with
  | []    -> cont(acc)
  | x::xs -> f acc x (fun res -> list_fold_left_cps f res xs cont)
