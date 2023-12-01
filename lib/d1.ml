let is_digit c =
  '0' <= c && c <= '9'

let to_int c =
  Char.code c - Char.code '0'

let get_number line =
  let f, l =
    String.fold_left (fun (f, l) c ->
      if is_digit c
      then
        let d = to_int c in
        match f with
        | None   -> (Some d, d)
        | Some _ -> (     f, d)
      else
        (f, l)
    ) (None, 0) line
  in
  match f with
  | Some f -> f * 10 + l
  | None   -> assert false

let solve (input : string list) =
  let res =
    List.fold_left (fun acc line ->
      acc + get_number line
    ) 0 input
  in
  Printf.printf "%d\n" res

(* let solve_star (input : string list) = *)

