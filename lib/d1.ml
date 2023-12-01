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

(* --------------------------------------------------------------------- Star *)

let number = [
    ("one", 1); ("two", 2); ("three", 3); ("four", 4); ("five", 5);
    ("six", 6); ("seven", 7); ("eight", 8); ("nine", 9)
  ]

let get_number line =
  let _, f, l =
    String.fold_left (fun (i, f, l) c ->
      let d = 
        if is_digit c
        then
          Some (to_int c)
        else
          match List.find_opt (fun (n, _) ->
            try
              let s = String.sub line i (String.length n) in
              String.equal s n
            with _ -> false
          ) number with
          | None -> None
          | Some (_, d) -> Some d
        in
        match d, f with
        | None, _        -> (i + 1,      f, l)
        | Some d, None   -> (i + 1, Some d, d)
        | Some d, Some _ -> (i + 1,      f, d)
    ) (0, None, 0) line
  in
  match f with
  | Some f -> f * 10 + l
  | None   -> assert false

let solve_star (input : string list) =
  let res =
    List.fold_left (fun acc line ->
      acc + get_number line
    ) 0 input
  in
  Printf.printf "%d\n" res

