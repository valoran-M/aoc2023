let read_file file =
  let ic = open_in file in
  let rec aux l =
    match input_line ic with
    | line                  -> aux (line :: l)
    | exception End_of_file -> close_in ic; List.rev l
  in
  aux []

