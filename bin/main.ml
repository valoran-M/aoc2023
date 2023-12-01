

let () =
  let file  = Sys.argv.(1) in
  let lines = Aoc2023.Utils.read_file file in
  Aoc2023.D1.solve_star lines
