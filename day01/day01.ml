open StdLabels

let read_input () =
  let rec read_input' acc =
    try
      let line = read_line () in
      read_input' (line :: acc)
    with End_of_file -> acc |> List.map ~f:int_of_string
  in
  read_input' []

let part1 input =
  input |> List.map ~f:(fun n -> (n / 3) - 2) |> List.fold_left ~init:0 ~f:( + )

let get_all_requirements n =
  let rec get_all_requirements' acc n =
    if n < 1 then acc
    else
      let new_n = (n / 3) - 2 in
      get_all_requirements' (n :: acc) new_n
  in
  get_all_requirements' [] ((n / 3) - 2)

let part2 input =
  input
  |> List.map ~f:get_all_requirements
  |> List.flatten
  |> List.fold_left ~init:0 ~f:( + )

let () =
  let input = read_input () in
  Printf.printf "Part 1: %d\n" (input |> part1);
  Printf.printf "Part 2: %d\n" (input |> part2)
