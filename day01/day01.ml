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

let () = read_input () |> part1 |> Printf.printf "%d\n"
