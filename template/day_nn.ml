open StdLabels

type input = string list

let read_input () =
  let rec read_input' acc =
    try
      let line = read_line () in
      read_input' (line :: acc)
    with End_of_file -> acc |> List.rev
  in
  read_input' []
