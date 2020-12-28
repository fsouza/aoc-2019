open StdLabels
open MoreLabels
module Mem = Map.Make (Int)

let read_input () =
  let rec read_input' acc =
    try
      let line = read_line () in
      read_input' (line :: acc)
    with End_of_file ->
      acc
      |> List.rev
      |> List.hd
      |> String.split_on_char ~sep:','
      |> List.mapi ~f:(fun i v -> (i, int_of_string v))
      |> List.fold_left ~init:Mem.empty ~f:(fun m (i, v) ->
             Mem.add ~key:i ~data:v m)
  in
  read_input' []

let rec exec m pos =
  let opcode = Mem.find pos m in
  if opcode = 1 then run m pos ( + )
  else if opcode = 2 then run m pos ( * )
  else if opcode = 99 then m
  else opcode |> Printf.sprintf "invalid opcode %d" |> failwith

and run m pos operation =
  let x = Mem.find (pos + 1) m |> Fun.flip Mem.find m in
  let y = Mem.find (pos + 2) m |> Fun.flip Mem.find m in
  let target_addr = Mem.find (pos + 3) m in
  exec (Mem.add ~key:target_addr ~data:(operation x y) m) (pos + 4)

let restore_gravity input =
  input |> Mem.add ~key:1 ~data:12 |> Mem.add ~key:2 ~data:2

let part1 input = input |> restore_gravity |> Fun.flip exec 0 |> Mem.find 0

let () = read_input () |> part1 |> Printf.printf "%d\n"
