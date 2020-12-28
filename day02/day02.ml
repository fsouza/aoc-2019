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

let restore_gravity ?(noun = 12) ?(verb = 2) input =
  input |> Mem.add ~key:1 ~data:noun |> Mem.add ~key:2 ~data:verb

let part1 input = input |> restore_gravity |> Fun.flip exec 0 |> Mem.find 0

let part2 target input =
  let min_value = 0 in
  let max_value = 99 in
  let rec find_nound_and_verb noun verb =
    let program_output =
      input |> restore_gravity ~noun ~verb |> Fun.flip exec 0 |> Mem.find 0
    in
    if program_output = target then (noun, verb)
    else if verb < max_value then find_nound_and_verb noun (verb + 1)
    else if noun < max_value then find_nound_and_verb (noun + 1) min_value
    else failwith "can't find the noun and verb!"
  in
  find_nound_and_verb 0 0 |> fun (noun, verb) -> (100 * noun) + verb

let () =
  let input = read_input () in
  input |> part1 |> Printf.printf "Part 1: %d\n";
  input |> part2 19690720 |> Printf.printf "Part 2: %d\n"
