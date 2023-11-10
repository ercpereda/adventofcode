open Base

module Pair = struct
  type t = int * int

  let make a b =
    if a > b then failwith "Invalid pair, a must be (a <= b)" else (a, b)

  let of_string s : t =
    match s |> String.split ~on:'-' |> List.map ~f:Int.of_string with
    | a :: b :: _ -> make a b
    | _ -> failwith "Invalid tuple string format"

  let fully_contains p1 p2 = fst p1 >= fst p2 && snd p1 <= snd p2
  let overlap p1 p2 = fst p1 <= fst p2 && snd p1 >= fst p2
  let to_string p = Int.to_string (fst p) ^ "-" ^ Int.to_string (snd p)
end

module Pair_of_Pairs = struct
  type t = Pair.t * Pair.t

  let of_string s : t =
    match s |> String.split ~on:',' |> List.map ~f:Pair.of_string with
    | a :: b :: _ -> (a, b)
    | _ -> failwith "Invalid pair of pairs format"

  let fully_contains p =
    Pair.fully_contains (fst p) (snd p) || Pair.fully_contains (snd p) (fst p)

  let overlap p = Pair.overlap (fst p) (snd p) || Pair.overlap (snd p) (fst p)

  [@@@ocaml.warning "-32"]

  let to_string p = Pair.to_string (fst p) ^ "," ^ Pair.to_string (snd p)
end

let part1_answer input =
  input
  |> List.map ~f:Pair_of_Pairs.of_string
  |> List.filter ~f:Pair_of_Pairs.fully_contains
  |> List.length |> Int.to_string |> Stdio.print_endline

let () =
  Stdio.print_endline "=== Part 1 ===";
  Stdio.print_string "Demo: ";
  Advent.read_lines "../data/day4-example-input.txt" |> part1_answer;
  Stdio.print_string "Answer: ";
  Advent.read_lines "../data/day4-input.txt" |> part1_answer

let part2_answer input =
  input
  |> List.map ~f:Pair_of_Pairs.of_string
  |> List.filter ~f:Pair_of_Pairs.overlap
  (* |> List.map ~f:Pair_of_Pairs.to_string *)
  (* |> Advent.print_listof_strs *)
  |> List.length
  |> Int.to_string |> Stdio.print_endline

let () =
  Stdio.print_endline "=== Part 2 ===";
  Stdio.print_string "Demo: ";
  Advent.read_lines "../data/day4-example-input.txt" |> part2_answer;
  Stdio.print_string "Answer: ";
  Advent.read_lines "../data/day4-input.txt" |> part2_answer
