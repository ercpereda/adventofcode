open Base
(* let s = String.to_list "hello world" |> List.fold ~init:(Set.empty (module Char)) ~f:(Set.add);; *)
(* val take : 'a t -> int -> 'a t *)
(* val drop : 'a t -> int -> 'a t *)

module Rucksack = struct
  type t = {
    first_comparment : (char, Char.comparator_witness) Set.t;
    second_comparment : (char, Char.comparator_witness) Set.t;
  }

  let of_string s =
    let len = String.length s in
    let middle = len / 2 in
    {
      first_comparment =
        String.prefix s middle |> String.to_list
        |> List.fold ~init:(Set.empty (module Char)) ~f:Set.add;
      second_comparment =
        String.suffix s middle |> String.to_list
        |> List.fold ~init:(Set.empty (module Char)) ~f:Set.add;
    }

  let common_item rucksack =
    let inter_set =
      Set.inter rucksack.first_comparment rucksack.second_comparment
    in
    if Set.length inter_set <> 1 then
      failwith "The amount of common items isn't one"
    else
      match Set.nth inter_set 0 with
      | None -> failwith "There should be one common item"
      | Some a -> a
end

let priority c =
  let open Stdlib.Char in
  if compare c 'z' >= -25 then code c - (code 'a' - 1)
  else code c - (code 'A' - 1) + 26

module Rucksacks = struct
  type t = Rucksack.t list

  let of_list l : t = List.map l ~f:Rucksack.of_string
  (* let common_items (rucksacks : t) = List.map rucksacks ~f:Rucksack.common_item *)

  let priority (rucksacks : t) =
    List.map rucksacks ~f:Rucksack.common_item
    |> List.map ~f:priority
    |> List.fold_left ~f:( + ) ~init:0
end

let part1_answer input =
  input |> Rucksacks.of_list |> Rucksacks.priority |> Int.to_string
  |> Stdio.print_endline
(* |> Advent.print_listof_chars *)
(* |> Advent.print_listof_strs *)

let () =
  Stdio.print_endline "=== Part 1 ===";
  Stdio.print_string "Demo: ";
  Advent.read_lines "../data/day3-example-input.txt" |> part1_answer;
  Stdio.print_string "Answer: ";
  Advent.read_lines "../data/day3-input.txt" |> part1_answer

let badget_item l1 l2 l3 =
  let s1 =
    String.to_list l1 |> List.fold ~init:(Set.empty (module Char)) ~f:Set.add
  in
  let s2 =
    String.to_list l2 |> List.fold ~init:(Set.empty (module Char)) ~f:Set.add
  in
  let s3 =
    String.to_list l3 |> List.fold ~init:(Set.empty (module Char)) ~f:Set.add
  in
  let inter_set = s1 |> Set.inter s2 |> Set.inter s3 in
  if Set.length inter_set <> 1 then
    failwith "The amount of common items isn't one"
  else
    match Set.nth inter_set 0 with
    | None -> failwith "There should be one common item"
    | Some a -> a

let badget_priorities l =
  let rec _badget_priorities l result =
    match l with
    | [] -> result
    | l1 :: l2 :: l3 :: tl ->
        _badget_priorities tl (badget_item l1 l2 l3 :: result)
    | _ -> failwith "Invalid input isn't divisible by 3"
  in
  _badget_priorities l [] |> List.map ~f:priority

let part2_answer input =
  input |> badget_priorities
  |> List.fold_left ~f:( + ) ~init:0
  |> Int.to_string |> Stdio.print_endline

let () =
  Stdio.print_endline "=== Part 2 ===";
  Stdio.print_string "Demo: ";
  Advent.read_lines "../data/day3-example-input.txt" |> part2_answer;
  Stdio.print_string "Answer: ";
  Advent.read_lines "../data/day3-input.txt" |> part2_answer
