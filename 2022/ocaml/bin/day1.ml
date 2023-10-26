open Base

let group_calories calories =
  let rec group input result =
    match input with
    | [] -> result
    | "" :: rest -> group rest (0 :: result)
    | cals :: rest ->
        let new_result =
          match result with
          | [] -> [ Int.of_string cals ]
          | hd :: tail -> (hd + Int.of_string cals) :: tail
        in
        group rest new_result
  in
  group calories []

(* Part 1*)
let max_of_list input =
  let rec _max_of_list input cur =
    match input with [] -> cur | hd :: tl -> _max_of_list tl (max hd cur)
  in
  _max_of_list input 0

let part1_answer input =
  input |> group_calories |> max_of_list |> Int.to_string |> Stdio.print_endline

let () =
  Stdio.print_endline "=== Part 1 ===";
  Stdio.print_string "Demo: ";
  Advent.read_lines "../data/day1-example-input.txt" |> part1_answer;
  Stdio.print_string "Answer: ";
  Advent.read_lines "../data/day1-input.txt" |> part1_answer

(* Part 2 *)
let max_3_of_list input =
  let rec _max_3_of_list input (m1, m2, m3) =
    match input with
    | [] -> (m1, m2, m3)
    | hd :: tl ->
        let new_maxs =
          match (m1, m2, m3) with
          | m1, m2, _ when hd > m1 -> (hd, m1, m2)
          | m1, m2, _ when hd > m2 -> (m1, hd, m2)
          | _, m2, m3 when hd > m3 -> (m1, m2, hd)
          | _ -> (m1, m2, m3)
        in
        _max_3_of_list tl new_maxs
  in
  _max_3_of_list input (0, 0, 0)

let part2_answer input =
  let m1, m2, m3 = input |> group_calories |> max_3_of_list in
  m1 + m2 + m3 |> Int.to_string |> Stdio.print_endline

let () =
  Stdio.print_endline "=== Part 2 ===";
  Stdio.print_string "Demo: ";
  Advent.read_lines "../data/day1-example-input.txt" |> part2_answer;
  Stdio.print_string "Answer: ";
  Advent.read_lines "../data/day1-input.txt" |> part2_answer
