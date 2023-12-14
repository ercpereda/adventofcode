open Advent__Day12

let () =
  Stdio.print_endline "=== Part 1 ===";
  Stdio.print_string "Demo: ";
  Advent.read_lines "../data/day12-example-input.txt" |> part1_answer;
  Stdio.print_string "Answer: ";
  Advent.read_lines "../data/day12-input.txt" |> part1_answer

let () =
  Stdio.print_endline "=== Part 2 ===";
  Stdio.print_string "Demo: ";
  Advent.read_lines "../data/day12-example-input.txt" |> part2_answer;
  Stdio.print_string "Answer: ";
  Advent.read_lines "../data/day12-input.txt" |> part2_answer
