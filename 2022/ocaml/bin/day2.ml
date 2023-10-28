open Base

(* Part 1 *)
type plays = Rock | Paper | Scissors | None

let parse_play chr =
  match chr with
  | "A" | "X" -> Rock
  | "B" | "Y" -> Paper
  | "C" | "Z" -> Scissors
  | _ -> None

let play_score play =
  match play with Rock -> 1 | Paper -> 2 | Scissors -> 3 | None -> 0

let game_score theirs ours =
  match (theirs, ours) with
  | Rock, Paper | Paper, Scissors | Scissors, Rock -> 6
  | t, o when t = o -> 3
  | _ -> 0

let () =
  Advent.read_lines "../data/day2-example-input.txt" |> Advent.print_listof_strs
