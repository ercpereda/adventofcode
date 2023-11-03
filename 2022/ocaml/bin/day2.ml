open Base

module RPS_Play = struct
  type t = Rock | Paper | Scissors

  exception Invalid_Symbol

  let of_string = function
    | "A" | "X" -> Rock
    | "B" | "Y" -> Paper
    | "C" | "Z" -> Scissors
    | _ -> raise Invalid_Symbol

  let score = function Rock -> 1 | Paper -> 2 | Scissors -> 3
end

module RPS_Outcome = struct
  type t = Win | Loss | Draw

  exception Invalid_Outcome

  let of_string = function
    | "X" -> Loss
    | "Y" -> Draw
    | "Z" -> Win
    | _ -> raise Invalid_Outcome

  let score = function Win -> 6 | Draw -> 3 | Loss -> 0

  let play_to_outcome their outcome =
    let open RPS_Play in
    match (their, outcome) with
    | Rock, Loss -> Scissors
    | Rock, Draw -> Rock
    | Rock, Win -> Paper
    | Paper, Loss -> Rock
    | Paper, Draw -> Paper
    | Paper, Win -> Scissors
    | Scissors, Loss -> Paper
    | Scissors, Draw -> Scissors
    | Scissors, Win -> Rock

  let outcome round =
    let open RPS_Play in
    match round with
    | Scissors, Rock | Paper, Scissors | Rock, Paper -> Win
    (* TODO: Investigate how to compare without using Poly module *)
    | a, b when Poly.(a = b) -> Draw
    | _ -> Loss
end

module RPS_Round = struct
  (* TODO: Use a record insted of a tuple *)
  type t = RPS_Play.t * RPS_Play.t (* Their * Ours *)

  exception Invalid_Round

  let of_string s =
    match String.split ~on:' ' s with
    | ours :: their :: _ -> (RPS_Play.of_string ours, RPS_Play.of_string their)
    | _ -> raise Invalid_Round

  let of_outcome_string s =
    match String.split ~on:' ' s with
    | their :: outcome :: _ ->
        let their_play = RPS_Play.of_string their in
        ( their_play,
          RPS_Outcome.of_string outcome
          |> RPS_Outcome.play_to_outcome their_play )
    | _ -> raise Invalid_Round

  let outcome_score (round : t) = RPS_Outcome.outcome round |> RPS_Outcome.score

  let score round =
    let _, ours = round in
    RPS_Play.score ours + outcome_score round
end

module RPS_Game = struct
  type t = RPS_Round.t list

  let make lines =
    let rec _make lines result =
      match lines with
      | [] -> result
      | line :: rest -> _make rest (RPS_Round.of_string line :: result)
    in
    _make lines []

  let of_outcome_lines lines =
    let rec _of_outcome_lines lines result =
      match lines with
      | [] -> result
      | line :: rest ->
          _of_outcome_lines rest (RPS_Round.of_outcome_string line :: result)
    in
    _of_outcome_lines lines []

  let score (game : t) =
    let rec _score game total =
      match game with
      | [] -> total
      | round :: rest -> _score rest (total + RPS_Round.score round)
    in
    _score game 0
end

let part1_answer input =
  input |> RPS_Game.make |> RPS_Game.score |> Int.to_string
  |> Stdio.print_endline

let () =
  Stdio.print_endline "=== Part 1 ===";
  Stdio.print_string "Demo: ";
  Advent.read_lines "../data/day2-example-input.txt" |> part1_answer;
  Stdio.print_string "Answer: ";
  Advent.read_lines "../data/day2-input.txt" |> part1_answer

let part2_answer input =
  input |> RPS_Game.of_outcome_lines |> RPS_Game.score |> Int.to_string
  |> Stdio.print_endline

let () =
  Stdio.print_endline "=== Part 2 ===";
  Stdio.print_string "Demo: ";
  Advent.read_lines "../data/day2-example-input.txt" |> part2_answer;
  Stdio.print_string "Answer: ";
  Advent.read_lines "../data/day2-input.txt" |> part2_answer
