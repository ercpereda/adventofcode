open Base

let lex_line (line : string) =
  let chars = String.to_list line in
  let rec lexer chars =
    match chars with
    | ' ' :: ' ' :: ' ' :: ' ' :: tl -> None :: lexer tl
    | ' ' :: ' ' :: ' ' :: _ -> [ None ]
    | '[' :: id :: ']' :: ' ' :: tl -> Some id :: lexer tl
    | '[' :: id :: ']' :: _ -> [ Some id ]
    | _ -> failwith "Invalid crates line"
  in
  lexer chars

let add_crate stacks crate idx =
  stacks.(idx) <- crate :: stacks.(idx);
  stacks

let add_crates_line stacks crates =
  let rec _add_crates stacks crates idx =
    match crates with
    | [] -> stacks
    | crate :: next_crates -> (
        match crate with
        | None -> _add_crates stacks next_crates (idx + 1)
        | Some c -> _add_crates (add_crate stacks c idx) next_crates (idx + 1))
  in
  _add_crates stacks crates 0

let make_stacks crates =
  let len =
    match crates with
    | [] -> failwith "Invalid crates"
    | c :: _ -> List.length c
  in
  List.fold_left ~f:add_crates_line ~init:(Array.create ~len []) crates
  |> Array.map ~f:List.rev

let split_lines input =
  List.partition_tf ~f:(String.is_prefix ~prefix:"move") input

let create_stacks crates_lines =
  crates_lines
  |> List.filter ~f:(fun s ->
         (String.is_prefix ~prefix:" 1" s || String.is_empty s) |> not)
  |> List.map ~f:lex_line |> make_stacks

type action = { move : int; from : int; to_ : int }

let parse_action action_line =
  match String.split ~on:' ' action_line with
  | [ _; move; _; from; _; to_ ] ->
      {
        move = Int.of_string move;
        from = Int.of_string from - 1;
        to_ = Int.of_string to_ - 1;
      }
  | _ -> failwith "Invalid action line"

let create_actions action_lines = List.map ~f:parse_action action_lines

let rec execute_action_9000 stacks action =
  match action with
  | { move = 0; from = _; to_ = _ } -> stacks
  | { move; from; to_ } -> (
      match stacks.(from) with
      | [] -> failwith "Can remove crate stack empty"
      | crate :: stack ->
          stacks.(from) <- stack;
          stacks.(to_) <- crate :: stacks.(to_);
          execute_action_9000 stacks { action with move = move - 1 })

let execute_actions_9000 stacks actions =
  List.fold ~f:execute_action_9000 ~init:stacks actions

let execute_action_9001 stacks action =
  match action with
  | { move; from; to_ } ->
      let top, botton = List.split_n stacks.(from) move in
      stacks.(from) <- botton;
      stacks.(to_) <- List.append top stacks.(to_);
      stacks

let execute_actions_9001 stacks actions =
  List.fold ~f:execute_action_9001 ~init:stacks actions

let stacks_top stacks =
  Array.fold stacks ~init:"" ~f:(fun acc stack ->
      let crate =
        match stack with [] -> "" | crate :: _ -> String.of_char crate
      in
      acc ^ crate)

let part1_answer input =
  let actions_lines, crates_lines = split_lines input in
  let stacks = create_stacks crates_lines in
  let actions = create_actions actions_lines in
  execute_actions_9000 stacks actions |> stacks_top |> Stdio.print_endline

let () =
  Stdio.print_endline "=== Part 1 ===";
  Stdio.print_string "Demo: ";
  Advent.read_lines "../data/day5-example-input.txt" |> part1_answer;
  Stdio.print_string "Answer: ";
  Advent.read_lines "../data/day5-input.txt" |> part1_answer

let part2_answer input =
  let actions_lines, crates_lines = split_lines input in
  let stacks = create_stacks crates_lines in
  let actions = create_actions actions_lines in
  execute_actions_9001 stacks actions |> stacks_top |> Stdio.print_endline

let () =
  Stdio.print_endline "=== Part 2 ===";
  Stdio.print_string "Demo: ";
  Advent.read_lines "../data/day5-example-input.txt" |> part2_answer;
  Stdio.print_string "Answer: ";
  Advent.read_lines "../data/day5-input.txt" |> part2_answer
