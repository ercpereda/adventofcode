open Base

let get_number sch pos =
  let rec _get_number number ~row ~j ~inc ~concat =
    let n = Array.length sch in
    if j < 0 || n <= j then number 
    else
      let value = row.(j) in
      match value with
      | '0'..'9' -> (
          row.(j) <- 'n';
          _get_number ~row ~j:(j + inc) ~inc ~concat (concat number (Char.to_string value)))
      | _ -> number
  in
  let i, j = pos in
  let number = _get_number ~row:sch.(i) ~j ~inc:1 ~concat:(fun a b -> a ^ b) "" in
  let number = _get_number ~row:sch.(i) ~j:(j-1) ~inc:(-1) ~concat:(fun a b -> b ^ a) number in
  Int.of_string number

let get_neighbor_numbers ?(piece=None) sch pos =
  let number_in_direction sch pos direction =
    let i, j = pos in
    let di, dj = direction in
    let i', j' = (i + di, j+dj) in
    let m, n = (Array.length sch, Array.length sch.(0)) in
    if (i' < 0 || m <= i') || (j' < 0 || n <= j') then None
    else (
      match sch.(i').(j') with
      | '0'..'9' -> Some (get_number sch (i', j'))
      | _ -> None
    )
  in
  let get_in_all_directions pos =
    let directions = [(-1, 1); (0, 1); (1, 1); (1, 0); (1, -1); (0, -1); (-1, -1)] in
    List.fold ~init:[] ~f:(fun acc direction -> match number_in_direction sch pos direction with None -> acc | Some n -> n :: acc) directions
  in
  let i, j = pos in
  match piece with
  | None -> begin
      match sch.(i).(j) with
      | '0'..'9' -> []
      | '.' -> []
      | 'n' -> []
      | _ -> get_in_all_directions pos
    end
  | Some p -> if Char.equal sch.(i).(j) p then get_in_all_directions pos else []

let get_part_numbers sch =
  Array.foldi sch ~init:[] ~f:(fun i acc _ ->
    acc @ (Array.foldi sch ~init:[] ~f:(fun j acc _ -> 
      acc @ (get_neighbor_numbers sch (i, j)))))

let sum_part_numbers = List.fold ~init:0 ~f:( + )

let parse_input input =
  input
  |> List.map ~f:String.to_array 
  |> List.to_array

let part1_answer input =
  input
  |> parse_input
  |> get_part_numbers
  |> sum_part_numbers
  |> Int.to_string 
  |> Stdio.print_endline

let () =
  Stdio.print_endline "=== Part 1 ===";
  Stdio.print_string "Demo: ";
  Advent.read_lines "../data/day3-example-input.txt" |> part1_answer;
  Stdio.print_string "Answer: ";
  Advent.read_lines "../data/day3-input.txt" |> part1_answer


let get_gear_numbers sch pos =
  let numbers = get_neighbor_numbers ~piece:(Some '*') sch pos in
  match List.length numbers with
  | 2 -> numbers
  | _ -> []

let get_all_gear_numbers sch =
  Array.foldi sch ~init:[] ~f:(fun i acc _ ->
    acc @ (Array.foldi sch ~init:[] ~f:(fun j acc _ -> 
      let gear_numbers = (get_gear_numbers sch (i, j)) in 
      match gear_numbers with 
      | [] -> acc
      | gn -> gn :: acc)
    ))

let gear_ratio = List.fold ~init:1 ~f:( * )

let get_all_gear_ratios = List.map ~f:gear_ratio

let add_gear_ratios = List.fold ~init:0 ~f:( + )

let part2_answer input =
  input
  |> parse_input
  |> get_all_gear_numbers
  |> get_all_gear_ratios
  |> add_gear_ratios
  |> Int.to_string 
  |> Stdio.print_endline

let () =
  Stdio.print_endline "=== Part 2 ===";
  Stdio.print_string "Demo: ";
  Advent.read_lines "../data/day3-example-input.txt" |> part2_answer |> ignore;
  Stdio.print_string "Answer: ";
  Advent.read_lines "../data/day3-input.txt" |> part2_answer
