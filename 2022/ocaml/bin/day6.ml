open Base

let start_of_packet ?distinct_chars:(n = 4) packet =
  let rec _start_of_packet acc i packet =
    match packet with
    | [] -> failwith "Invalid packet, doesn't have start"
    | hd :: tl ->
        if Queue.length acc < n then (
          ignore (Queue.enqueue acc hd);
          _start_of_packet acc (i + 1) tl)
        else (
          ignore (Queue.dequeue acc);
          ignore (Queue.enqueue acc hd);
          if List.contains_dup (Queue.to_list acc) ~compare:Char.compare then
            _start_of_packet acc (i + 1) tl
          else i + 1)
  in
  _start_of_packet (Queue.create ~capacity:4 ()) 0 (String.to_list packet)

let rec part1_answer input =
  match input with
  | [] -> ()
  | line :: rest ->
      start_of_packet line |> Int.to_string |> Stdio.print_endline;
      part1_answer rest

let () =
  Stdio.print_endline "=== Part 1 ===";
  Stdio.print_endline "Demo: ";
  Advent.read_lines "../data/day6-example-input.txt" |> part1_answer;
  Stdio.print_string "Answer: ";
  Advent.read_lines "../data/day6-input.txt" |> part1_answer

let start_of_message = start_of_packet ~distinct_chars:14

let rec part2_answer input =
  match input with
  | [] -> ()
  | line :: rest ->
      start_of_message line |> Int.to_string |> Stdio.print_endline;
      part2_answer rest

let () =
  Stdio.print_endline "=== Part 2 ===";
  Stdio.print_endline "Demo: ";
  Advent.read_lines "../data/day6-part2-example-input.txt" |> part2_answer;
  Stdio.print_string "Answer: ";
  Advent.read_lines "../data/day6-input.txt" |> part2_answer
