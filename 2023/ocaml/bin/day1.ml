open Base

let fst_lst_digits acc item =
  match (item |> Char.to_string |> Int.of_string_opt) with 
  | None -> acc
  | Some digit -> (
      match acc with 
      | None, _ -> (Some digit, None)
      | fst, _ -> (fst, Some digit)
    )

let concat_digits digits =
  let concatenated = (match digits with
  | Some fst, Some lst -> (Int.to_string fst) ^ (Int.to_string lst) 
  | Some fst, None -> let fst_str = Int.to_string fst in fst_str ^ fst_str
  | _ -> failwith "Invalid digits"
  ) 
  in Int.of_string concatenated

let find_fst_lst_digits line = 
  List.fold ~f:fst_lst_digits ~init:(None, None) line

let part1_answer input =
  input
  |> List.map ~f:String.to_list 
  |> List.map ~f:find_fst_lst_digits
  |> List.map ~f:concat_digits
  |> List.fold ~init:0 ~f:( + )
  |> Int.to_string 
  |> Stdio.print_endline

let () =
  Stdio.print_endline "=== Part 1 ===";
  Stdio.print_string "Demo: ";
  Advent.read_lines "../data/day1-part1-example-input.txt" |> part1_answer;
  Stdio.print_string "Answer: ";
  Advent.read_lines "../data/day1-input.txt" |> part1_answer

let rec add_digits_to_line line =
  match line with
  | [] -> []
  | 'z' :: 'e' :: 'r' :: 'o' :: rest -> '0' :: 'z' :: 'e' :: 'r' :: 'o' :: add_digits_to_line ('e' :: 'r' :: 'o' :: rest)
  | 'o' :: 'n' :: 'e' :: rest -> '1' :: 'o' :: 'n' :: 'e' :: add_digits_to_line ('n' :: 'e' :: rest)
  | 't' :: 'w' :: 'o' :: rest -> '2' :: 't' :: 'w' :: 'o' :: add_digits_to_line ('w' :: 'o' :: rest)
  | 't' :: 'h' :: 'r' :: 'e' :: 'e' :: rest -> '3' :: 't' :: 'h' :: 'r' :: 'e' :: 'e' :: add_digits_to_line ('h' :: 'r' :: 'e' :: 'e' :: rest)
  | 'f' :: 'o' :: 'u' :: 'r' :: rest ->  '4' :: 'f' :: 'o' :: 'u' :: 'r' :: add_digits_to_line ('o' :: 'u' :: 'r' :: rest)
  | 'f' :: 'i' :: 'v' :: 'e' :: rest ->  '5' :: 'f' :: 'i' :: 'v' :: 'e' ::  add_digits_to_line ('i' :: 'v' :: 'e' :: rest)
  | 's' :: 'i' :: 'x' :: rest -> '6' :: 's' :: 'i' :: 'x' :: add_digits_to_line ('i' :: 'x' :: rest)
  | 's' :: 'e' :: 'v' :: 'e' :: 'n' :: rest -> '7' :: 's' :: 'e' :: 'v' :: 'e' :: 'n' :: add_digits_to_line ('e' :: 'v' :: 'e' :: 'n' :: rest)
  | 'e' :: 'i' :: 'g' :: 'h' :: 't' :: rest -> '8' :: 'e' :: 'i' :: 'g' :: 'h' :: 't' :: add_digits_to_line ('i' :: 'g' :: 'h' :: 't' :: rest)
  | 'n' :: 'i' :: 'n' :: 'e' :: rest ->  '9' :: 'n' :: 'i' :: 'n' :: 'e' ::  add_digits_to_line ('i' :: 'n' :: 'e' :: rest)
  | hd :: tl -> hd :: add_digits_to_line tl

let part2_answer input =
  input
  |> List.map ~f:String.to_list
  |> List.map ~f:add_digits_to_line
  |> List.map ~f:find_fst_lst_digits
  |> List.map ~f:concat_digits
  |> List.fold ~init:0 ~f:( + )
  |> Int.to_string
  |> Stdio.print_endline

let () =
  Stdio.print_endline "=== Part 2 ===";
  Stdio.print_string "Demo: ";
  Advent.read_lines "../data/day1-part2-example-input.txt" |> part2_answer;
  Stdio.print_string "Answer: ";
  Advent.read_lines "../data/day1-input.txt" |> part2_answer
