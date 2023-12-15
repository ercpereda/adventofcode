open Base;;

let parse_springs = String.to_list

let parse_damanage_groups s = s
|> String.split ~on:',' 
|> List.map ~f:Int.of_string

let parse_line l =
  match String.split l ~on:' ' with
  | [s; dg] -> parse_springs s, parse_damanage_groups dg
  | _ -> invalid_arg ("Invalida line " ^ l)

let rec validate_group springs damage_group =
  match springs with
  | [] when damage_group > 0 -> false, []
  | [] when damage_group = 0 -> true, []
  | hd::tl when (Char.equal '#' hd && damage_group > 0) -> validate_group tl (damage_group - 1)
  | hd::tl when (Char.equal '#' hd && damage_group = 0) -> false, tl
  | hd::tl when (Char.equal '.' hd && damage_group > 0) -> false, tl
  | hd::tl when (Char.equal '.' hd && damage_group = 0) -> true, tl
  | _ -> failwith ((springs |> List.map ~f:Char.to_string |> String.concat ~sep:"") ^ " " ^ (damage_group |> Int.to_string ))

let rec arrangements springs damage_groups in_group =
  match springs, damage_groups, in_group with
  | [], [], _ -> 1
  | [], 0::dgs_rest, _ -> arrangements [] dgs_rest false
  | [], _::_, _ -> 0

  | '#'::_, [], _ -> 0
  | '#'::_, 0::_, _ -> 0
  | '#'::spring_rest, dg::dgs_rest, _ -> arrangements spring_rest ((dg-1)::dgs_rest) true

  | '.'::spring_rest, [], _ -> arrangements spring_rest [] false
  | '.'::spring_rest, 0::dgs_rest, true -> (arrangements spring_rest dgs_rest false)
  | '.'::_, _::_, true -> 0
  | '.'::spring_rest, dg::dgs_rest, false -> (arrangements spring_rest (dg::dgs_rest) false)

  | '?'::spring_rest, [], _ -> arrangements ('.'::spring_rest) [] false
  | '?'::spring_rest, dgs, _
      -> (arrangements ('#'::spring_rest) dgs in_group) + (arrangements ('.'::spring_rest) dgs in_group)

  | _ -> failwith ((springs |> List.map ~f:Char.to_string |> String.concat ~sep:"") ^ " " ^ (damage_groups |> List.map ~f:Int.to_string |> String.concat ~sep:","))

let part1_answer input = input
  |> List.map ~f:(fun l -> let s, dg = parse_line l in arrangements s dg false)
  |> List.fold ~init:0 ~f:( + )
  |> Int.to_string
  |> Stdio.print_endline

let unfold_springs_string springs = List.range 0 5
  |> List.fold ~init:[] ~f:( fun acc _ -> (springs::acc) )
  |> String.concat ~sep:"?"

let parse_spring_with_unfold s = s
  |> unfold_springs_string
  |> parse_springs

let unfold_danger_groups_string dg = List.range 0 5
  |> List.fold ~init:[] ~f:( fun acc _ -> (dg::acc) )
  |> String.concat ~sep:","

let parse_damanage_groups_with_unfold s = s
  |> unfold_danger_groups_string
  |> parse_damanage_groups

let parse_line_with_unfold l =
  match String.split l ~on:' ' with
  | [s; dg] -> parse_spring_with_unfold s, parse_damanage_groups_with_unfold dg
  | _ -> invalid_arg ("Invalida line " ^ l)

let part2_answer _input =
  "Too slow"
  |> Stdio.print_endline
  (* |> List.mapi ~f:(fun i l -> (Stdio.print_endline (Int.to_string i ^ " | " ^ l) ;let s, dg = parse_line_with_unfold l in arrangements s dg false)) *)
  (* |> List.fold ~init:0 ~f:( + ) *)
  (* |> Int.to_string *)
  (* |> Stdio.print_endline *)
