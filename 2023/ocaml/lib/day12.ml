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

let to_string springs damage_groups =
  let springs_s = List.map springs ~f:Char.to_string |> String.concat ~sep:"" in
  let damage_groups_s = List.map damage_groups ~f:Int.to_string |> String.concat ~sep:"," in
  springs_s ^ "|" ^ damage_groups_s

let arrangements springs damage_groups =
  let rec _arrangements springs damage_groups in_group cache =
    match springs, damage_groups, in_group with
    | [], [], _ -> cache, 1
    | [], 0::dgs_rest, _ -> cached_call [] dgs_rest false cache
    | [], _::_, _ -> cache, 0

    | '#'::_, [], _ -> cache, 0
    | '#'::_, 0::_, _ -> cache, 0
    | '#'::spring_rest, dg::dgs_rest, _ -> cached_call spring_rest ((dg-1)::dgs_rest) true cache

    | '.'::spring_rest, [], _ -> cached_call spring_rest [] false cache
    | '.'::spring_rest, 0::dgs_rest, true -> cached_call spring_rest dgs_rest false cache
    | '.'::_, _::_, true -> cache, 0
    | '.'::spring_rest, dg::dgs_rest, false -> cached_call spring_rest (dg::dgs_rest) false cache

    | '?'::spring_rest, [], _ -> cached_call ('.'::spring_rest) [] false cache
    | '?'::spring_rest, dgs, _
        ->  let c, x = cached_call ('#'::spring_rest) dgs in_group cache in
            let c', y = cached_call ('.'::spring_rest) dgs in_group c in
            c', x + y

    | _ -> failwith ((springs |> List.map ~f:Char.to_string |> String.concat ~sep:"") ^ " " ^ (damage_groups |> List.map ~f:Int.to_string |> String.concat ~sep:","))
  and cached_call springs damage_groups in_group cache =
    let key = to_string springs damage_groups in
    match Map.find cache key with
    | Some x -> cache, x
    | None -> let c, x = _arrangements springs damage_groups in_group cache in
              Map.set c ~key ~data:x, x
  in
  _arrangements springs damage_groups false (Map.empty (module String))

let part1_answer input = input
  |> List.map ~f:(fun l -> let s, dg = parse_line l in arrangements s dg |> snd)
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

let part2_answer input = input
  |> List.map ~f:(fun l -> let s, dg = parse_line_with_unfold l in arrangements s dg |> snd)
  |> List.fold ~init:0 ~f:( + )
  |> Int.to_string
  |> Stdio.print_endline
