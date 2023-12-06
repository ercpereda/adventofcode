open Base

module RangeMap = struct
  type t = {
    source_category: int;
    destination_category: int;
    range_lenght: int;
  }

  let create = function
    | dc, sc, rl -> {
      source_category = sc;
      destination_category = dc;
      range_lenght = rl;
    }

let of_string s = s 
  |> String.split ~on:' '
  |> List.map ~f:Int.of_string
  |> fun l -> match l with
     | [dc; sc; rl] -> create (dc, sc, rl)
     | _ -> failwith "Invalid map range format"


  let get x rm =
    let x' = x - rm.source_category in
    if x' < 0 || rm.source_category + x' >= rm.source_category + rm.range_lenght then None
    else Some (rm.destination_category + x')
end

module RangeMaps = struct
  type t = RangeMap.t list

  let of_string_list sl = List.map ~f:RangeMap.of_string sl

  let get x rms =
    match List.find_map ~f:(RangeMap.get x) rms with
    | None -> x
    | Some x' -> x'
end

module Almanac = struct
  type t = {
    seeds: int list;
    seeds_ranges: (int * int) list;
    maps: RangeMaps.t list;
  }

  let parse_seeds s =
    s
    |> String.split ~on:' '
    |> List.tl_exn
    |> List.map ~f:Int.of_string

  let parse_seeds_ranges s =
    let rec _make_pairs l =
      match l with
      | [] -> []
      | a :: b :: tl -> (a, a + b - 1) :: _make_pairs tl 
      | _ -> failwith "Invalid list of pairs"
    in
    s
    |> parse_seeds
    |> _make_pairs


  let parse_maps l =
    let rec _parse_maps l =
      match l with
      | [] -> []
      | _ :: tl ->
          let map, rest = List.split_while tl ~f:(fun line -> not(String.equal line "")) 
          in
          (List.tl_exn map) :: _parse_maps rest
    in
    l |> _parse_maps |> List.map ~f:RangeMaps.of_string_list

  let of_string_list sl = 
  {
    seeds = parse_seeds (List.hd_exn sl);
    seeds_ranges = parse_seeds_ranges (List.hd_exn sl);
    maps = parse_maps (List.tl_exn sl);
  }

  let get_seed_location alm seed =
    List.fold ~init:seed ~f:RangeMaps.get alm.maps

  let _get_all_seed_locations alm seeds =
    List.map ~f:(get_seed_location alm) seeds

  let get_all_seed_locations alm = _get_all_seed_locations alm alm.seeds

  let get_seed_range_locations alm range =
    List.range (fst range) (snd range)
    |> _get_all_seed_locations alm

  let _get_all_seed_range_locations alm ranges =
    List.map ~f:(get_seed_range_locations alm) ranges

  let get_all_seed_range_locations alm = _get_all_seed_range_locations alm alm.seeds_ranges
    
end

(* let almanac = Advent.read_lines "../data/day5-example-input.txt" |> Almanac.of_string_list *)

let part1_answer input =
  input
  |> Almanac.of_string_list
  |> Almanac.get_all_seed_locations
  |> List.min_elt ~compare:Int.compare
  |> Option.value_exn
  |> Int.to_string
  |> Stdio.print_endline

let () =
  Stdio.print_endline "=== Part 1 ===";
  Stdio.print_string "Demo: ";
  Advent.read_lines "../data/day5-example-input.txt" |> part1_answer;
  Stdio.print_string "Answer: ";
  Advent.read_lines "../data/day5-input.txt" |> part1_answer


let _part2_answer input =
  input
  |> Almanac.of_string_list
  |> Almanac.get_all_seed_range_locations
  |> List.concat
  |> List.min_elt ~compare:Int.compare
  |> Option.value_exn
  |> Int.to_string
  |> Stdio.print_endline

let () =
  Stdio.print_endline "=== Part 2 ===";
  Stdio.print_endline "Not implemented"
  (* Stdio.print_string "Demo: "; *)
  (* Advent.read_lines "../data/day5-example-input.txt" |> part2_answer; *)
  (* Stdio.print_string "Answer: "; *)
  (* Advent.read_lines "../data/day5-input.txt" |> part2_answer *)

