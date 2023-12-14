open Base

(* let input = Advent.read_lines "../data/day11-example-input.txt" *)

module SkyMap = struct
  type t = char array array

  let of_string_list lst = lst
    |> List.map ~f:String.to_array
    |> List.to_array

  let expanded_rows sm =
    Array.filter_mapi sm ~f:begin
      fun i row -> if Array.for_all ~f:(Char.equal '.') row then Some i else None
    end
    |> Array.to_list
    |> List.rev

  let expanded_cols (sm: t) =
    let m, n = Array.length sm, Array.length sm.(0) in
    let result = ref [] in
    for j = 0 to n - 1 do
      let is_expanded = ref true in
        for i = 0 to m - 1 do
          is_expanded := !is_expanded && (Char.equal '.' sm.(i).(j))
        done;
        !is_expanded |> Bool.to_string |> Stdio.print_endline;
        result := if !is_expanded then j::!result else !result;
    done;
    !result

  type expanded_galaxy_coords = {
    coords: (int * int) array;
    exp_cols: int list;
    exp_rows: int list;
    i_expantion: int;
    j_expantion: int;
  }

  let create_expanded_galaxy_coords sm = {
    coords = Array.create (0, 0) ~len:0;
    exp_cols = expanded_cols sm;
    exp_rows = expanded_rows sm;
    i_expantion = 0;
    j_expantion = 0;
  }

  let galaxies_coords (sm: t) =
    let galaxies_in_row row = row 
      |> Array.filter_mapi ~f:(fun i c -> if Char.equal c '#' then Some i else None)
    in
    Array.foldi sm ~init:(Array.create (0, 0) ~len:0 ) ~f:begin 
      fun i acc row -> row
      |> galaxies_in_row
      |> (Array.map ~f:(fun j -> (i, j))) 
      |> Array.append acc
    end

  (* let galaxies_coords sm = *)
  (*   let m, n = Array.length sm, Array.length sm.(0) in *)
  (*   let exp_rows, exp_cols = ref (expanded_rows sm), ref (expanded_cols sm) in *)
  (*   let result = ref [] in *)
  (*   for i = m - 1 to 0 do *)
  (*     for j = n - 1 to 0 do *)
  (*       let exp_row = (!expanded_rows).head_ex in  *)
  (*       let i' = (!expanded_rows).head_ex = i *)
  (*     done *)
  (*   done *)

    (* let galaxies_in_row row = row  *)
    (*   |> List.filter_mapi ~f:(fun i c -> if Char.equal c '#' then Some i else None) *)
    (* in *)
    (* List.foldi sm ~init:[] ~f:begin  *)
    (*   fun i acc row -> row  *)
    (*   |> galaxies_in_row *)
    (*   |> (List.map ~f:(fun j -> (i, j)))  *)
    (*   |> List.append acc *)
    (* end *)

  let calculate_dist g1 g2 =
    (Int.abs (fst g1 - fst g2), Int.abs (snd g1 - snd g2))

  let distances gc =
    let rec _distances acc galaxies =
      match galaxies with
      | [] -> acc
      | galaxy::others -> 
          let distances = List.fold others ~init:[] ~f:begin
            fun acc other_galaxy -> 
              (calculate_dist galaxy other_galaxy)::acc
            end
          in
          _distances (List.append distances acc) others
    in 
    _distances [] gc
      

  let to_string sm = sm
    |> Array.map ~f:String.of_array
    |> String.concat_array ~sep:"\n"
end

let part1_answer _input =
  "Not implemented"
  |> Stdio.print_endline

let part2_answer _input =
  "Not implemented"
  |> Stdio.print_endline
