open Base

module History = struct
  type t = int list list

  let of_string ?(backwards=true) s = 
    let l = s
      |> String.split ~on:' '
      |> (fun l -> if backwards then List.rev l else l)
      |> List.map ~f:Int.of_string
    in
    [l]

  let reduce h =
    let rec _reduce = function
    | [] -> []
    | first :: second :: tl -> (first - second) :: _reduce (second :: tl)
    | _ :: _ -> []
    in
    match h with
    | [] -> invalid_arg "History can't be empty"
    | hd :: _ -> (_reduce hd) :: h

  let rec reduce_until_all_zeros h =
    match h with
    | [] -> invalid_arg "History can't be empty"
    | hd :: _ ->  if List.for_all hd ~f:(Int.equal 0) then h 
                  else reduce_until_all_zeros (reduce h)


  let next_seq h =
    let increase_seq s1 s2 =
      match s1, s2 with
      | h1 :: _, h2 :: _ -> (h1 + h2) :: s2
      | _ -> invalid_arg "List can't be empty"
    in
    let rec loop h =
      match h with
      | [] -> invalid_arg "Never get to this step"
      | hd :: [] -> hd
      | first :: second :: tl ->
          let new_second = increase_seq first second in 
          loop (new_second :: tl)
    in
    loop h |> List.hd_exn
end


let process_line line ~backwards = line
  |> History.of_string ~backwards
  |> History.reduce_until_all_zeros
  |> History.next_seq

let part1_answer input =
  input
  |> List.map ~f:(process_line ~backwards:true)
  |> List.fold ~init:0 ~f:( + )
  |> Int.to_string
  |> Stdio.print_endline

let part2_answer input = input
  |> List.map ~f:(process_line ~backwards:false)
  |> List.fold ~init:0 ~f:( + )
  |> Int.to_string
  |> Stdio.print_endline
