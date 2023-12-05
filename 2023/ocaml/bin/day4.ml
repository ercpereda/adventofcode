open Base

let strip = List.map ~f:String.strip

module ScratchCard = struct
  type t = {
    (* id: int; *)
    winning_numbers: (string, String.comparator_witness) Set.t;
    numbers_you_have: (string, String.comparator_witness) Set.t;
    copies: int;
  }

  let of_string s =
    let create_set numbers =
      numbers
      |> String.split ~on:' '
      |> List.filter ~f:(fun i -> not (String.is_empty i))
      |> strip
      |> List.fold ~init:(Set.empty (module String)) ~f:Set.add
    in
    (* let get_card_id cardid = *)
    (*   match cardid |> String.split ~on:' ' |> strip |> List.filter ~f:(fun i -> not (String.is_empty i)) with *)
    (*   | [_; id] -> Int.of_string id *)
    (*   | _ -> failwith "Invalid cardid" *)
    (* in *)
    match s |> String.split ~on:':' |> strip with
    | [_cid; numbers] -> begin
        match numbers |> String.split ~on:'|' |> strip |> List.map ~f:create_set with
        | [wn; nyh] -> { 
            (* id = get_card_id cid; *)
            winning_numbers = wn;
            numbers_you_have = nyh;
            copies = 1;
          }
        | _ -> failwith "Invalid card numbers"
      end
    | _ -> failwith "Invalid card"

  let matching_numbers sc = Set.inter sc.winning_numbers sc.numbers_you_have

  let amount_of_matching_numbers sc = sc |> matching_numbers |> Set.length

  let points sc =
    let n = amount_of_matching_numbers sc in
    if n = 0 then 0 else 2 ** (n - 1)
end

let part1_answer input =
  input
  |> List.map ~f:ScratchCard.of_string
  |> List.map ~f:ScratchCard.points
  |> Advent.sum_listof_ints
  |> Int.to_string
  |> Stdio.print_endline

let () =
  Stdio.print_endline "=== Part 1 ===";
  Stdio.print_string "Demo: ";
  Advent.read_lines "../data/day4-example-input.txt" |> part1_answer;
  Stdio.print_string "Answer: ";
  Advent.read_lines "../data/day4-input.txt" |> part1_answer

module ScratchCardStack = struct
  type t = ScratchCard.t list

  let rec calculate_copies (scs: t) =
    let rec add_copy (scs: t) inc copies =
      match copies, scs with
      | 0, l -> l
      | _, [] -> []
      | c, hd :: tl -> {hd with copies = hd.copies + inc } :: (add_copy tl inc (c - 1))
    in
    match scs with
    | [] -> []
    | hd :: tl -> hd :: calculate_copies (add_copy tl hd.copies (ScratchCard.amount_of_matching_numbers hd))
end

let part2_answer input =
  input
  |> List.map ~f:ScratchCard.of_string
  |> ScratchCardStack.calculate_copies
  |> List.map ~f:(fun (sc: ScratchCard.t) -> sc.copies)
  |> Advent.sum_listof_ints
  |> Int.to_string
  |> Stdio.print_endline

let () =
  Stdio.print_endline "=== Part 2 ===";
  Stdio.print_string "Demo: ";
  Advent.read_lines "../data/day4-example-input.txt" |> part2_answer;
  Stdio.print_string "Answer: ";
  Advent.read_lines "../data/day4-input.txt" |> part2_answer
