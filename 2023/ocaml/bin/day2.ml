open Base

let strip = List.map ~f:String.strip

module GameSet = struct
  type t = {blue: int; red: int; green: int}

  let empty = {blue = 0; red = 0; green = 0}
  
  let of_string s =
    let parse_cubes acc cube =
      match cube |> String.split ~on:' ' |> strip with
      | [quantity; "blue"] -> { acc with blue = Int.of_string quantity} 
      | [quantity; "red"] -> { acc with red = Int.of_string quantity} 
      | [quantity; "green"] -> { acc with green = Int.of_string quantity} 
      | _ -> failwith "Invalid cube"
    in
    s |> String.split ~on:',' |> strip |> List.fold ~f:parse_cubes ~init:{blue = 0; red = 0; green = 0}

    let is_possible reference gameset = gameset.blue <= reference.blue && gameset.red <= reference.red && gameset.green <= reference.green

    let max gameset1 gameset2 =
      {
      blue = Int.max gameset1.blue gameset2.blue;
      red = Int.max gameset1.red gameset2.red;
      green = Int.max gameset1.green gameset2.green;
      }

    let power gameset = gameset.blue * gameset.green * gameset.red
end

module Game = struct
  type t = {id: int; sets: GameSet.t list}

  let of_string s =
    let parse_gameid gameid =
      match gameid |> String.split ~on:' ' |> strip with
      |   ["Game"; id] -> Int.of_string id
      | _ -> failwith "Invalid gameid"
    in
    let parse_gamesets gamesets =
      gamesets |> String.split ~on:';' |> strip |> List.map ~f:GameSet.of_string
    in
    match s |> String.split ~on:':' |> strip with
    | [gameid; gamesets] -> {id = parse_gameid gameid; sets = parse_gamesets gamesets}
    | _ -> failwith "Invalid game"

  let is_possible reference game = List.for_all ~f:(GameSet.is_possible reference) game.sets

  let min_number_of_cubes game = List.fold ~init:GameSet.empty ~f:GameSet.max game.sets
end

let part1_answer input =
  input
  |> List.map ~f:Game.of_string
  |> List.filter ~f:(Game.is_possible (GameSet.of_string "12 red, 13 green, 14 blue"))
  |> List.map ~f:(fun (game: Game.t) -> game.id)
  |> List.fold ~init:0 ~f:( + )
  |> Int.to_string 
  |> Stdio.print_endline

let () =
  Stdio.print_endline "=== Part 1 ===";
  Stdio.print_string "Demo: ";
  Advent.read_lines "../data/day2-example-input.txt" |> part1_answer;
  Stdio.print_string "Answer: ";
  Advent.read_lines "../data/day2-input.txt" |> part1_answer

let part2_answer input =
  input
  |> List.map ~f:Game.of_string
  |> List.map ~f:Game.min_number_of_cubes
  |> List.map ~f:GameSet.power
  |> List.fold ~init:0 ~f:( + )
  |> Int.to_string 
  |> Stdio.print_endline

let () =
  Stdio.print_endline "=== Part 2 ===";
  Stdio.print_string "Demo: ";
  Advent.read_lines "../data/day2-example-input.txt" |> part2_answer;
  Stdio.print_string "Answer: ";
  Advent.read_lines "../data/day2-input.txt" |> part2_answer
