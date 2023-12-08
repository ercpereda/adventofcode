open Base

module Label = struct
  type t = Ace | King | Queen | Jack | Ten | Nine | Eight | Seven | Six | Five | Four | Three | Two | Jocker

  let strength = function
  | Ace -> 14
  | King -> 13
  | Queen -> 12
  | Jack -> 11
  | Ten -> 10
  | Nine -> 9
  | Eight -> 8
  | Seven -> 7
  | Six -> 6
  | Five -> 5
  | Four -> 4
  | Three -> 3
  | Two -> 2
  | Jocker -> 1

  let of_char = function
  | 'A' -> Ace 
  | 'K' -> King
  | 'Q' -> Queen
  | 'J' -> Jack
  | 'T' -> Ten
  | '9' -> Nine
  | '8' -> Eight
  | '7' -> Seven
  | '6' -> Six
  | '5' -> Five
  | '4' -> Four
  | '3' -> Three
  | '2' -> Two
  | _ -> invalid_arg "Invalid card label"

  let of_char_with_jocker c = 
    match c with
    | 'J' -> Jocker
    | _ -> of_char c

  let compare l1 l2 = Int.compare (strength l1) (strength l2)

  let compare_from_tuple ls = compare (fst ls) (snd ls)

  let equal l1 l2 = compare l1 l2 = 0

  let%test "equal" = equal Ace Ace
end

module HandType = struct
  type t = FiveOfAKind | FourOfAKind | FullHouse | ThreeOfAKind | TwoPair | OnePair | HighCard

  let group hand =
    let rec _group hand =
      match hand with
      | [] -> []
      | hd :: tl -> let hd', tl'  = List.split_while tl ~f:(Label.equal hd) in (hd :: hd') :: _group tl'
    in
    let hand_without_jockers, jockers = hand
      |> List.sort ~compare:(fun l1 l2 -> -(Label.compare l1 l2))
      |> List.split_while ~f:(fun l -> not(Label.equal Label.Jocker l))
    in
    let groups = _group hand_without_jockers
    |> List.sort ~compare:(fun x y -> -(Int.compare (List.length x) (List.length y)))
    in
    match groups with
    | [] -> [jockers]
    | hd :: tl -> (List.append jockers hd) :: tl

  let of_hand hand =
    match group hand with
    | [_] -> FiveOfAKind
    | [a; _] when List.length a = 4 -> FourOfAKind
    | [a; _] when List.length a = 3 -> FullHouse
    | [a; _; _] when List.length a = 3-> ThreeOfAKind
    | [a; _; _] when List.length a = 2 -> TwoPair
    | [_; _; _; _] -> OnePair
    | [_; _; _; _; _] -> HighCard
    | _ -> invalid_arg "Invalid hand"

  let strength = function
    | FiveOfAKind -> 7
    | FourOfAKind -> 6
    | FullHouse -> 5
    | ThreeOfAKind -> 4
    | TwoPair -> 3
    | OnePair -> 2
    | HighCard -> 1
end

module Hand = struct
  type t = Label.t list

  let of_string ?(with_jocker=false) s =
    let labels = s
      |> String.to_list
      |> List.map ~f:(fun c -> if with_jocker then Label.of_char_with_jocker c else Label.of_char c)
    in
    match labels with
    | [_; _; _; _; _] -> labels
    | _ -> invalid_arg "Invalid hand string"


  let compare hand1 hand2 =
    let compare_types = Int.compare (hand1 |> HandType.of_hand |> HandType.strength) (hand2 |> HandType.of_hand |> HandType.strength) in
    if compare_types <> 0 then compare_types 
    else List.zip_exn hand1 hand2 
      |> List.drop_while ~f:(fun ls -> Label.compare_from_tuple ls = 0)
      |> List.hd_exn |> Label.compare_from_tuple

  let%test "compare 1" = (compare (of_string "33332") (of_string "2AAAA")) = 1
  let%test "compare 2" = (compare (of_string "77888") (of_string "77788")) = -1
end

module Game = struct
  type t = (Hand.t * int) list

  let of_string_list ?(with_jocker=false) ls  = 
    let parse_line l =
      match String.split ~on:' ' l with
      | [h; b] -> Hand.of_string ~with_jocker h, Int.of_string b
      | _ -> invalid_arg "Invalid play"
    in
    List.map ~f:parse_line ls

  let sort_by_rank g = g
    |> List.sort ~compare:(fun a b -> (Hand.compare (fst a) (fst b)))

  let winnings g = g
    |> sort_by_rank
    |> List.foldi ~init:0 ~f:(fun i acc p -> (snd p) * (i+1) + acc)
end


let part1_answer input = input
  |> Game.of_string_list
  |> Game.winnings
  |> Int.to_string
  |> Stdio.print_endline

let part2_answer input = input
  |> Game.of_string_list ~with_jocker:true
  |> Game.winnings
  |> Int.to_string
  |> Stdio.print_endline
