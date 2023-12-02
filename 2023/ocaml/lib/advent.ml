open Base

let read_lines file_name =
  In_channel.with_open_text file_name In_channel.input_all |> String.split_lines

let print_listof_strs strs =
  let open Stdlib.Format in
  printf "%a \n"
    (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "; ") pp_print_string)
    strs

let print_listof_chars chars =
  let open Stdlib.Format in
  printf "%a \n"
    (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "; ") pp_print_char)
    chars

let print_listof_ints ints =
  let open Stdlib.Format in
  printf "%a \n"
    (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "; ") pp_print_int)
    ints
