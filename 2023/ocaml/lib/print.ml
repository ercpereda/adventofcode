let listof_strs strs =
  let open Stdlib.Format in
  printf "%a \n"
    (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "; ") pp_print_string)
    strs

let listof_chars chars =
  let open Stdlib.Format in
  printf "%a \n"
    (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "; ") pp_print_char)
    chars

let listof_ints ints =
  let open Stdlib.Format in
  printf "%a \n"
    (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "; ") pp_print_int)
    ints
