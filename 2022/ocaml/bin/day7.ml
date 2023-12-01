open Base

module FileSystem = struct
  type t = Empty | File of file | Folder of folder
  and file = { name : string; size : int }
  and folder = { name : string; size : int option; children : t list }

  let create_folder name = Folder { name; size = None; children = [] }
  let create_file name size = File { name; size }

  let add_folder fs folder_name = 
    match fs with 
    | Empty -> create_folder folder_name
    | Folder fol -> Folder { fol with children = (create_folder folder_name) :: fol.children}
    | File _ -> failwith "Can't add a folder to a file" 

  let parse_line fs line =
    match String.split ~on:' ' line with
    | ["$"; "cd"; name ] -> add_folder fs name
    | ["$"; "ls"] -> fs
    | _ -> failwith "something"

  let of_string_list input =
    let rec _of_string_list fs input =
      match input with
      | [] -> fs
      | line :: rest -> (
          match String.split line ~on:' ' with
          | [ "$"; "cd"; name ] -> (
            match fs with 
            | Empty -> _of_string_list (create_folder name) rest 
            | _of_string_list ({fs with children = (create_folder name) :: fs.children }) rest)
          | [ "$"; "ls" ] -> _of_string_list fs rest)
    in
    _of_string_list Empty input
end
