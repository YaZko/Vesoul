
let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []

let print_file name =
  let sl = read_lines name in
  List.iter (fun x -> Printf.printf "%s\n" x) sl
    
let () =
  print_file "main.ml"
