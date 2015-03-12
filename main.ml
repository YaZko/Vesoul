type ord = Asc | Desc

type state = int array 		(* [1,R] -> left_bound *)

type data = {
  cols: int;
  rows : int;
  groups: int;
  is_undisp : bool array array;
  size : int array;
  ratio: (int * float) array;
}

let solution (d: data) =

let servers = Array.length d.size in

let fit
    (l: int) (c:int) (sz:int) : int option =
  let rec fit c t =
  if t = 0
  then Some (c - sz)
  else
    try 
      if d.is_undisp.(l).(c)
      then fit (c+1) sz
      else fit (c+1) (t-1)
    with _ -> None
  in
  fit c sz
in

let st = Array.init d.rows (fun _ -> 0)

in

let next_line l ord =
  match ord with
    Asc -> let l2 = l + 1 in
	   if l2 < d.rows then (Asc, l2)
	   else (Desc, l-1)
  | Desc ->
    if l = 0
    then (Asc,1)
    else (Desc,l-1)
in
  
  let gr     = Array.init servers (fun _ -> -1) in
  let line   = Array.init servers (fun _ -> -1) in
  let column = Array.init servers (fun _ -> -1) in
  
let alloc (server: int)
    (g:int) (l:int) (ord: ord)  :  int option = (* returns the line where we allocated *)

  let is_done = ref false in

      let rec alloc l ord =
  (if l = 0
   then if !is_done then raise Not_found
     else is_done := true 
   else ());
  match fit l st.(l) d.size.(server) with
    None ->
      st.(l) <- d.cols;
      let (new_ord, next) = next_line l ord in
      alloc next new_ord
  | Some n ->
    st.(l) <- n + d.size.(server);
    column.(server) <- n;
    line.(server) <- l;
    gr.(server) <- g;
    is_done := false;
    Some (l)
   in
   begin try alloc l ord with Not_found -> None end

in
  
  let rec main (i: int) (g: int) (l: int) (ord: ord)  =
    let ti = fst d.ratio.(i) in
    begin match alloc ti g l ord with
    | Some l ->
      begin
    let (new_ord, next) = next_line l ord in
    if i < servers then
      main (i+1) ((g+1) mod d.groups) next new_ord 
    else
      (gr, line, column)
      end
    | None ->
      (gr, line, column)
    end
    (* matchwith *)
    (*   None -> let (new_ord, next) = next_line l ord in *)
    (* 	      main i g next new_ord  *)
    (* | Some (col, st') -> *)

  in
  main 0 0 0 Asc

let out_solution (oc: out_channel) (group: int array) (ligne: int array) (column: int array) : unit =
  Array.iteri (fun s g ->
    if g < 0
    then Printf.fprintf oc "x\n"
    else Printf.fprintf oc "%d %d %d\n" ligne.(s) column.(s) g
  ) group

let () =
  let stdin = try open_in Sys.argv.(1) with _ -> stdin in
  (* Nombre de rangées *)
  (* Nombre d’emplacements *)
  (* Nombre d’emplacements indisponibles *)
  (* Nombre de groupes *)
  (* Nombre de serveurs *)
  Scanf.fscanf stdin "%d %d %d %d %d\n" (fun rows cols undisp groups servers ->
  (* Tableau des cases indisponibles idx -> row × col *)
  let undisp = Array.init undisp (fun _ ->
    Scanf.fscanf stdin "%d %d\n" (fun ri ci -> (ri, ci))
  ) in
  (* Tableau des serveurs à allouer idx -> size × capa *)
  let data = Array.init servers (fun _ ->
    Scanf.fscanf stdin "%d %d\n" (fun zi ci -> (zi, ci))
  ) in
  (** Tableau des cases indisponibles *)
  let is_undisp = Array.init rows (fun r ->
    Array.init cols (fun c -> false
    )) in
  Array.iter (fun (r, c) -> is_undisp.(r).(c) <- true) undisp;

  (** Affichage de la grille initiale *)
  let () = Array.iter (fun row ->
    Array.iter (fun b -> print_char (if b then 'X' else '.')) row; print_newline ())
    is_undisp
  in

  (** Ratio capa/taille par serveur *)
  let ratio = Array.init servers (fun s ->
    let (z,c) = data.(s) in
    (s, float_of_int c /. float_of_int z)
  ) in

  Array.sort (fun (s1, r1) (s2, r2) -> compare (r2, - fst data.(s2)) (r1, - fst data.(s1))) ratio;

  (** Affichage des serveurs par ratio croissant
  let () = Array.iter (fun (s, r) -> Printf.printf "%4d %2.0f %2d\n" s r (fst data.(s))) ratio in
  *)

  let d : data = {
    cols = cols;
    rows = rows;
    groups = groups;
    ratio = ratio;
    is_undisp = is_undisp;
    size = Array.map fst data;
  } in

  (** Solution *)
  let (group , ligne , column) = solution d in

  (** Impression de la solution. *)
  let oc = open_out_bin "vesoul" in
  out_solution oc group ligne column;
  close_out oc;

  ()
  )
