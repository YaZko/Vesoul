
type Ord = Asc | Desc

type state = int array 		(* [1,R] -> left_bound *)


let fit (u: int -> int -> bool)
    (l: int) (c:int) (t: int) (sz:int) : int option =
  if t = 0
  then Some (c-sz)
  else
    try 
      if u l c
      then fit u l (c+1) sz sz
      else fit u l (c+1) (t-1) sz
    with _ -> None

let st = Array.init R (fun _ -> 0)

let is_done = ref true
  
let alloc (u: int -> int-> bool) (server: int)
    (g:int) (l:int) (ord: Ord)  :  int option = (* returns the line where we allocated *)
  (if server = 0
   then if !is_done then raise Not_found
     else is_done := true 
   else ());
  match fit u l st.(l) size.(server) size.(server) with
    None ->
      st.(l) <- S; 
      let (new_ord, next) = next_line l ord in
      alloc u server g next new_ord
  | Some n ->
    st.(l) <- n + size.(server);
    column.(server) <- n;
    line.(server) <- l;
    gr.(server) <- g;
    is_done := false
  

let () =
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

  Array.sort (fun (s1, r1) (s2, r2) -> compare r2 r1) ratio;

  (** Affichage des serveurs par ratio croissant *)
  let () = Array.iter (fun (s, r) -> Printf.printf "%4d %f\n" s r) ratio in

  let gr     := Array.init servers (fun _ -> -1) in
  let line   := Array.init servers (fun _ -> -1) in
  let column := Array.init servers (fun _ -> -1) in
  
  let main (i: int) (g: int) (l: int) (ord: Ord)  =
    let ti = ratio[i] in
    alloc ti g l ord;
    let (new_ord, next) = next_line l ord in
    if i < M then
      main (i+1) ((g+1) mod P) next new_ord 
    else ()      
    (* matchwith *)
    (*   None -> let (new_ord, next) = next_line l ord in *)
    (* 	      main i g next new_ord  *)
    (* | Some (col, st') -> *)

  in
  main 0 0 0 Asc (fun _ -> 0)
  
  )
