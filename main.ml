let print_undisp =
  Array.iter (fun row ->
    Array.iter (fun b -> print_char (if b then 'X' else '.')) row; print_newline ())

type data = {
  cols: int;
  rows : int;
  groups: int;
  is_undisp : bool array array;
  size : int array;
  capa : int array;
  ratio: (int * float) array;
}


let score_solution (d: data) gr c l : int =

  let servers = Array.length d.size in

  let rec foldmin n f: int =
    if n = 0 then f 0
    else min (f n) (foldmin (n - 1) f)
  in

  let rec foldsum n f: int =
    if n = 0 then f 0
    else (f n) + (foldsum (n - 1) f)
  in

  let guar_capa m i g : int =
    if (l.(m) = i
    || gr.(m) <> g) then 0
    else d.capa.(m)
  in

  foldmin (d.groups - 1) 
	  (fun g -> (foldmin (d.rows -  1) 
			     (fun i -> (foldsum (servers - 1) 
						(fun m -> guar_capa m i g)))))

let solution (d: data) (startline: int) =

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
    (*   Printf.printf "fit: %d %d %d\n" l c sz; *)
    fit c sz
  in


  let gr     = Array.init servers (fun _ -> -1) in
  let line   = Array.init servers (fun _ -> -1) in
  let column = Array.init servers (fun _ -> -1) in
  let capa_cur = Array.init d.groups
			    (fun _ -> Array.init d.rows
						 (fun _ -> 0)) in

  let set_undisp l c sz =
    let rec set_undisp c sz =
      if sz = 0 then ()
      else begin d.is_undisp.(l).(c) <- true;
		 set_undisp (c+1) (sz-1)
	   end
    in
    set_undisp c sz
  in

  let cand_lines g : int array =        (* tableau de lignes *)
    let t = Array.init d.rows (fun r -> (r,capa_cur.(g).(r))) in
    Array.sort (fun (x,xcapa) (y,ycapa) -> compare xcapa ycapa) t;
    Array.map fst t
  in

  let alloc (server: int) (g:int) : int option = (* returns the line where we allocated *)
    let lc = cand_lines g in
    let rec try_line i =
      if i = d.rows
      then raise Not_found
      else
	let l = lc.(i) in
	match fit l 0 d.size.(server) with
	  None ->
	  try_line (i+1)
	| Some n ->
	   set_undisp l n d.size.(server);
	   column.(server) <- n;
	   line.(server) <- l;
	   gr.(server) <- g;
	   capa_cur.(g).(l) <- capa_cur.(g).(l) + d.capa.(server);
	   Some l
    in
    begin try try_line 0 with Not_found -> None end

  in


  let rec main (i: int) (g: int)  =
    let ti = fst d.ratio.(i) in
    begin match alloc ti g with
	  | Some l ->
	     begin
	       if i + 1 < servers then
		 main (i+1) ((g+1) mod d.groups)
	       else
		 (gr, line, column)
	     end
	  | None ->
	     (gr, line, column)
    end
  in
  main 0 0

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
  print_undisp is_undisp;

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
    capa = Array.map snd data;
  } in

  let is_undisp_save = Array.map Array.copy is_undisp in

  let best_score = ref 0 in
  let best_sol = ref ([||],[||],[||]) in
  let best_undisp = ref [||] in

  (** Solution *)
  for k = 0 to rows - 1  do
    Array.iteri (fun i a -> d.is_undisp.(i) <- Array.copy a) is_undisp_save;
    let (group , ligne , column) as sol = solution d k in

(*     print_undisp d.is_undisp; *)
    let score = score_solution d group column ligne in
    if score >= ! best_score
    then begin
      best_score := score ;
      best_sol := sol ;
      best_undisp := Array.map Array.copy d.is_undisp
    end
    else ();
    Printf.printf "Solution %d has score %d\n" k score
  done
  ;

  print_undisp ! best_undisp;

  (** Impression de la solution. *)
  let oc = open_out_bin "vesoul" in
  let (group , ligne , column) = ! best_sol in
  out_solution oc group ligne column;
  close_out oc;

  ()
  )

