type ord = Asc | Desc

type state = int array 		(* [1,R] -> left_bound *)

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

  

let foldargmin n f : int =
  let rec aux i res =
    if i = 0 then (if f 0 < f res then 0 else res)
    else (if f i < f res then aux (i-1) i else aux (i-1) res)
  in
  aux n 0

let foldargmax n f : int =
  let rec aux i res =
    if i = 0 then (if f 0 > f res then 0 else res)
    else (if f i > f res then aux (i-1) i else aux (i-1) res)
  in
  aux n 0


    
    
    
let rec foldmin n f: int =
  if n = 0 then f 0
  else min (f n) (foldmin (n - 1) f)

let rec foldsum n f: int =
  if n = 0 then f 0
  else (f n) + (foldsum (n - 1) f)
    

let hurt_capa l gr d m i g : int =
  if (l.(m) = i
     || gr.(m) <> g) then 0
  else d.capa.(m)

(* guar_capa : granted capacity for group g for current allocation (l,gr) *)
let guar_capa l gr d g : int =
  let servers = Array.length d.size in
  foldmin (d.rows -  1)
    (fun i -> (foldsum (servers - 1)
		 (fun m -> hurt_capa l gr d m i g)))

let score_solution (d: data) gr c l : int =
  foldmin (d.groups - 1) (guar_capa l gr d)


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

  let gr_sorted = Array.init d.groups (fun i -> (i,d.capa.(i))) in
  
  let set_undisp l c sz =
    let rec set_undisp c sz =
      if sz = 0 then ()
      else begin d.is_undisp.(l).(c) <- true;
	set_undisp (c+1) (sz-1)
      end
    in
    set_undisp c sz
  in

  let cand_lines g : int array =		(* tableau de lignes *)
    let t = Array.init d.rows (fun r -> (r,capa_cur.(g).(r))) in
    Array.sort (fun (x,xcapa) (y,ycapa) -> compare xcapa ycapa) t;
    Array.map fst t
  in
  
  let update_gr_sorted s =
  (* we know that we allocated server s to the first group of gr_sorted *)
  (* insérer (gr_sorted.(0) + capa(s)) dans gr_sorted[1..groups] *)
    let (g,x) = gr_sorted.(0) in 
    gr_sorted.(0) <- (g , x + d.capa.(s));
    Array.sort (fun (x,xcapa) (y,ycapa) ->
      compare xcapa ycapa) gr_sorted
    (* let rec aux i = *)
    (*   if i >= d.groups || gr_sorted.(i) >= x  *)
    (*   then () *)
    (*   else *)
    (* 	begin *)
    (* 	  gr_sorted.(i-1) <- gr_sorted.(i); *)
    (* 	  gr_sorted.(i) <- x; *)
    (* 	  aux (i+1) *)
    (* 	end *)
    (* in *)
    (* begin *)
    (*   aux 1 *)
    (* end *)
      
  in

  let alloc2 (server: int)
      (g:int)  :  int option = (* returns the line where we allocated *)
    let lc = cand_lines g in
    let rec try_line l =
      if l = d.rows
      then raise Not_found
      else
	match
	  fit lc.(l) 0 d.size.(server)
	with
	  None ->
	    try_line (l+1)
	| Some n ->
	      set_undisp lc.(l) n d.size.(server);
	      column.(server) <- n;
	      line.(server) <- lc.(l);
	      gr.(server) <- g;
	      capa_cur.(g).(lc.(l)) <- capa_cur.(g).(lc.(l)) + d.capa.(server);
	      update_gr_sorted server;
	      Some lc.(l)

    in
    begin try try_line 0 with Not_found -> None end
  in

  let rec main (i: int)  =
    let next_group = fst gr_sorted.(0) in
    let ti = fst d.ratio.(i) in
    begin
      match
	alloc2 ti next_group
      with
      | Some l ->
	begin
	  if i + 1 < servers then
	    main (i+1)
	  else
	    (gr, line, column)
	end
      | None ->
	(gr, line, column)
    end  
  in
  
  main 0 

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
    (* print_undisp is_undisp; *)

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

  (** Solution *)

      let (group , ligne , column) as sol = solution d 0 in

      let score = score_solution d group column ligne in

      Printf.printf "Solution has score %d\n" score;

      (* PW: il faudrait essayer de faire plusieurs permutations de groupes
	 localement non optimisantes, mais vérifier qu'en N pas, on obtient bien
	 une meilleure allocation*)
      let best_score = ref score in
      for iter = 0 to 10 do
	let amax = foldargmax (d.groups - 1) (guar_capa ligne group d) in
	let amin = foldargmin (d.groups - 1) (guar_capa ligne group d) in
      (* donner un serveur de l'argmax à l'argmin*)
	for k = 0 to servers - 1 do
      	(* Printf.printf "server %d\n" k; *)
      	  let new_gr = Array.copy group in
      	  if group.(k) = amax then
      	    begin new_gr.(k) <- amin; 
      		  let score = score_solution d new_gr column ligne in
      		  if score > ! best_score
      		  then begin
      		    group.(k) <- amin;
      		    best_score := score;
      		    Printf.printf "Updated solution has score %d\n" score;
      		    flush stdout
      		  end
      		  else ()
      	    end
      	  else ()
	done
      done
      ;

    (*   Random.self_init (); *)
    (*   let get_server_in_group g group = *)
    (*   	let shuffled_servers = *)
    (*   	  Array.init servers (fun i -> i) in *)
    (*   	Array.sort (fun _ _ -> (Random.int 3) - 1) shuffled_servers; *)
    (*   	let rec gsig m = *)
    (*   	  let s = shuffled_servers.(m) in *)
    (*   	  if m = 0 then None *)
    (*   	  else if group.(s) = g then Some s *)
    (*   	  else gsig (m-1) *)
    (*   	in *)
    (*   	gsig (servers - 1) *)
    (*   in *)

    (*   let best_score = ref score in *)
    (*   let best_group = Array.copy group in *)
      
    (* for k = 0 to 50 do *)
    (* 	let amax = foldargmax (d.groups - 1) (guar_capa ligne group d) in *)
    (* 	let amin = foldargmin (d.groups - 1) (guar_capa ligne group d) in *)
    (* 	begin match get_server_in_group amax group with *)
    (* 	  Some s -> group.(s) <- amin; *)
    (* 	    let score = score_solution d group column ligne in *)
    (* 	    if score > ! best_score *)
    (* 	    then *)
    (* 	      begin *)
    (* 		Array.iteri (fun i e -> best_group.(i) <- e) group; *)
    (* 		best_score := score; *)
    (* 		Printf.printf "Updated solution has score %d\n" score *)
    (* 	      end *)
    (* 	    else *)
    (* 	      begin *)
    (* 		Array.iteri (fun i e -> group.(i) <- e) best_group; *)
    (* 	      end *)
    (* 	| None -> () *)
    (* 	end *)
    (* done *)
    (*   ; *)
      
      (* print_undisp d.is_undisp; *)

  (** Impression de la solution. *)
    let oc = open_out_bin "vesoul" in
    out_solution oc group ligne column;
    close_out oc;

    ()
  )
