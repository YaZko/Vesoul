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

  ()
  
  )
