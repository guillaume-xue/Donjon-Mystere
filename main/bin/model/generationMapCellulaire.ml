(* TODO: Ajouter un menu option pour une meilleur interaction *)
let taille_terrain = 50 (* Taille du terrain *)
let densite = 0.45 (* Proportion initiale de cellules vivantes *)
let iterations = 5 (* Nombre d'itérations de l'automate cellulaire *)
let taille_min_terrain = 50 (* Taille minimale pour garder une zone *)

(** 
  @return Une matrice carrée de taille [taille_terrain] x [taille_terrain] où chaque cellule est initialisée à 1 avec une probabilité de [densite] et à 0 sinon.
*)
let initialisation_terrain () =
  Array.init taille_terrain (fun _ ->
      Array.init taille_terrain (fun _ ->
          if Random.float 1.0 < densite then 1 else 0))

(** 
  Calcule le nombre de voisins vivants autour d'une cellule dans une grille selon le voisinage de Moore.

  @param grid La grille de cellules, représentée par une matrice 2D d'entiers (0 pour mort, 1 pour vivant).
  @param x La coordonnée x de la cellule centrale.
  @param y La coordonnée y de la cellule centrale.
  @return Le nombre de voisins vivants autour de la cellule (x, y).

  On utilise une référence pour obtenir un compteur mutable tout le long de la fonction.

  La fonction parcourt les cellules voisines dans un carré de 3x3 autour de la cellule centrale (x, y), 
  en ignorant la cellule centrale elle-même. Pour chaque cellule voisine, elle vérifie si elle est 
  dans les limites de la grille et ajoute son état (vivant ou mort) au compteur. Le compteur est 
  ensuite retourné comme résultat.
*)
let nb_voisins_vivant grid x y =
  let cpt = ref 0 in
  for x_bis = -1 to 1 do
    for y_bis = -1 to 1 do
      if x_bis <> 0 || y_bis <> 0 then
        let nx = x + x_bis and ny = y + y_bis in
        if nx >= 0 && nx < taille_terrain && ny >= 0 && ny < taille_terrain then
          cpt := !cpt + grid.(nx).(ny)
    done
  done;
  !cpt

(** 
  Applique les règles de l'automate cellulaire pour [iterations] itérations.

  @param grid La grille de cellules, représentée par une matrice 2D d'entiers (0 pour mort, 1 pour vivant).
  @param n Le nombre d'itérations restantes à effectuer.
  @return Une nouvelle grille représentant l'état de l'automate cellulaire après une itération des règles.

  La fonction suit les règles suivantes :
  - Si une cellule est vivante (1) et a 4 voisins vivants ou plus, elle reste vivante.
  - Si une cellule est morte (0) et a exactement 5 ou 6 voisins vivants, elle devient vivante.
  - Dans tous les autres cas, la cellule reste ou devient morte (0).

  @param taille_terrain La taille de la grille (supposée carrée).
  @param nb_voisins_vivant Fonction qui calcule le nombre de voisins vivants d'une cellule donnée.
*)
let rec regles_auto_cell grid n =
  if n = 0 then grid
  else
    let new_grid =
      Array.init taille_terrain (fun x ->
          Array.init taille_terrain (fun y ->
              let voisins = nb_voisins_vivant grid x y in
              match grid.(x).(y) with
              | 1 -> if voisins >= 4 then 1 else 0
              | 0 -> if voisins = 5 || voisins = 6 then 1 else 0
              | _ -> 0))
    in
    regles_auto_cell new_grid (n - 1)


(** 
  Flood-fill pour compter la taille d'une zone.

  @param grid La grille de cellules, représentée par une matrice 2D d'entiers (0 pour mort, 1 pour vivant).
  @param visited Une matrice 2D de booléens pour suivre les cellules déjà visitées.
  @param x La coordonnée x de la cellule de départ.
  @param y La coordonnée y de la cellule de départ.
  @return La taille de la zone connectée de cellules vivantes à partir de (x, y).

  La fonction utilise une approche de recherche en profondeur (DFS) pour explorer toutes les cellules connectées à partir de (x, y).
*)
let flood_fill grid visited x y =
  let directions = [|(1, 0); (-1, 0); (0, 1); (0, -1)|] in
  let rec dfs stack zone_size =
    match stack with
    | [] -> zone_size
    (* 
    Pour chaque cellule présente dans la liste, on détecte ses voisins "haut,bas,gauche,droite"
    et on l'ajoute au début de la liste s'il est vivant et pas encore visité, 
    puis on continue la recherche avec la prochaine case de la liste tout en incrémentant.
    *)
    | (cx, cy) :: rest ->
        visited.(cx).(cy) <- true;
        let new_stack =
          Array.fold_left
            (fun acc (x_bis, y_bis) ->
              let nx, ny = (cx + x_bis, cy + y_bis) in
              if nx >= 0 && nx < taille_terrain && ny >= 0 && ny < taille_terrain
                 && not visited.(nx).(ny)
                 && grid.(nx).(ny) = 1
                then (nx, ny) :: acc
              else acc)
            rest directions
        in
        dfs new_stack (zone_size + 1)
  in
  dfs [(x, y)] 0


(** 
  Supprime une zone en la remplaçant par des cellules mortes.

  @param grid La grille de cellules, représentée par une matrice 2D d'entiers (0 pour mort, 1 pour vivant).
  @param x La coordonnée x de la cellule de départ.
  @param y La coordonnée y de la cellule de départ.

  La fonction utilise une approche de recherche en profondeur (DFS) pour explorer toutes les cellules connectées à partir de (x, y) et les remplace par des cellules mortes (0).
*)
let remove_zone grid x y =
  let directions = [|(1, 0); (-1, 0); (0, 1); (0, -1)|] in
  let rec dfs stack =
    match stack with
    | [] -> ()
    | (cx, cy) :: rest ->
        grid.(cx).(cy) <- 0;
        let new_stack =
          Array.fold_left
            (fun acc (x_bis, y_bis) ->
              let nx, ny = (cx + x_bis, cy + y_bis) in
              if nx >= 0 && nx < taille_terrain && ny >= 0 && ny < taille_terrain
                 && grid.(nx).(ny) = 1
              then (nx, ny) :: acc
              else acc)
            rest directions
        in
        dfs new_stack
  in
  dfs [(x, y)]

(** 
  Supprime les zones trop petites, règle définit par [taille_min_terrain].

  @param grid La grille de cellules, représentée par une matrice 2D d'entiers (0 pour mort, 1 pour vivant).

  La fonction parcourt toute la grille, 
  utilise la fonction flood_fill pour déterminer la taille de chaque zone connectée de cellules vivantes, 
  et supprime les zones dont la taille est inférieure à une taille minimale spécifiée par [taille_min_terrain].
*)
let remove_small_zones grid =
  let visited = Array.make_matrix taille_terrain taille_terrain false in
  for x = 0 to taille_terrain - 1 do
    for y = 0 to taille_terrain - 1 do
      if grid.(x).(y) = 1 && not visited.(x).(y) then
        let zone_size = flood_fill grid visited x y in
        if zone_size < taille_min_terrain then remove_zone grid x y
    done
  done

(** 
  Affiche la grille de cellules donnée en paramètre.

  @param grid La grille de cellules, représentée par une matrice 2D d'entiers (0 pour mort, 1 pour vivant).

  La fonction parcourt chaque ligne de la grille et affiche chaque cellule sous forme de caractère ('#' pour vivant, '.' pour mort).
*)
let print_grid grid =
  Array.iter
    (fun row ->
      Array.iter (fun cell -> print_char (if cell = 1 then '#' else '.')) row;
      print_newline ())
    grid

(* Main *)
let generation_Map_Cellulaire () =
  Random.self_init ();
  let grid = initialisation_terrain () in

  (* Automate cellulaire *)
  let final_grid = regles_auto_cell grid iterations in

  (* Suppression des petites zones *)
  remove_small_zones final_grid;

  (* Affichage final *)
  print_grid final_grid;
