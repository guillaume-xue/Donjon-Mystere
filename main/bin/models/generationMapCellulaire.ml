(* TODO: Ajouter un menu option pour une meilleur interaction *)

open Utils.Types

(** 
  [tile_to_yojson] convertit une tuile en une représentation JSON.

  @param tile La tuile à convertir, qui est un enregistrement avec les champs suivants :
  - [x] : la position x de la tuile (entier)
  - [y] : la position y de la tuile (entier) 
  - [texture_id] : l'identifiant de la texture associée à la tuile (entier).

  @return Une valeur JSON de type [`Assoc] représentant la tuile, avec les clés suivantes :
  - ["x"] : la position x de la tuile.
  - ["y"] : la position y de la tuile.
  - ["texture_id"] : l'identifiant de la texture associée à la tuile.
*)
let tile_to_yojson (tile : tile) =
  `Assoc [
    ("x", `Int tile.x);
    ("y", `Int tile.y);
    ("texture_id", `Int tile.texture_id)
  ]

(** 
  [map_to_yojson] convertit une carte en une représentation JSON.

  @param map La carte à convertir, qui est un enregistrement avec les champs suivants :
    - [width] : la largeur de la carte (entier).
    - [height] : la hauteur de la carte (entier).
    - [tiles] : une liste de tuiles de la carte.

  @return Une valeur JSON de type [`Assoc] représentant la carte, avec les clés suivantes :
    - ["width"] : la largeur de la carte.
    - ["height"] : la hauteur de la carte.
    - ["tiles"] : une liste de tuiles converties en JSON.
*)
let map_to_yojson (map: map) =
  `Assoc [
    ("width", `Int map.width);
    ("height", `Int map.height);
    ("tiles", `List (List.map tile_to_yojson map.tiles))
  ]

(** 
  [write_json_to_file] écrit le JSON [json] dans un fichier nommé [filename].
  
  @param filename Le nom du fichier dans lequel écrire le JSON.
  @param json Le JSON à écrire dans le fichier.
*)
let write_json_to_file filename json =
  let oc = open_out filename in
  Yojson.Safe.pretty_to_channel oc json;
  close_out oc

let taille_terrain_x = 50 (* Taille du terrain X *)
let taille_terrain_y = 50 (* Taille du terrain Y *)
let densite = 0.45 (* Proportion initiale de cellules vivantes *)
let iterations = 5 (* Nombre d'itérations de l'automate cellulaire *)
let taille_min_terrain = 20 (* Taille minimale pour garder une zone *)

(** 
  [initialisation_terrain] crée une liste de tuiles pour un terrain de dimensions [taille_terrain_x] par [taille_terrain_y]. 
  
  Chaque tuile est initialisée avec une position (x, y) et un identifiant de texture 0 ou 1 basé sur la [densite].

  @return Une liste de tuiles représentant le terrain initialisé.
*)
let initialisation_terrain () =
  let rec init_tiles x y acc =
    if x >= taille_terrain_x then acc
    else if y >= taille_terrain_y then init_tiles (x + 1) 0 acc
    else
      let texture_id = if Random.float 1.0 < densite then 1 else 0 in
      let tile = { x; y; texture_id } in
      init_tiles x (y + 1) (tile :: acc)
  in
  init_tiles 0 0 []

(** 
  [nb_voisins_vivant] calcule le nombre de voisins vivants pour une cellule située aux coordonnées (x, y) dans une grille de tuiles.

  @param tiles une liste de tuiles représentant la grille. Chaque tuile est supposée avoir des champs [x], [y] pour ses coordonnées et [texture_id] pour son état (vivant ou mort).
  @param x la coordonnée x de la cellule pour laquelle on veut compter les voisins vivants.
  @param y la coordonnée y de la cellule pour laquelle on veut compter les voisins vivants.
  @return le nombre de voisins vivants autour de la cellule située aux coordonnées (x, y). Le nombre de voisins vivants est déterminé en additionnant les [texture_id] des tuiles voisines.
*)
let nb_voisins_vivant tiles x y =
  let directions = [(1, 0); (-1, 0); (0, 1); (0, -1); (1, 1); (-1, -1); (1, -1); (-1, 1)] in
  List.fold_left (fun acc (tile : tile) ->
    if List.exists (fun (dx, dy) -> tile.x = x + dx && tile.y = y + dy) directions then
      acc + tile.texture_id
    else
      acc
  ) 0 tiles

(** 
  [regles_auto_cell] applique les règles de l'automate cellulaire sur une liste de tuiles pour un nombre donné d'itérations.

  Les règles de l'automate cellulaire sont les suivantes :
  - Si la tuile a une texture_id de 1 et a au moins 4 voisins vivants, elle reste vivante (texture_id reste 1), sinon elle meurt (texture_id devient 0).
  - Si la tuile a une texture_id de 0 et a exactement 5 ou 6 voisins vivants, elle devient vivante (texture_id devient 1), sinon elle reste morte (texture_id reste 0).
  - Pour toute autre valeur de texture_id, la tuile devient morte (texture_id devient 0).

  @param tiles La liste des tuiles initiales.
  @param n Le nombre d'itérations à appliquer.
  @return La liste des tuiles après l'application des règles de l'automate cellulaire.
*)
let rec regles_auto_cell tiles n =
  if n = 0 then tiles
  else
    let new_tiles = List.map (fun (tile : tile) ->
      let voisins = nb_voisins_vivant tiles tile.x tile.y in
      let texture_id =
        match tile.texture_id with
        | 1 -> if voisins >= 4 then 1 else 0
        | 0 -> if voisins = 5 || voisins = 6 then 1 else 0
        | _ -> 0
      in
      { tile with texture_id }
    ) tiles in
    regles_auto_cell new_tiles (n - 1)

(** 
  [flood_fill] effectue un algorithme de remplissage par diffusion (flood fill) 
  sur une carte représentée par [tiles], en partant de la position [(x, y)]. 

  L'algorithme utilise une pile pour effectuer une recherche en profondeur (DFS) et explore les quatre directions cardinales 
  (haut, bas, gauche, droite) à partir de chaque tuile. Si une tuile voisine a un identifiant de texture de 1 et n'a pas encore 
  été visitée, elle est ajoutée à la pile pour exploration future.

  @param tiles une liste de tuiles représentant la carte, où chaque tuile a des coordonnées (x, y) et un identifiant de texture.
  @param visited une référence vers une liste de positions déjà visitées.
  @param x la coordonnée x de la position de départ.
  @param y la coordonnée y de la position de départ.
  @return la taille de la zone remplie par diffusion, c'est-à-dire le nombre de tuiles connectées ayant la même texture_id.
*)
let flood_fill tiles visited x y =
  let directions = [(1, 0); (-1, 0); (0, 1); (0, -1)] in
  let rec dfs stack zone_size =
    match stack with
    | [] -> zone_size
    | (cx, cy) :: rest ->
      if not (List.exists (fun (vx, vy) -> vx = cx && vy = cy) !visited) then (
        visited := (cx, cy) :: !visited;
        let new_stack = List.fold_left (fun acc (dx, dy) ->
          let nx, ny = (cx + dx, cy + dy) in
          if List.exists (fun (tile: tile) -> tile.x = nx && tile.y = ny && tile.texture_id = 1) tiles then
            (nx, ny) :: acc
          else
            acc
        ) rest directions in
        dfs new_stack (zone_size + 1)
      ) else
        dfs rest zone_size
  in
  dfs [(x, y)] 0

(** 
  [remove_zone] supprime une zone de tuiles connectées à partir de la position (x, y) 
  en utilisant une recherche en profondeur (DFS). La fonction prend une liste de tuiles [tiles] 
  et les coordonnées [x] et [y] de la tuile de départ. Elle retourne une nouvelle liste de tuiles 
  où la zone connectée a été mise à jour avec des identifiants de texture à 0.

  @param tiles La liste des tuiles à mettre à jour.
  @param x La coordonnée x de la tuile de départ.
  @param y La coordonnée y de la tuile de départ.
  @return Une nouvelle liste de tuiles avec la zone connectée mise à jour.
*)
let remove_zone tiles x y =
  let directions = [(1, 0); (-1, 0); (0, 1); (0, -1)] in
  let rec dfs stack updated_tiles =
    match stack with
    | [] -> updated_tiles
    | (cx, cy) :: rest ->
      let updated_tiles = List.map (fun (tile : tile) ->
        if tile.x = cx && tile.y = cy then { tile with texture_id = 0 } else tile
      ) updated_tiles in
      let new_stack = List.fold_left (fun acc (dx, dy) ->
        let nx, ny = (cx + dx, cy + dy) in
        if List.exists (fun (tile: tile) -> tile.x = nx && tile.y = ny && tile.texture_id = 1) updated_tiles then
          (nx, ny) :: acc
        else
          acc
      ) rest directions in
      dfs new_stack updated_tiles
  in
  dfs [(x, y)] tiles

(** 
  [remove_small_zones] supprime les petites zones de tuiles avec une texture spécifique.

  La fonction [remove_small_zones] parcourt la liste des tuiles et utilise une fonction auxiliaire récursive 
  [remove_small_zones_aux] pour traiter chaque tuile. Si une tuile a une texture spécifique (texture_id = 1) 
  et n'a pas encore été visitée, elle utilise un algorithme de remplissage par diffusion (flood fill) pour 
  déterminer la taille de la zone connectée. Si la taille de la zone est inférieure à une taille minimale 
  définie ([taille_min_terrain]), la zone est supprimée de la liste des tuiles. Sinon, la tuile est conservée 
  et marquée comme visitée. La fonction renvoie la liste des tuiles après suppression des petites zones.

  @param tiles La liste des tuiles à traiter.
  @param visited Une référence à une liste de coordonnées des tuiles déjà visitées.
  @return La liste des tuiles après suppression des petites zones.
*)
let remove_small_zones tiles =
  let rec remove_small_zones_aux tiles visited =
    match tiles with
    | [] -> []
    | tile :: rest ->
      if tile.texture_id = 1 && not (List.exists (fun (vx, vy) -> vx = tile.x && vy = tile.y) !visited) then
        let zone_size = flood_fill tiles visited tile.x tile.y in
        if zone_size < taille_min_terrain then
          remove_small_zones_aux (remove_zone tiles tile.x tile.y) visited
        else
          tile :: remove_small_zones_aux rest (ref ((tile.x, tile.y) :: !visited))
      else
        tile :: remove_small_zones_aux rest visited
  in
  remove_small_zones_aux tiles (ref [])

(** 
  [print_grid tiles] affiche une grille représentant le terrain en utilisant les tuiles fournies.
  
  @param tiles une liste de tuiles, où chaque tuile a des coordonnées (x, y) et un identifiant de texture (texture_id).
  
  La fonction crée une matrice de caractères représentant la grille du terrain. Chaque cellule de la grille est initialisée avec le caractère '.'.
  Ensuite, pour chaque tuile dans la liste, la fonction met à jour la cellule correspondante dans la grille avec '#' si l'identifiant de texture de la tuile est différent de 0, sinon elle reste '.'.
  Enfin, la grille est affichée ligne par ligne, chaque cellule étant séparée par un espace.
*)
let print_grid tiles =
  let grid = Array.make_matrix taille_terrain_x taille_terrain_y '.' in
  List.iter (fun (tile: tile) ->
    grid.(tile.x).(tile.y) <- if tile.texture_id = 0 then '.' else '#'
  ) tiles;
  Array.iter (fun row ->
    Array.iter (fun cell -> print_char cell; print_char ' ') row;
    print_newline ()
  ) grid


(* Main *)
let generation_Map_Cellulaire () =
  Random.self_init ();

  (* Création de la map *)
  let map = {
    width = taille_terrain_x;
    height = taille_terrain_y;
    tiles = remove_small_zones (regles_auto_cell (initialisation_terrain ()) iterations)
    (* Trois en un, init -> auto cell -> supp petite zone *)
  } in

  (* Affichage final *)
  print_grid map.tiles;

  (* Sérialisation en JSON *)
  let json = map_to_yojson map in

  (* Écriture dans un fichier *)
  write_json_to_file "resources/map/map.json" json
