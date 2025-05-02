open Utils.Types
open Utils.Json_conversions
open Automate_1
open Utils.Settings_map
open Prim_2
open Biomes_3
open Entity_gen

(** 
  [init_map] crée une liste de tuiles pour un terrain de dimensions [map_size_x] par [map_size_y]. 
  
  Chaque tuile est initialisée avec une position (x, y) et un identifiant de texture 0 ou 1 basé sur la [density].

  @return Une liste de tuiles représentant le terrain initialisé.
*)
let init_map () =
  let rec init_tiles x y acc =
    if x >= map_size_x then acc
    else if y >= map_size_y then init_tiles (x + 1) 0 acc
    else
      let texture_id = if Random.float 1.0 < density then 1 else 0 in
      let tile = { x; y; texture_id; biome_id = 0 } in
      init_tiles x (y + 1) (tile :: acc)
  in
  init_tiles 0 0 []

(** 
  [print_grid] affiche une grille représentant le terrain en utilisant les tuiles fournies.
  
  @param tiles une liste de tuiles, où chaque tuile a des coordonnées (x, y) et un identifiant de texture (texture_id).
  
  La fonction crée une matrice de caractères représentant la grille du terrain. Chaque cellule de la grille est initialisée avec le caractère '.'.
  Ensuite, pour chaque tuile dans la liste, la fonction met à jour la case correspondante avec sa valeur de texture.
  Enfin, la grille est affichée ligne par ligne, chaque cellule étant séparée par un espace.
*)
let print_grid tiles x y =
  let grid = Array.make_matrix x y '.' in
  List.iter (fun tile ->
    grid.(tile.x).(tile.y) <- if tile.texture_id = 0 then '.' else Char.chr (tile.biome_id + Char.code '0')
  ) tiles;
  Array.iter (fun row ->
    Array.iter (fun cell -> print_char cell; print_char ' ') row;
    print_newline ()
  ) grid

(** 
  [get_zone] récupère une zone de tuiles connectées à partir de la position  d'une tuile donnée ([x], [y]) 
  en utilisant une recherche en profondeur (DFS). 

  @param tiles La liste des tuiles à explorer.
  @param visited Une référence à une liste de coordonnées des tuiles déjà visitées.
  @param x La coordonnée x de la tuile de départ.
  @param y La coordonnée y de la tuile de départ.
  @return Une liste de tuiles représentant la zone connectée.
*)
let get_zone tiles visited x y =
  let directions = [(1, 0); (-1, 0); (0, 1); (0, -1)] in
  let rec dfs stack zone =
    match stack with
    | [] -> zone
    | (cx, cy) :: rest ->
      if not (List.exists (fun (vx, vy) -> vx = cx && vy = cy) !visited) then (
        visited := (cx, cy) :: !visited;
        let new_zone = List.filter (fun tile -> tile.x = cx && tile.y = cy) tiles @ zone in
        let new_stack = List.fold_left (fun acc (dx, dy) ->
          let nx, ny = (cx + dx, cy + dy) in
          if List.exists (fun tile -> tile.x = nx && tile.y = ny && tile.texture_id <> 0) tiles then
            (nx, ny) :: acc
          else
            acc
        ) rest directions in
        dfs new_stack new_zone
      ) else
        dfs rest zone
  in
  dfs [(x, y)] []

(** 
  [get_all_zones] récupère toutes les zones distinctes de tuiles ayant une texture_id différente de 0.

  @param tiles La liste des tuiles à explorer.
  @return Une liste de zones.
*)
let get_all_zones tiles =
  let visited = ref [] in
  let rec aux tiles zones =
    match tiles with
    | [] -> zones
    | tile :: rest ->
      if tile.texture_id <> 0 && not (List.exists (fun (vx, vy) -> vx = tile.x && vy = tile.y) !visited) then
        let zone_tiles = get_zone tiles visited tile.x tile.y in
        aux rest ({ id = List.length zones; size = List.length zone_tiles; tiles = zone_tiles } :: zones)
      else
        aux rest zones
  in
  aux tiles []

(** 
  [copy_map_add_marge tiles] crée une nouvelle carte en ajoutant une marge autour de la carte existante.

  @param tiles La liste des tuiles de la carte originale.
  @return Une nouvelle liste de tuiles représentant la carte avec une marge ajoutée.
*)
let copy_map_add_marge tiles = 
  let rec new_map tiles x y acc =
    if x >= map_size_x + map_marge * 2 then acc
    else if y >= map_size_y + map_marge * 2 then new_map tiles (x + 1) 0 acc
    else
      if x - map_marge >= 0 && x - map_marge < map_size_x && y - map_marge >= 0 && y - map_marge < map_size_y then
        match tiles with
        | [] -> new_map tiles x (y + 1) acc
        | tile :: rest ->
          let new_tile = { tile with x = x; y = y} in
          new_map rest x (y + 1) (new_tile :: acc)
      else
        let new_tile = { x; y; texture_id = 0; biome_id = 0 } in
        new_map tiles x (y + 1) (new_tile :: acc)
  in
  new_map tiles 0 0 []

(* Main *)
let generation_map floor =
  (* Trois en un, init -> auto cell -> supp petite zone *)
  let tiles_tmp1 = remove_small_zones (regles_auto_cell (init_map ()) iterations) in
  (* Ajout de la marge *)
  let tiles_tmp = copy_map_add_marge tiles_tmp1 in
  (* Récupération des zones distinctes *)
  let regions_tmp = get_all_zones tiles_tmp in
  (* Connctions des zones avec l'algo de Prim *)
  let tiles_with_paths = connect_zones tiles_tmp regions_tmp in
  (* Génération des biomes *)
  let tiles_with_biomes = generate_biomes tiles_with_paths regions_tmp () in

  (* Création de la map *)
  let map = {
    width = map_size_x + map_marge * 2;
    height = map_size_y + map_marge * 2;
    tiles = tiles_with_biomes;
    regions = regions_tmp;
    floor = floor; (* Initial floor *)
  } in

  (* Spawning du joueur *)
  let (player, zone_rand) = spawn_player map in
  let trap_and_ground = spawn_list_of_trap_and_ground map zone_rand in
  let (enemys, cpt) = spawn_list_of_enemys map player in
  let player = { player with last_id = cpt } in
  let items = spawn_list_of_loot map in

  (* Affichage préliminaire avec l'automate cellulaire *)
  (* print_grid tiles_tmp (map_size_x+map_marge*2) (map_size_y+map_marge*2); *)
  
  (* Affichage final *)
  print_grid tiles_with_biomes (map_size_x+map_marge*2) (map_size_y+map_marge*2);
  (map, player, trap_and_ground, enemys, items)

(**
  [create_map_json filename] génère une carte et l'enregistre au format JSON dans un fichier.

  @param filename Le nom du fichier (sans extension) dans lequel la carte sera enregistrée.
*)
let create_map_json filename =
  (* Génération de la map *)
  let (map, player, trap_and_ground, enemys, items) = generation_map 0 in
  (* Sérialisation en JSON *)
  let json = map_player_to_json map player enemys items trap_and_ground in
  (* Écriture dans un fichier *)
  write_json_to_file (map_dir ^ filename ^ ".json") json;