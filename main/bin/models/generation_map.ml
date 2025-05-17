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
let init_map () : tile list =
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
  
  @param tiles La liste des tuiles à afficher.
  @param x La largeur de la grille.
  @param y La hauteur de la grille.
*)
let print_grid (tiles : tile list) (x : int) (y : int) : unit =
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
let get_zone (tiles : tile list) (visited : (int * int) list ref) (x : int) (y : int) : tile list =
  let directions = [(1, 0); (-1, 0); (0, 1); (0, -1)] in
  let rec dfs (stack : (int * int) list) (zone : tile list) : tile list =
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
let get_all_zones (tiles : tile list) : zone list=
  let visited = ref [] in
  let rec aux (tiles : tile list) (zones : zone list) : zone list =
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
let copy_map_add_marge (tiles : tile list) : tile list = 
  let rec new_map (tiles : tile list) (x : int) (y : int) (acc : tile list) : tile list =
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

(**
  [generation_map floor] génère une carte en trois étapes :
  1. Initialisation de la carte avec des tuiles aléatoires.
  2. Application de l'automate cellulaire pour créer des zones.
  3. Suppression des petites zones.
  4. Ajout de la marge autour de la carte.
  5. Récupération des zones distinctes.
  6. Connexion des zones avec l'algorithme de Prim.
  7. Génération des biomes.
  8. Création de la carte finale.

  @param floor Le numéro de l'étage de la carte.
  @return La carte générée.
*)
let rec generation_map (floor : int) : map  =
  (* Trois en un, init -> auto cell -> supp petite zone *)
  let tiles_tmp1 = remove_small_zones (regles_auto_cell (init_map ()) iterations) in
  (* Ajout de la marge *)
  let tiles_tmp = copy_map_add_marge tiles_tmp1 in
  (* Récupération des zones distinctes *)
  let regions_tmp = get_all_zones tiles_tmp in

  if List.length regions_tmp = 0 then begin
    generation_map floor
  end else begin
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
      music = Some (""); (* Initial music *)
    } in

    (* Affichage final *)
    print_grid tiles_with_biomes (map_size_x+map_marge*2) (map_size_y+map_marge*2);
    map
  end
  
(**
  [create_new_floor floor player] génère un nouvel étage de la carte en utilisant la fonction [generation_map].
  Elle initialise le joueur, les pièges, les ennemis et les objets sur la nouvelle carte.

  @param floor Le numéro de l'étage de la carte.
  @param player Le joueur à placer sur la carte.
  @return Un tuple contenant la carte générée, le joueur, les pièges et le sol, les ennemis et les objets.
*)
let create_new_floor (floor : int) (player : pokemon) : map * pokemon * trap_and_ground list * pokemon list * loot list =
  let map = generation_map floor in
  let (player, zone_rand) = spawn_player_pos map player in
  let trap_and_ground = spawn_list_of_trap_and_ground map zone_rand in
  let (enemys, cpt) = spawn_list_of_enemys map player in
  let player = { player with last_id = cpt } in
  let items = spawn_list_of_loot map in
  (map, player, trap_and_ground, enemys, items)

(**
  [generation_entity map num_pokemon] génère les entités sur la carte, y compris le joueur, les pièges, les ennemis et les objets.

  @param map La carte sur laquelle générer les entités.
  @param num_pokemon Le nombre de Pokémon à générer.
  @return Un tuple contenant la carte, le joueur, les pièges et le sol, les ennemis et les objets.
*)
let generation_entity (map : map) (num_pokemon : int) : map * pokemon * trap_and_ground list * pokemon list * loot list =
  (* Spawning du joueur *)
  let (player, zone_rand) = spawn_player map num_pokemon in
  let trap_and_ground = spawn_list_of_trap_and_ground map zone_rand in
  let (enemys, cpt) = spawn_list_of_enemys map player in
  let player = { player with last_id = cpt } in
  let items = spawn_list_of_loot map in
  (map, player, trap_and_ground, enemys, items)

(**
  [create_map_json filename] génère une carte et l'enregistre au format JSON dans un fichier.

  @param filename Le nom du fichier (sans extension) dans lequel la carte sera enregistrée.
  @param num_pokemon Le nombre de Pokémon à générer sur la carte.
*)
let create_map_json (filename : string) (num_pokemon : int) : unit =
  let map = generation_map 0 in
  (* Génération de la map *)
  let (map, player, trap_and_ground, enemys, items) = generation_entity map num_pokemon in
  (* Sérialisation en JSON *)
  let json = map_player_to_json map player enemys items trap_and_ground in
  (* Écriture dans un fichier *)
  write_json_to_file (map_dir ^ filename ^ ".json") json;


