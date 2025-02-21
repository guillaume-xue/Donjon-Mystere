open Types
open Json_conversions
open Automate_1
open Settings_map
open Prim_2

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
      let tile = { x; y; texture_id } in
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
let print_grid tiles =
  let grid = Array.make_matrix map_size_x map_size_y '.' in
  List.iter (fun tile ->
    grid.(tile.x).(tile.y) <- if tile.texture_id = 0 then '.' else Char.chr (tile.texture_id + Char.code '0')
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

(* Main *)
let generation_map () =
  Random.self_init ();

  (* Trois en un, init -> auto cell -> supp petite zone *)
  let tiles_tmp = remove_small_zones (regles_auto_cell (init_map ()) iterations) in
  (* Récupération des zones distinctes *)
  let regions_tmp = get_all_zones tiles_tmp in
  (* Connctions des zones avec l'algo de Prim *)
  let tiles_with_paths = connect_zones tiles_tmp regions_tmp in

  (* Création de la map *)
  let map = {
    width = map_size_x;
    height = map_size_y;
    tiles = tiles_with_paths;
    regions = regions_tmp;
  } in

  (* Affichage préliminaire avec l'automate cellulaire *)
  print_grid tiles_tmp;
  Printf.printf "\n\n";
  
  (* Affichage final *)
  print_grid tiles_with_paths;

  (* Sérialisation en JSON *)
  let json = map_to_yojson map in

  (* Écriture dans un fichier *)
  write_json_to_file "resources/map/map.json" json
