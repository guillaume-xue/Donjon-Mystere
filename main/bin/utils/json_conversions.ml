open Types
open Yojson.Basic
open Yojson.Basic.Util

(**
  [load_map_player_from_json filename] loads a map and a player from a combined JSON file.
  @param filename The name of the file to load.
  @return A tuple containing the map and the player loaded from the file.
*)
let load_map_player_from_json (filename: string): (map * player) =
  let json = from_file filename in
  let map_json = json |> member "map" in
  let player_json = json |> member "player" in

  let map = {
    width = map_json |> member "width" |> to_int;
    height = map_json |> member "height" |> to_int;
    tiles = map_json |> member "tiles" |> to_list |> List.map (fun tile ->
      {
        x = tile |> member "x" |> to_int;
        y = tile |> member "y" |> to_int;
        texture_id = tile |> member "texture_id" |> to_int;
        biome_id = tile |> member "biome_id" |> to_int;
      }
    );
    regions = []
  } in

  let player = {
    pos_x = player_json |> member "pos_x" |> to_float;
    pos_y = player_json |> member "pos_y" |> to_float;
    screen_x = player_json |> member "screen_x" |> to_int;
    screen_y = player_json |> member "screen_y" |> to_int;
    player_textures_id = 0;
    target_x = player_json |> member "target_x" |> to_float;
    target_y = player_json |> member "target_y" |> to_float;
    moving = false;
    state = Idle;
    direction = Down;
    current_hp = player_json |> member "current_hp" |> to_int;
    max_hp = player_json |> member "max_hp" |> to_int;
    level = player_json |> member "level" |> to_int;
    current_xp = player_json |> member "current_xp" |> to_int;
    max_xp = player_json |> member "max_xp" |> to_int;
  } in

  (map, player)

(** 
  [tile_to_yojson] convertit une tuile en une représentation JSON.

  @param tile La tuile à convertir.

  @return Une valeur JSON de type [`Assoc] représentant la tuile, avec les clés suivantes :
  - ["x"] : la position x de la tuile.
  - ["y"] : la position y de la tuile.
  - ["texture_id"] : l'identifiant de la texture associée à la tuile.
  - ["biome_id"] : l'identifiant du biome associé à la tuile.
*)
let tile_to_yojson tile =
  `Assoc [
    ("x", `Int tile.x);
    ("y", `Int tile.y);
    ("texture_id", `Int tile.texture_id);
    ("biome_id", `Int tile.biome_id)
  ]

(** 
  [map_to_yojson] convertit une carte en une représentation JSON.

  @param map La carte à convertir.

  @return Une valeur JSON de type [`Assoc] représentant la carte, avec les clés suivantes :
    - ["width"] : la largeur de la carte.
    - ["height"] : la hauteur de la carte.
    - ["tiles"] : une liste de tuiles converties en JSON.
*)
let map_to_yojson map =
  `Assoc [
    ("width", `Int map.width);
    ("height", `Int map.height);
    ("tiles", `List (List.map tile_to_yojson map.tiles))
  ]

(**
  [layer_to_yojson filename player] saves a player to a JSON file.
  @param filename The name of the file to save.
  @param player The player to save.
*)
let player_to_yojson (player: player) =
  `Assoc [
    ("pos_x", `Float player.pos_x);
    ("pos_y", `Float player.pos_y);
    ("screen_x", `Int player.screen_x);
    ("screen_y", `Int player.screen_y);
    ("player_textures_id", `Int player.player_textures_id);
    ("target_x", `Float player.target_x);
    ("target_y", `Float player.target_y);
    ("current_hp", `Int player.current_hp);
    ("max_hp", `Int player.max_hp);
    ("level", `Int player.level);
    ("current_xp", `Int player.current_xp);
    ("max_xp", `Int player.max_xp);
  ]

(**
  [map_player_to_json filename map player] saves a map and a player to a combined JSON file.
  @param filename The name of the file to save.
  @param map The map to save.
  @param player The player to save.
*)
let map_player_to_json (map: map) (player: player) =
  `Assoc [
    ("map", map_to_yojson map);
    ("player", player_to_yojson player)
  ]

(** 
  [write_json_to_file] écrit le JSON [json] dans un fichier nommé [filename].
  
  @param filename Le nom du fichier dans lequel écrire le JSON.
  @param json Le JSON à écrire dans le fichier.
*)
let write_json_to_file filename json =
  let oc = open_out_gen [Open_creat; Open_trunc; Open_wronly] 0o666 filename in
  Yojson.Basic.pretty_to_channel oc json;
  close_out oc

(** 
  [create_empty_json_file filename] crée un fichier JSON vide s'il n'existe pas.

  @param filename Le nom du fichier.
*)
let create_empty_json_file filename =
  if not (Sys.file_exists filename) then
    let oc = open_out_gen [Open_creat; Open_trunc; Open_wronly] 0o666 filename in
    output_string oc "";
    close_out oc

(**
  [read_json_files_in_directory dir_path] lit tous les noms de fichiers .json dans un dossier.
  @param dir_path Le chemin du dossier.
  @return Une liste de noms de fichiers .json.
*)
let read_json_files_in_directory dir_path =
  let files = Sys.readdir dir_path in
  Array.to_list files
  |> List.filter (fun file -> Filename.check_suffix file ".json")
  |> List.map Filename.chop_extension