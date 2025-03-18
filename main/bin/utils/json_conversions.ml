open Types
open Yojson.Basic
open Yojson.Basic.Util

(**
  [load_map_player_from_json filename] loads a map and a player from a combined JSON file.
  @param filename The name of the file to load.
  @return A tuple containing the map and the player loaded from the file.
*)
let load_map_player_from_json (filename: string): (map * pokemon * pokemon list) =
  let json = from_file filename in
  let map_json = json |> member "map" in
  let player_json = json |> member "player" in
  let enemy_json = json |> member "enemy" |> to_list in

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
    entity_textures_id = 0;
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

  let enemy = enemy_json |> List.map (fun enemy_json ->
    {
      pos_x = enemy_json |> member "pos_x" |> to_float;
      pos_y = enemy_json |> member "pos_y" |> to_float;
      screen_x = enemy_json |> member "screen_x" |> to_int;
      screen_y = enemy_json |> member "screen_y" |> to_int;
      entity_textures_id = 0;
      target_x = enemy_json |> member "target_x" |> to_float;
      target_y = enemy_json |> member "target_y" |> to_float;
      moving = false;
      state = Idle;
      direction = Down;
      current_hp = enemy_json |> member "current_hp" |> to_int;
      max_hp = enemy_json |> member "max_hp" |> to_int;
      level = enemy_json |> member "level" |> to_int;
      current_xp = enemy_json |> member "current_xp" |> to_int;
      max_xp = enemy_json |> member "max_xp" |> to_int;
    }) in

  (map, player, enemy)

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
  [pokemon_to_yojson player] converts a player to a JSON representation.
  @param player The player to convert.
  @return A JSON value of type [`Assoc] representing the player, with the following keys:
  - ["pos_x"]: the x position of the player.
  - ["pos_y"]: the y position of the player.
  - ["screen_x"]: the x screen position of the player.
  - ["screen_y"]: the y screen position of the player.
  - ["entity_textures_id"]: the texture id of the player.
  - ["target_x"]: the x target position of the player.
  - ["target_y"]: the y target position of the player.
  - ["current_hp"]: the current HP of the player.
  - ["max_hp"]: the maximum HP of the player.
  - ["level"]: the level of the player.
  - ["current_xp"]: the current XP of the player.
  - ["max_xp"]: the maximum XP of the player.
*)
let pokemon_to_yojson (player: pokemon) =
  `Assoc [
    ("pos_x", `Float player.pos_x);
    ("pos_y", `Float player.pos_y);
    ("screen_x", `Int player.screen_x);
    ("screen_y", `Int player.screen_y);
    ("entity_textures_id", `Int player.entity_textures_id);
    ("target_x", `Float player.target_x);
    ("target_y", `Float player.target_y);
    ("current_hp", `Int player.current_hp);
    ("max_hp", `Int player.max_hp);
    ("level", `Int player.level);
    ("current_xp", `Int player.current_xp);
    ("max_xp", `Int player.max_xp);
  ]

  let pokemons_to_yojson (pokemons: pokemon list) =
    `List (List.map pokemon_to_yojson pokemons)

(**
  [map_player_to_json map player enemy] converts a map, a player and an enemy to a JSON representation.
  @param map The map to convert.
  @param player The player to convert.
  @param enemy The enemy to convert.
  @return A JSON value of type [`Assoc] representing the map, player and enemy, with the following keys:
  - ["map"]: the map converted to JSON.
  - ["player"]: the player converted to JSON.
  - ["enemy"]: the enemy converted to JSON.
*)
let map_player_to_json (map: map) (player: pokemon) (enemy: pokemon list) =
  `Assoc [
    ("map", map_to_yojson map);
    ("player", pokemon_to_yojson player);
    ("enemy", pokemons_to_yojson enemy)
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
  [read_json_files_in_directory dir_path] lit tous les noms de fichiers .json dans un dossier.
  @param dir_path Le chemin du dossier.
  @return Une liste de noms de fichiers .json.
*)
let read_json_files_in_directory dir_path =
  let files = Sys.readdir dir_path in
  Array.to_list files
  |> List.filter (fun file -> Filename.check_suffix file ".json")
  |> List.map Filename.chop_extension