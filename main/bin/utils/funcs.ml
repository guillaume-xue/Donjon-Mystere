open Types
open Yojson.Basic
open Yojson.Basic.Util

(**
  [load_map_from_json filename] loads a map from a JSON file.
  @param filename The name of the file to load.
  @return The map loaded from the file.
*)
let load_map_from_json (filename: string): map =
  let json = from_file filename in
  let width = json |> member "width" |> to_int in
  let height = json |> member "height" |> to_int in
  let tiles = json |> member "tiles" |> to_list |> List.map (fun tile ->
    {
      x = tile |> member "x" |> to_int;
      y = tile |> member "y" |> to_int;
      texture_id = tile |> member "texture_id" |> to_int;
    }
  ) in
  { width; height; tiles }

(**
  [load_player_from_json filename] loads a player from a JSON file.
  @param filename The name of the file to load.
  @return The player loaded from the file.
*)
let load_player_from_json (filename: string): player =
  let json = from_file filename in
  {
    pos_x = json |> member "pos_x" |> to_float;
    pos_y = json |> member "pos_y" |> to_float;
    screen_x = json |> member "screen_x" |> to_int;
    screen_y = json |> member "screen_y" |> to_int;
    player_textures_id = 0;
    target_x = json |> member "target_x" |> to_float;
    target_y = json |> member "target_y" |> to_float;
    moving = false;
    state = Idle;
    direction = Down;
    current_hp = json |> member "current_hp" |> to_int;
    max_hp = json |> member "max_hp" |> to_int;
    level = json |> member "level" |> to_int;
    current_xp = json |> member "current_xp" |> to_int;
    max_xp = json |> member "max_xp" |> to_int;
  }

(**
  [save_player_to_json filename player] saves a player to a JSON file.
  @param filename The name of the file to save.
  @param player The player to save.
*)
let save_player_to_json (filename: string) (player: player) =
  let json = `Assoc [
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
  ] in
  to_file filename json