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
    pos_x = json |> member "pos_x" |> to_int;
    pos_y = json |> member "pos_y" |> to_int;
    screen_x = json |> member "screen_x" |> to_int;
    screen_y = json |> member "screen_y" |> to_int;
    player_textures_id = json |> member "player_textures_id" |> to_int;
  }