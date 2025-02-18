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