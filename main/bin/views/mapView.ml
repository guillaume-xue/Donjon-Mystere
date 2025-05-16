open Raylib
open Utils.Types
open Utils.Settings_map
open Utils.Funcs
open Models.EntityModel

(**
  [init_map_textures ()] initializes the textures for the map.
  @return The textures of the map.
*)
let init_map_textures () =
   let cwd = Sys.getcwd () in
      print_endline ("Current working directory: " ^ cwd);
  let image_paths = Sys.readdir "resources/images/map/biome/" |> Array.to_list |> List.filter (fun file -> Filename.check_suffix file ".png") in
  let map_textures = [] in
  let images = List.map (fun file -> load_image (Printf.sprintf "resources/images/map/biome/%s" file)) image_paths in
  let map_textures = 
    let rec load_textures acc = function
      | [] -> List.rev acc
      | img::rest ->
        let texture = init_textures 0 2 img acc in
        load_textures (texture) rest
    in
    load_textures map_textures images
  in
  map_textures

let draw_map_view tiles textures =
  let rec draw_textures (tiles: tile list) (x: float) (y: float) =
    match tiles with
    | [] -> ()
    | tile :: rest ->
      if (tile.biome_id = 0) then
        if (tile.texture_id = 0) then
          let texture = List.nth textures (1) in (* Debug *)
          draw_texture texture (tile.x * (int_of_float tile_texture_size) - int_of_float(x)) (tile.y * (int_of_float tile_texture_size) - int_of_float(y)) Color.white;
          draw_textures rest x y
        else
          let texture = List.nth textures (0) in (* Debug *)
          draw_texture texture (tile.x * (int_of_float tile_texture_size) - int_of_float(x)) (tile.y * (int_of_float tile_texture_size) - int_of_float(y)) Color.white;
          draw_textures rest x y
      else
        (* let texture = List.nth textures ((tile.texture_id) + ((tile.biome_id - 1) * 2)) in *)
        let texture = List.nth textures ((tile.texture_id) + ((tile.biome_id - 1) * 2)) in (* Debug *)
        draw_texture texture (tile.x * (int_of_float tile_texture_size) - int_of_float(x)) (tile.y * (int_of_float tile_texture_size) - int_of_float(y)) Color.white;
        draw_textures rest x y
  in
  draw_textures tiles 0.0 0.0

(**
  [draw_map map player textures] draws the map.
  @param map The map to draw.
  @param player The player.
  @param textures The textures of the map.
*)
let draw_map (map: map) (player: pokemon) textures =
  let rec draw_textures (tiles: tile list) (x: float) (y: float) =
    match tiles with
    | [] -> ()
    | tile :: rest ->
      let num n =
        match n with
        | 0 -> 1
        | 1 -> 0
        | _ -> 1
      in
      let texture = List.nth textures ((num tile.texture_id) + (tile.biome_id - 1) * 2) in (* Debug *)
      let (screen_x, screen_y) = get_entity_screen player in
      draw_texture texture (screen_x + tile.x * int_of_float(tile_texture_size) - int_of_float(x)) (screen_y + tile.y * int_of_float(tile_texture_size) - int_of_float(y)) Color.white;
      draw_textures rest x y
  in
  let (pos_x, pos_y) = get_entity_position player in
  draw_textures map.tiles (pos_x *. tile_texture_size) (pos_y *. tile_texture_size)

let draw_floor_intro map =
  draw_text ("Floor: " ^ string_of_int map.floor) (screen_width / 2 - 50) (screen_height / 2 - 50) 20 Color.white