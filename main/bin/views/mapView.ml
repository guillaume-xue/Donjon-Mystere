open Raylib
open Utils.Types
open Utils.Settings_map

(**
  [init_map_textures ()] initializes the textures for the map.
*)
let init_map_textures () =
  let rec load_image_list path x =
    let image = load_image ("resources/images/map/biome" ^ string_of_int x ^ ".png") in
    if x < 9 then
      load_image_list (image :: path) (x + 1)
    else
      image :: path
  in
  
  let image_load = (load_image_list [] 1) in

  let rec init_textures x y image textures =
    if x < 2 then
      begin
        let source_rec = Rectangle.create (tile_texture_size *. float_of_int(x) +. float_of_int(x)) (tile_texture_size *. float_of_int(y) +. float_of_int(y)) tile_texture_size tile_texture_size in
        let tex = load_texture_from_image (image_from_image image source_rec) in
        init_textures (x + 1) y image (tex :: textures)
      end
    else if y < 0 then
      init_textures 0 (y + 1) image textures
    else
      textures
  in
  let rec image_to_texture_list images textures =
    match images with
    | [] -> textures
    | image :: rest -> image_to_texture_list rest ((init_textures 0 0 image textures) @ textures)
  in
  let textures = image_to_texture_list image_load [] in
  List.rev textures

(**
  [draw_map map player textures] draws the map.
  @param map The map to draw.
  @param player The player.
  @param textures The textures of the map.
*)
let draw_map map player textures =
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

      let texture = List.nth textures ((num tile.texture_id) + (tile.biome_id - 1) * 2) in
      draw_texture texture (player.screen_x + tile.x * int_of_float(tile_texture_size) - int_of_float(x)) (player.screen_y + tile.y * int_of_float(tile_texture_size) - int_of_float(y)) Color.white;
      draw_textures rest x y
  in
  draw_textures map.tiles (player.pos_x *. tile_texture_size) (player.pos_y *. tile_texture_size);