open Raylib
open Utils.Types
open Utils.Settings_map

(**
  [init_map_textures ()] initializes the textures for the map.
*)
let init_map_textures () =
  let image = load_image "resources/images/map/forest.png" in
  if not (is_image_ready image) then begin
    Printf.printf "Failed to load image: resources/images/map/forest.png\n";
    []
  end else begin
    let rec init_textures x y map_textures =
      if x < 2 then
        begin
          let source_rec = Rectangle.create (tile_texture_size *. float_of_int(x) +. float_of_int(x)) (tile_texture_size *. float_of_int(y) +. float_of_int(y)) tile_texture_size tile_texture_size in
          let tex = load_texture_from_image (image_from_image image source_rec) in
          init_textures (x + 1) y (tex :: map_textures)
        end
      else if y < 2 then
        init_textures 0 (y + 1) map_textures
      else
        map_textures
    in
    let textures = init_textures 0 0 [] in
    unload_image image;
    List.rev textures
  end

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
      let texture = List.nth textures (num tile.texture_id) in
      draw_texture texture (player.screen_x + tile.x * int_of_float(tile_texture_size) - int_of_float(x)) (player.screen_y + tile.y * int_of_float(tile_texture_size) - int_of_float(y)) Color.white;
      draw_textures rest x y
  in
  draw_textures map.tiles (player.pos_x *. 24.0) (player.pos_y *. 24.0);