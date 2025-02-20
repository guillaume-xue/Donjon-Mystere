open Raylib
open Utils.Types

let textures = ref []

(**
  [init_map ()] initializes the textures for the map.
*)
let init_map () =
  let image = load_image "resources/map/forest.png" in
  let rec init_textures x y =
    if x < 3 then
      begin
        let source_rec = Rectangle.create (float_of_int(24 * x) +. float_of_int(x)) (float_of_int(24 * y) +. float_of_int(y)) 24.0 24.0 in
        let tex = load_texture_from_image (image_from_image image source_rec) in
        textures := tex :: !textures;
        init_textures (x + 1) y
      end
    else if y < 2 then
      init_textures 0 (y + 1)
  in
  init_textures 0 0;
  textures := List.rev !textures;
  unload_image image

(**
  [draw_map map] draws the map on the screen.
  @param map The map to draw.
*)
let draw_map (map: map) (player: player) =
  let rec draw_textures (tiles: tile list) =
    match tiles with
    | [] -> ()
    | tile :: rest ->
      let num n =
        match n with
        | 0 -> 0
        | 1 -> 4
        | _ -> 0
      in
      let texture = List.nth !textures (num tile.texture_id) in
      draw_texture texture (player.screen_x + player.pos_x + tile.x * 24) (player.screen_y + player.pos_y + tile.y * 24) Color.white;
      draw_textures rest;
  in
  draw_textures map.tiles
  
  