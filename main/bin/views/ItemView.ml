open Raylib
open Utils.Types
open Utils.Settings_map

let init_items_textures () =
  let image = load_image ("resources/images/loot/throw_items.png") in
  if is_image_ready image then
    let rec init_loots x y loots_textures =
      if x < 5 then
        begin
          let source_rec = Rectangle.create (loot_texture_size *. float_of_int(x) +. float_of_int(x)) (loot_texture_size *. float_of_int(y) +. float_of_int(y)) loot_texture_size loot_texture_size in
          let tex = load_texture_from_image (image_from_image image source_rec) in
          init_loots (x + 1) y (tex :: loots_textures)
        end
      else if y < 2 then
        init_loots 0 (y + 1) loots_textures
      else
        loots_textures
    in
    let loots_textures = init_loots 0 0 [] in
    unload_image image;
    List.rev loots_textures
  else begin
    Printf.printf "Failed to load image: %s\n" ("resources/images/map/throw_items.png");
    []
  end

let draw_items (loots : loot list) (player : pokemon) loots_textures =
  List.iter (fun loot ->
    let texture = List.nth loots_textures loot.item_skin_id in
    draw_texture texture 
      (int_of_float (float_of_int player.screen_x +. loot.pos_x *. tile_texture_size -. player.pos_x *. tile_texture_size)) 
      (int_of_float (float_of_int player.screen_y +. loot.pos_y *. tile_texture_size -. player.pos_y *. tile_texture_size)) 
      Color.white
  ) loots
