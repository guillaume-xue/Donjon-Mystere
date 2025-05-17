open Raylib
open Utils.Types
open Utils.Settings_map
open Utils.Funcs
open Models.EntityModel

(**
  [init_items_textures ()] initializes the textures for the items.
  @return The textures of the items.
*)
let init_items_textures () =
  let image = load_image ("resources/images/loot/throw_items.png") in
  init_textures 0 10 image [] |> List.rev


(**
  [draw_items loots player loots_textures] draws the items on the map.
  @param loots The list of items.
  @param player The player.
  @param loots_textures The textures of the items.
*)
let draw_items (loots : loot list) (player : pokemon) loots_textures =
  List.iter (fun loot ->
    let texture = List.nth loots_textures loot.item_skin_id in
    let (screen_x, screen_y) = get_entity_screen player in
    let (pos_x, pos_y) = get_entity_position player in
    draw_texture texture 
      (int_of_float (float_of_int screen_x +. loot.pos_x *. tile_texture_size -. pos_x *. tile_texture_size)) 
      (int_of_float (float_of_int screen_y +. loot.pos_y *. tile_texture_size -. pos_y *. tile_texture_size)) 
      Color.white
  ) loots

let pick_up_item_print (item: loot) =
  "Vous avez ramassé un " ^ item.description ^ " !"