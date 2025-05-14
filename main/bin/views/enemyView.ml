open Raylib
open Utils.Types
open Utils.Settings_map
open Models.EntityModel

(**
  [draw_enemy enemy enemy_textures player] draws the enemy.
  @param enemy The enemy.
  @param enemy_textures The textures of the enemy.
  @param player The player.
*)
let draw_enemy enemies enemy_textures (player: pokemon)=
  List.iter (fun enemy ->
    if enemy.current_hp <= 0 then
      ()
    else
    let texture = List.nth enemy_textures (40 * enemy.number + enemy.entity_textures_id) in
    let (p_screen_x, p_screen_y) = get_entity_screen player in
    let (p_pos_x, p_pos_y) = get_entity_position player in
    let (e_pos_x, e_pos_y) = get_entity_position enemy in
    draw_texture texture 
      (int_of_float (float_of_int p_screen_x +. e_pos_x *. tile_texture_size -. p_pos_x *. tile_texture_size)) 
      (int_of_float (float_of_int p_screen_y +. e_pos_y *. tile_texture_size -. p_pos_y *. tile_texture_size)) 
      Color.white
  ) enemies