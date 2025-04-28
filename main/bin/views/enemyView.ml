open Raylib
open Utils.Types
open Utils.Settings_map

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
    draw_texture texture 
      (int_of_float (float_of_int player.screen_x +. enemy.pos_x *. tile_texture_size -. player.pos_x *. tile_texture_size)) 
      (int_of_float (float_of_int player.screen_y +. enemy.pos_y *. tile_texture_size -. player.pos_y *. tile_texture_size)) 
      Color.white
  ) enemies