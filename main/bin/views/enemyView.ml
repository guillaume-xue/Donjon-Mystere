open Raylib
open Utils.Types
open Utils.Settings_map
open Utils.Funcs

(**
  [init_enemy_textures ()] initializes the textures for the enemy.
  @return The textures of the enemy.
*)
let init_enemy_textures () =
  let image_path = "resources/images/player/hericendre.png" in
  if not (file_exists image_path) then begin
    Printf.printf "Image file does not exist: %s\n" image_path;
    []
  end else begin
    let player_textures = [] in
    let image = load_image image_path in
    if is_image_ready image then begin
      let player_textures = init_textures 0 40 image player_textures in
      unload_image image;
      List.rev player_textures
    end else begin
      Printf.printf "Failed to load image: %s\n" image_path;
      []
    end
  end

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
    let texture = List.nth enemy_textures enemy.entity_textures_id in
    draw_texture texture 
      (int_of_float (float_of_int player.screen_x +. enemy.pos_x *. tile_texture_size -. player.pos_x *. tile_texture_size)) 
      (int_of_float (float_of_int player.screen_y +. enemy.pos_y *. tile_texture_size -. player.pos_y *. tile_texture_size)) 
      Color.white
  ) enemies