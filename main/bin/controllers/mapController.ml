open Raylib
open Models.EntityModel
open Models.EnemyModel
open Views.PlayerView
open Views.MapView
open Views.EnemyView
open Utils.Types
open Utils.Json_conversions
open Utils.Funcs
open Utils.Settings_map

(**
  [init_map_textures filename] initializes the textures for the map.
  @param filename The name of the file.
  @return The textures and map elements.
*)
let init_map_controller filename =
  let map_textures = init_map_textures () in
  let player_textures = init_player_textures () in
  let enemy_textures = init_enemy_textures () in
  let (map, player, enemy) = load_map_player_from_json (map_dir ^ filename ^ ".json") in
  let new_player = 
    player
    |> set_entity_screen (screen_width / 2) (screen_height / 2)
    |> set_entity_texture_id 24
  in
  (map_textures, player_textures, enemy_textures, map, new_player, enemy)

(**
  [draw_game map player map_textures player_textures] draws the game.
  @param map The map.
  @param player The player.
  @param enemy The enemy.
  @param map_textures The textures of the map.
  @param player_textures The textures of the player.
  @param enemy_textures The textures of the enemy.
*)
let draw_game map player enemy map_textures player_textures enemy_textures =
  begin_drawing ();
  clear_background Color.raywhite;
  draw_map map player map_textures;
  draw_player player player_textures;
  draw_enemy enemy enemy_textures player;
  draw_player_stats player;
  end_drawing ()

(**
  [check_key_pressed player] checks if a key is pressed.
  @param player The player.
  @return The direction and if a key is pressed.
*)
let check_key_pressed player =
  if not(player.moving) then begin
    if is_key_down Key.Right then begin
      (Right, true)
    end else if is_key_down Key.Left then begin
      (Left, true)
    end else if is_key_down Key.Up then begin
      (Up, true)
    end else if is_key_down Key.Down then begin
      (Down, true)
    end else
      (player.direction, false)
  end else
    (player.direction, false)

(**
  [update_player player enemy map last_time] updates the player.
  @param player The player.
  @param enemy The enemy.
  @param map The map.
  @param last_time The last time.
  @return The updated player.
*)
let update_player player enemy map last_time =
  let (direction, key_pressed) = check_key_pressed player in
  let player = move direction player key_pressed in
  let (player, last_update_time) = new_entity_pos map player enemy (List.nth last_time 4) in
  let last_time = replace_nth last_time 4 last_update_time in
  let (player, last_texture_update_time) = increment_texture_id player (List.nth last_time 3) in
  let last_time = replace_nth last_time 3 last_texture_update_time in
  let player = is_end_moving player in
  (player, key_pressed, last_time)

(**
  [update_enemy enemy player map key_pressed last_time] updates the enemy.
  @param enemy The enemy.
  @param player The player.
  @param map The map.
  @param key_pressed True if a key is pressed, false otherwise.
  @param last_time The last time.
  @return The updated enemy.
*)
let update_enemy enemy player map key_pressed last_time =
  let enemy_target = update_target_enemy enemy player in
  let enemy = move enemy_target enemy key_pressed in
  let (enemy, last) = new_entity_pos map enemy player (List.nth last_time 1) in
  let last_time = replace_nth last_time 1 last in
  let (enemy, last_t) = increment_texture_id enemy (List.nth last_time 2) in
  let last_time = replace_nth last_time 2 last_t in
  let enemy = is_end_moving enemy in
  (enemy, last_time)

(**
  [save_game filename map player enemy] saves the game.
  @param filename The name of the file.
  @param map The map.
  @param player The player.
  @param enemy The enemy.
*)
let save_game filename map player enemy =
  let json = map_player_to_json map player enemy in
  write_json_to_file (map_dir ^ filename ^ ".json") json