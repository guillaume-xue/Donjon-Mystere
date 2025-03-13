open Raylib
open Models.PlayerModel
open Views.PlayerView
open Views.MapView
open Utils.Types
open Utils.Json_conversions
open Utils.Settings_map

(**
  [init_map_textures filename] initializes the textures for the map.
  @param filename The name of the file.
  @return The textures of the map.
*)
let init_map_controller filename =
  let map_textures = init_map_textures () in
  let player_textures = init_player_textures () in
  let (my_map, my_player) = load_map_player_from_json (map_dir ^ filename ^ ".json") in
  let my_player = set_player_screen my_player (screen_width / 2) (screen_height / 2) in
  let my_player = set_player_texture_id my_player 24 in
  (map_textures, player_textures, my_map, my_player)

(**
  [draw_game my_map my_player map_textures player_textures] draws the game.
  @param my_map The map.
  @param my_player The player.
  @param map_textures The textures of the map.
  @param player_textures The textures of the player.
*)
let draw_game my_map my_player map_textures player_textures =
  begin_drawing ();
  clear_background Color.raywhite;
  draw_map my_map my_player map_textures;
  draw_player my_player player_textures;
  draw_player_stats my_player;
  end_drawing ()

(**
  [increment_texture_id my_player last_texture_update_time] increments the texture id of the player.
  @param my_player The player.
  @param last_texture_update_time The last time the texture was updated.
  @return The updated player and the last time the texture was updated.
*)
let increment_texture_id my_player last_texture_update_time =
  if get_time () -. last_texture_update_time >= 0.2 then begin
    let my_player = update_player_texture_id my_player in
    (my_player, get_time ())
  end
  else begin
    (my_player, last_texture_update_time)
  end

(**
  [is_wall my_map my_player] checks if the player is facing a wall.
  @param my_map The map.
  @param my_player The player.
  @return True if the player is facing a wall, false otherwise.
*)
let is_wall my_map my_player =
  let tiles = my_map.tiles in
  let target_x = int_of_float my_player.target_x in
  let target_y = int_of_float my_player.target_y in
  List.exists (fun tile -> tile.x = target_x && tile.y = target_y && tile.texture_id = 1) tiles

(**
  [new_player_pos my_map my_player last_update_time] updates the player position.
  @param my_map The map.
  @param my_player The player.
  @param last_update_time The last time the player position was updated.
  @return The updated player and the last time the player position was updated.
*)
let new_player_pos my_map my_player last_update_time =
  let current_time = get_time () in
  let my_player = my_player in
  if not (is_wall my_map my_player) then begin
    let my_player = set_player_moving my_player false in
    let my_player = set_player_state my_player Idle in
    let my_player = set_target my_player my_player.pos_x my_player.pos_y in
    (my_player, last_update_time)
  end else if current_time -. last_update_time >= 0.01 then
    let dx = my_player.target_x -. my_player.pos_x in
    let dy = my_player.target_y -. my_player.pos_y in
    let step = 0.1 in
    let new_x = if abs_float dx < step then my_player.target_x else my_player.pos_x +. (if dx > 0.0 then step else -.step) in
    let new_y = if abs_float dy < step then my_player.target_y else my_player.pos_y +. (if dy > 0.0 then step else -.step) in
    if my_player.pos_x = my_player.target_x && my_player.pos_y = my_player.target_y then begin
      let my_player = set_player_moving my_player false in
      (my_player, current_time)
    end else if not my_player.moving then begin
      let new_x = if new_x <> floor new_x then floor new_x +. 1.0 else new_x in
      let new_y = if new_y <> floor new_y then floor new_y +. 1.0 else new_y in
      let my_player = set_player_pos my_player new_x new_y in
      let my_player = set_player_state my_player Idle in
      (my_player, current_time)
    end else begin
      let my_player = set_player_pos my_player new_x new_y in
      let my_player = set_player_state my_player Moving in
      (my_player, current_time)
    end
  else
    (my_player, last_update_time)

(**
  [check_key_pressed my_player] checks if a key is pressed.
  @param my_player The player.
  @return The updated player.
*)
let check_key_pressed my_player =
  let my_player = my_player in
  if not(my_player.moving) then begin
    if is_key_down Key.Right then begin
      let my_player = set_target my_player (my_player.pos_x +. 1.0) my_player.pos_y in
      let my_player = set_player_direction my_player Right in
      let my_player = set_player_moving my_player true in
      my_player
    end else if is_key_down Key.Left then begin
      let my_player = set_target my_player (my_player.pos_x -. 1.0) my_player.pos_y in
      let my_player = set_player_direction my_player Left in
      let my_player = set_player_moving my_player true in
      my_player
    end else if is_key_down Key.Up then begin
      let my_player = set_target my_player my_player.pos_x (my_player.pos_y -. 1.0) in
      let my_player = set_player_direction my_player Up in
      let my_player = set_player_moving my_player true in
      my_player
    end else if is_key_down Key.Down then begin
      let my_player = set_target my_player my_player.pos_x (my_player.pos_y +. 1.0) in
      let my_player = set_player_direction my_player Down in
      let my_player = set_player_moving my_player true in
      my_player
    end else
      my_player
  end else
    my_player

(**
  [save_game filename my_map my_player] saves the game.
  @param filename The name of the file.
  @param my_map The map.
  @param my_player The player.
*)
let save_game filename my_map my_player =
  let json = map_player_to_json my_map my_player in
  write_json_to_file (map_dir ^ filename ^ ".json") json