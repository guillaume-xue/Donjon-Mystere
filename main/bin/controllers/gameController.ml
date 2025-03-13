open Raylib
open Utils.Types
open Utils.Json_conversions
open Utils.Settings_map
open Models.PlayerModel
open MenuController
open MapController

(**
  [run ()] runs the game.
*)
let run () =

  init_window screen_width screen_height "Mystery Dungeon";
  set_target_fps 60;

  (* Variables *)
  let screen_state = Intro in
  let map_name = "map " in
  let list_of_maps = read_json_files_in_directory map_dir in
  let index_select_x = 0 in (* Variable of cursor x *)
  let index_select_y = 0 in (* Variable of cursor y *)
  let last_key_press_time = get_time () in
  let val_menu = init_menu_controller () in
  let arrow_pos_x = 70 in
  let arrow_pos_y = 58 in

  (* Game loop *)
  let rec game_loop var_game last_update_time last_texture_update_time name = 
    let (map_textures, player_textures, my_map, my_player) = var_game in
    if window_should_close () then begin
      save_game name my_map my_player;
      close_window ()
    end else
      begin
        draw_game my_map my_player map_textures player_textures;
        let my_player = check_key_pressed my_player in
        let (my_player, last_update_time) = new_player_pos my_map my_player last_update_time in
        let (my_player, last_texture_update_time) = increment_texture_id my_player last_texture_update_time in
        let my_player = is_end_moving my_player in
        game_loop (map_textures, player_textures, my_map, my_player) last_update_time last_texture_update_time name
      end
  in

  (* Menu loop *)
  let rec menu_loop screen_state name index_select_x index_select_y last_key_press_time arrow_pos_x arrow_pos_y =
    if window_should_close () then
      begin
        close_window ()
      end
    else
      if screen_state = Game then
        begin
          let var_game = init_map_controller name in
          let last_update_time = 0.0 in (* Last Player position update time *)
          let last_texture_update_time = 0.0 in (* Last texture update time *)
          game_loop var_game last_update_time last_texture_update_time name;
        end
      else
        begin
          let (screen_state, new_map_name, index_select_x, index_select_y, time, arrow_pos_x, arrow_pos_y) = check_screen_state screen_state name list_of_maps index_select_x index_select_y last_key_press_time val_menu arrow_pos_x arrow_pos_y in
          menu_loop screen_state new_map_name index_select_x index_select_y time arrow_pos_x arrow_pos_y;
        end
  in
  menu_loop screen_state map_name index_select_x index_select_y last_key_press_time arrow_pos_x arrow_pos_y