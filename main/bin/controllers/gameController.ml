open Raylib
open Utils.Types
open Utils.Json_conversions
open Utils.Settings_map
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
  let (menu_item_info, menu_stats) = init_menu_controller () in

  (* Game loop *)
  let rec game_loop var_game last_time name = 
    let (map_textures, player_textures, enemy_textures, items_textures, my_map, my_player, enemy, loots) = var_game in
    if window_should_close () then begin
      save_game name my_map my_player enemy loots;
      close_window ()
    end else
      begin
        let (player, key_pressed, last_time) = update_player my_player enemy my_map last_time in
        let (enemy, last_time) = update_enemy enemy player my_map key_pressed last_time in
        draw_game my_map player enemy loots map_textures player_textures enemy_textures items_textures;
        game_loop (map_textures, player_textures, enemy_textures, items_textures, my_map, player, enemy, loots) last_time name
      end
  in

  (* Menu loop *)
  let rec menu_loop screen_state map_name menu_item_info last_time_menu =
    if window_should_close () then
      begin
        close_window ()
      end
    else
      if screen_state = Game then
        begin
          let (map_textures, player_textures, enemy_textures, items_textures, my_map, my_player, enemy, loots) = init_map_controller map_name in
          let list_of_last_time = List.init (2 + ((List.length(enemy))*2)) (fun _ -> 0.0) in (* Use for animations *)
          game_loop (map_textures, player_textures, enemy_textures, items_textures, my_map, my_player, enemy, loots) list_of_last_time map_name;
        end
      else
        begin
          let (screen_state, map_name, menu_item_info, last_time_menu) = check_screen_state screen_state map_name menu_item_info menu_stats list_of_maps last_time_menu in
          menu_loop screen_state map_name menu_item_info last_time_menu;
        end
  in
  menu_loop screen_state map_name menu_item_info (List.init 5 (fun _ -> 0.0));