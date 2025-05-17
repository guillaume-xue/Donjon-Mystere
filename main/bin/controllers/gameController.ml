open Raylib
open Utils.Types
open Utils.Json_conversions
open Utils.Settings_map
open Utils.Audio
open Models.Game_state
open MenuController
open MapController

(**
  [run ()] runs the game.
*)
let run () : unit =

  init_window screen_width screen_height "Mystery Dungeon";
  set_target_fps 0;

  (* Variables *)
  init_audio ();
  let screen_state = Intro in
  let map_name = "map " in
  let list_of_maps = read_json_files_in_directory map_dir in
  let (menu_item_info, menu_stats) = init_menu_controller () in

  (* Game loop *)
  let rec game_loop var_game last_time name msgs = 
    let (game_textures, game_states) = var_game in
    if window_should_close () then begin
      stop_music ();
      save_game name game_states;
      close_window ()
    end else
      begin
        let player =
          if List.for_all (fun e -> not e.your_turn) game_states.enemies_state && not game_states.player_state.your_turn then
            set_your_turn true game_states.player_state
          else
            game_states.player_state 
        in
        let game_states = set_game_state_player player game_states in
        let (game_states, last_time) = update_player game_states last_time in
        (* Update enemies *)
        let rec aux (player : pokemon) (enemy : pokemon list) (other: pokemon list) (map : map) (last_time : float list) (game_states : game_state) : pokemon list * float list * game_state =
          match enemy with
          | [] -> (other, last_time, game_states)
          | e :: rest ->
            if e.your_turn then begin
              let (enemy, player, last_time, msg) = update_enemy e player (rest @ other) map last_time in
              let game_states = add_game_state_msg msg game_states in
              let game_states = set_game_state_player player game_states in
              aux player rest (enemy :: other) map last_time game_states
            end else begin
              aux player rest (e :: other) map last_time game_states
            end 
        in
        let (enemy, last_time, game_states) = aux game_states.player_state game_states.enemies_state [] game_states.map_state last_time game_states in
        if game_states.player_state.current_hp <= 0 then begin
          stop_music ();
          begin_drawing ();
          clear_background Color.black;
          draw_die_end ();
          end_drawing ();
          Unix.sleep 2;
          delete_save name;
          menu_loop Intro map_name menu_item_info (List.init 5 (fun _ -> 0.0)) (read_json_files_in_directory map_dir);
        end else begin
          let game_states = set_game_state_enemy enemy game_states in
          let grid_shadow_cast = update_shadow_cast game_states.player_state game_states.map_state in
          draw_game game_states game_textures grid_shadow_cast;
          game_loop (game_textures, game_states) last_time name msgs
        end
      end
  and

  (* Menu loop *)
  menu_loop (screen_state : screenState) (map_name : string) (menu_item_info : int * int * int * int) (last_time_menu : float list) (list_of_maps : string list) : unit =
    if window_should_close () then
      begin
        stop_music ();
        close_window ()
      end
    else
      if screen_state = Game then
        begin          
          let (game_textures, game_states) = init_map_controller map_name in
          (match  game_states.map_state.music with
          | Some file -> play_music file
          | None -> ());
          let id_max =
          List.fold_left
            (fun acc enemy ->
            if enemy.id > acc then enemy.id else acc)
            0 game_states.enemies_state
          in
          let list_of_last_time = List.init (7 + (id_max*2)) (fun _ -> 0.0) in (* Use for animations *)
          let msgs = [] in
          game_loop (game_textures, game_states) list_of_last_time map_name msgs;
        end
      else
        begin
          let (screen_state, map_name, menu_item_info, last_time_menu) = check_screen_state screen_state map_name menu_item_info menu_stats list_of_maps last_time_menu in
          menu_loop screen_state map_name menu_item_info last_time_menu list_of_maps;
        end
  in
  menu_loop screen_state map_name menu_item_info (List.init 5 (fun _ -> 0.0)) list_of_maps