open Raylib
open Utils.Types
open Utils.Json_conversions
open Utils.Settings_map
open Utils.Audio
open MenuController
open MapController

(**
  [run ()] runs the game.
*)
let run () =

  init_window screen_width screen_height "Mystery Dungeon";
  set_target_fps 0;

  (* Variables *)
  let screen_state = Intro in
  let map_name = "map " in
  let list_of_maps = read_json_files_in_directory map_dir in
  let (menu_item_info, menu_stats) = init_menu_controller () in

  (* Game loop *)
  let rec game_loop var_game last_time name msgs = 
    let (map_textures, entity_textures, items_textures, bag_textures, shadow_cast_texture, trap_and_ground_texures, attack_msg_textures, my_map, my_player, enemy, loots, traps_and_grounds, select) = var_game in
    if window_should_close () then begin
      stop_music ();
      save_game name my_map my_player enemy loots traps_and_grounds;
      close_window ()
    end else
      begin
        (* let (my_map, player, _key_pressed, last_time, select, loots, enemy, status_fin, traps_and_grounds, enemy) = update_player my_player enemy my_map last_time select loots traps_and_grounds in
        let enemy = if status_fin then List.map (fun e -> set_your_turn true e) enemy else enemy in
        let (enemy, last_time, player, status_fin) = update_enemy enemy player my_map last_time in
        let player = if status_fin then set_your_turn true player else player in *)

        (* let rec aux entity acc select loots last_time traps_and_grounds msgs =
          match entity with
          | [] -> let acc = List.sort (fun e1 e2 -> compare e1.id e2.id) acc in
            (* List.iter (fun e -> Printf.printf "Enemy %d is your turn\n%!" e.id) acc; *)
            let player = List.hd acc in
            let enemy = List.tl acc in
            let player = set_your_turn true player in
            (player :: enemy, select, loots, last_time, traps_and_grounds, msgs)
          | e :: rest ->
            if e.your_turn then begin
              if e.id = 0 then begin
                let (player, select, loots, enemy, traps_and_grounds, last_time, msg) = update_player e rest my_map select loots traps_and_grounds last_time in
                  let msgs = add_msg msg msgs in
                  (player :: enemy, select, loots, last_time, traps_and_grounds, msgs)                  
              end else begin
                (* Printf.printf "Enemy %d is not your turn\n%!" e.id; *)
                let (enemy, player, last_time, msg) = update_enemy e (acc @ rest) my_map last_time in
                let msgs = add_msg msg msgs in
                (* Printf.printf "Enemy %d is your turn\n\n%!" e.id; *)
                let acc = List.sort (fun e1 e2 -> compare e1.id e2.id) acc in
                let other = List.tl acc in
                let acc = player :: other in
                (* Printf.printf "Enemy %d your turn\n\n%!" e.id; *)
                (acc @ (enemy :: rest), select, loots, last_time, traps_and_grounds, msgs)

              end
            end else begin
              (* Printf.printf "Enemy %d is not your turn\n%!" e.id; *)
              aux rest (e :: acc) select loots last_time traps_and_grounds msgs
            end in *)

        let player =
        if List.for_all (fun e -> not e.your_turn) enemy && not my_player.your_turn then
          set_your_turn true my_player
        else
          my_player in


        let (player, select, loots, enemy, traps_and_grounds, last_time, msg) = update_player player enemy my_map select loots traps_and_grounds last_time in
        let msgs = add_msg msg msgs in

        let rec aux player enemy (other:pokemon list) map last_time msgs =
          match enemy with
          | [] -> (other, last_time, msgs)
          | e :: rest ->
            if e.your_turn then begin
              let (enemy, player, last_time, msg) = update_enemy e (player :: rest) map last_time in
              let msgs = add_msg msg msgs in
              aux player rest (enemy :: other) map last_time msgs
            end else begin
              aux player rest (e :: other) map last_time msgs
            end in


        let (enemy, last_time, msgs) = aux player enemy [] my_map last_time msgs in
        let grid_shadow_cast = update_shadow_cast player my_map in
        draw_game my_map player enemy loots grid_shadow_cast traps_and_grounds map_textures entity_textures items_textures bag_textures shadow_cast_texture trap_and_ground_texures attack_msg_textures select msgs;
        game_loop (map_textures, entity_textures, items_textures, bag_textures, shadow_cast_texture, trap_and_ground_texures, attack_msg_textures, my_map, player, enemy, loots, traps_and_grounds, select) last_time name msgs
      end
  in

  (* Menu loop *)
  let rec menu_loop screen_state map_name menu_item_info last_time_menu =
    if window_should_close () then
      begin
        stop_music ();
        close_window ()
      end
    else
      if screen_state = Game then
        begin          
          let (map_textures, entity_textures, items_textures, bag_textures, shadow_cast_texture, trap_and_ground_texures, attack_msg_textures, my_map, my_player, enemy, loots, traps_and_grounds) = init_map_controller map_name in
          init_audio ();
          (match my_map.music with
          | Some file -> play_music file
          | None -> ());
          let list_of_last_time = List.init (7 + ((List.length(enemy))*2)) (fun _ -> 0.0) in (* Use for animations *)
          let msgs = [] in
          game_loop (map_textures, entity_textures, items_textures, bag_textures, shadow_cast_texture, trap_and_ground_texures, attack_msg_textures, my_map, my_player, enemy, loots, traps_and_grounds, 0) list_of_last_time map_name msgs;
        end
      else
        begin
          let (screen_state, map_name, menu_item_info, last_time_menu) = check_screen_state screen_state map_name menu_item_info menu_stats list_of_maps last_time_menu in
          menu_loop screen_state map_name menu_item_info last_time_menu;
        end
  in
  menu_loop screen_state map_name menu_item_info (List.init 5 (fun _ -> 0.0))