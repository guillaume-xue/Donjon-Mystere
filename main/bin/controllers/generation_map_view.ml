open Raylib
open Models.Automate_1
open Models.Prim_2
open Models.Biomes_3
open Utils.Settings_map
open Views.MapView
open Views.EntityView
open Views.TrapGroundView
open Views.ItemView
open Models.Generation_map
open Models.Entity_gen
open Utils.Types

let run_generation () =

  init_window 1120 840 "Mystery Dungeon";
  set_target_fps 60;
  

  let map_textures = init_map_textures () in
  let player_textures = init_entity_textures () in
  let trap_ground_textures = init_trap_ground_textures () in
  let items_textures = init_items_textures () in
  Printf.printf "size textures : %d\n" (List.length map_textures);
  let list_of_maps = init_map () in

  let rec loop list_of_maps player trap enemys items =
    if window_should_close () then
      begin
        close_window ()
      end
    else
      begin
        begin_drawing ();
        clear_background Color.white;
        draw_map_view list_of_maps map_textures;
        draw_items_view items items_textures;
        draw_trap_ground_view trap trap_ground_textures;
        draw_entity_view player player_textures;
        List.iter (fun enemy -> draw_entity_view enemy player_textures) enemys;
        end_drawing ();
        loop list_of_maps player trap enemys items
      end
  in

  let rec loop_regles_auto_cell map_textures list_of_maps n =
    if window_should_close () then
      begin
        close_window ()
      end
    else
      begin
        let list_of_maps_new = regles_auto_cell_view list_of_maps n in
        if list_of_maps = list_of_maps_new then
          let list_of_maps_new = remove_small_zones list_of_maps in
          begin_drawing ();
          clear_background Color.white;
          draw_map_view list_of_maps_new map_textures;
          end_drawing ();
          Unix.sleep 1;
          let regions_tmp = get_all_zones list_of_maps_new in
          let list_of_maps_new = connect_zones list_of_maps_new regions_tmp in
          begin_drawing ();
          clear_background Color.white;
          draw_map_view list_of_maps_new map_textures;
          end_drawing ();
          Unix.sleep 1;
          let list_of_maps_new = generate_biomes list_of_maps_new regions_tmp () in
          let map = {
            width = map_size_x;
            height = map_size_y;
            tiles = list_of_maps_new;
            regions = regions_tmp;
            floor = 10; (* Initial floor *)
            music = Some (""); (* Initial music *)
          } in
          begin_drawing ();
          clear_background Color.white;
          draw_map_view list_of_maps_new map_textures;
          end_drawing ();
          Unix.sleep 1;
          let (player, zone_rand) = spawn_player map 1 in
          let player = { player with entity_textures_id = 50 } in
          begin_drawing ();
          clear_background Color.white;
          draw_map_view list_of_maps_new map_textures;
          draw_entity_view player player_textures;
          end_drawing ();
          Unix.sleep 1;
          let trap_and_ground = spawn_list_of_trap_and_ground map zone_rand in
          begin_drawing ();
          clear_background Color.white;
          draw_map_view list_of_maps_new map_textures;
          draw_trap_ground_view trap_and_ground trap_ground_textures;
          draw_entity_view player player_textures;
          end_drawing ();
          Unix.sleep 1;
          let (enemys, _) = spawn_list_of_enemys map player in
          begin_drawing ();
          clear_background Color.white;
          draw_map_view list_of_maps_new map_textures;
          draw_trap_ground_view trap_and_ground trap_ground_textures;
          draw_entity_view player player_textures;
          List.iter (fun enemy -> draw_entity_view enemy player_textures) enemys;
          end_drawing ();
          Unix.sleep 1;
          let items = spawn_list_of_loot map in
          begin_drawing ();
          clear_background Color.white;
          draw_map_view list_of_maps_new map_textures;
          draw_items_view items items_textures;
          draw_trap_ground_view trap_and_ground trap_ground_textures;
          draw_entity_view player player_textures;
          List.iter (fun enemy -> draw_entity_view enemy player_textures) enemys;
          end_drawing ();
          Unix.sleep 1;
          loop list_of_maps_new player trap_and_ground enemys items
        else
          begin_drawing ();
          clear_background Color.white;
          draw_map_view list_of_maps_new map_textures;
          end_drawing ();
          Unix.sleep 1;
          loop_regles_auto_cell map_textures list_of_maps_new (n - 1)
      end
  in
  loop_regles_auto_cell map_textures list_of_maps iterations