open Raylib
open Models.EntityModel
open Models.EnemyModel
open Models.ItemModel
open Models.Shadowcaster
open Models.Trap_ground
open Models.Map_model
open Models.Game_state
open Views.PlayerView
open Views.EntityView
open Views.MapView
open Views.EnemyView
open Views.ItemView
open Views.BagView
open Views.TrapGroundView
open Views.ShadowCastView
open Views.AttackView
open TrapController
open Utils.Types
open Utils.Json_conversions
open Utils.Funcs
open Utils.Settings_map

(**
  [init_map_controller filename] initializes the map controller.
  @param filename The name of the map file.
  @return The map textures, player textures, enemy textures, items textures, bag textures, map, player, enemy and loots.
*)
let init_map_controller filename =
  let items_textures = init_items_textures () in
  let (bag, square, items) = init_bag_textures items_textures in
  let bag_textures = {
    bag_background_tex = bag;
    square_tex = square;
    items_tex = items;
  } in
  let game_textures = {
    tiles_tex = init_map_textures ();
    entities_tex = init_entity_textures ();
    items_tex = items_textures;
    bag_tex = bag_textures;
    shadow_cast_tex = init_shadow_cast_view ();
    traps_and_grounds_tex = init_trap_ground_textures ();
    attack_msg_tex = init_attack_msg_textures ();
  } in
  let (map, player, enemy, loots, traps_and_grounds) = load_map_player_from_json (map_dir ^ filename ^ ".json") in
  let map = set_map_music "resources/audio/music1.mp3" map in
  let new_player = 
    player
    |> set_entity_screen (screen_width / 2) (screen_height / 2)
    |> set_entity_texture_id 24
  in
  let game_states = {
    map_state = map;
    player_state = new_player;
    enemies_state = enemy;
    loots_state = loots;
    traps_and_grounds_state = traps_and_grounds;
    msgs_state = [];
  } in
  (game_textures, game_states)

(**
  [draw_open_bag player bag_textures select] draws the bag if the player is opening it.
  @param player The player.
  @param bag_textures The textures of the bag.
  @param select The selected item.
*)
let draw_open_bag player bag_textures select =
  if player.action = OpenBag then
    draw_bag player bag_textures select
  
(**
  [draw_game map player enemy items map_textures player_textures enemy_textures items_textures bag_textures select] draws the game.
  @param map The map.
  @param player The player.
  @param enemy The enemy.
  @param items The items.
  @param visibility The grid for shadowcasting
  @param select The selected item.
*)
let draw_game game_states game_textures visibility =
  if is_stairs game_states.traps_and_grounds_state game_states.player_state then begin
    begin_drawing ();
    clear_background Color.black;
    draw_floor_intro game_states.map_state;
    end_drawing ()
  end else begin
    begin_drawing ();
    clear_background Color.black;
    draw_map game_states.map_state game_states.player_state game_textures.tiles_tex;
    draw_trap_ground game_states.traps_and_grounds_state game_states.player_state game_textures.traps_and_grounds_tex;
    draw_items game_states.loots_state game_states.player_state game_textures.items_tex;
    draw_player game_states.player_state game_textures.entities_tex;
    draw_enemy game_states.enemies_state game_textures.entities_tex game_states.player_state;
    draw_shadow_cast game_textures.shadow_cast_tex visibility game_states.player_state (game_states.map_state.width) (game_states.map_state.height);
    draw_player_stats game_states.player_state;
    draw_open_bag game_states.player_state game_textures.bag_tex game_states.player_state.bag.selected_item;
    draw_attack_msg game_states.msgs_state game_textures.attack_msg_tex;
    end_drawing ()
  end


(**
  [check_key_pressed player] checks if a key is pressed.
  @param player The player.
  @return The direction and if a key is pressed.
*)
let check_key_pressed player =
  if player.your_turn && not(player.moving) && player.action = Nothing then begin
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
  [check_key_pressed_action player] checks if a key is pressed for an action.
  @param player The player.
  @return The action and if a key is pressed.
*)
let check_key_pressed_action player =
  if player.your_turn && not(player.moving) then begin
    if is_key_pressed Key.J then begin
      (Attack, true)
    end else if is_key_pressed Key.I then begin
      if player.action <> OpenBag then
        (OpenBag, true)
      else
        (Nothing, true) 
    end else if is_key_pressed Key.T then begin 
      (PickUp, true)
    end else
      (Nothing, false)
  end else
    (Nothing, false)

(**
  [check_key_pressed_bag player select] checks if a key is pressed for the bag.
  @param player The player.
  @param select The selected item.
  @return The action and the selected item.
*)
let check_key_pressed_bag player select =
  let len = (List.length player.bag.items) - 1 in
  if (player.action = OpenBag)  && player.your_turn then begin
    if is_key_pressed Key.Up && select - 7 >= 0 then begin
      (false, select - 7)
    end else if is_key_pressed Key.Down && (select + 7 <= len && select + 7 <= 27) then begin
      (false, select + 7)
    end else if is_key_pressed Key.Left && select - 1 >= 0 then begin
      (false, select - 1)
    end else if is_key_pressed Key.Right && (select + 1 <= len && select + 1 <= 27) then begin
      (false, select + 1)
    end else if is_key_pressed Key.Enter then begin
      (true, select)
    end else
      (false, select)
  end else
    (false, 0)

let check_pickup_item (player: pokemon) (items: loot list) =
  if player.action = PickUp && player.your_turn then begin
    let rec aux x y (list: loot list) =
      match list with
      | [] -> (player, items)
      | item :: rest ->
        if x = item.pos_x && y = item.pos_y then
          let new_player = 
            player
            |> add_item_bag item
            |> set_entity_action Nothing
          in
          let new_list = remove_item_in_list item items in
          (new_player, new_list)
        else
          aux x y rest
    in
    let (pos_x, pos_y) = get_entity_position player in
    match player.direction with
    | Up -> 
        aux pos_x (pos_y -. 1.0) items
    | Down -> 
        aux pos_x (pos_y +. 1.0) items
    | Left -> 
        aux (pos_x -. 1.0) pos_y items
    | Right -> 
        aux (pos_x +. 1.0) pos_y items
    | _ -> (player, items)
  end else
    (player, items)
  
(**
  [update_player player enemy map last_time] updates the player.
  @param player The player.
  @param enemy The enemy.
  @param map The map.
  @param last_time The last time.
  @return The updated player.
*)
let update_player game_states last_time =
  let (game_states, last_time) = update_trap_and_ground game_states last_time in
  let (action , key_pressed) = check_key_pressed_action game_states.player_state in
  let player = action_player action game_states.player_state key_pressed in
  let (player, enemy, _action1, msg) = player_attack player game_states.enemies_state in
  let game_states = add_game_state_msg msg game_states in
  let (player, items) = check_pickup_item player game_states.loots_state in
  let (direction, key_pressed) = check_key_pressed player in
  let player = move direction player key_pressed false in
  
  let ((player, last_update_time), _action2) = new_entity_pos_pre_check game_states.map_state player enemy (List.nth last_time 0) in
  let last_time = replace_nth last_time 0 last_update_time in
  let (player, last_texture_update_time) = increment_texture_id player (List.nth last_time 1) in
  let last_time = replace_nth last_time 1 last_texture_update_time in

  let (player, _action3) = is_end_moving player in
  let (enter, nb_select) = check_key_pressed_bag player game_states.player_state.bag.selected_item in
  let player = 
    if enter then 
      try 
        player
        |> remove_item_bag game_states.player_state.bag.selected_item
        |> set_entity_action Nothing
        |> set_entity_bag_selected 0
      with _ -> player
    else 
      player |> set_entity_bag_selected nb_select
  in
  (* Printf.printf "key_pressed: %b, action1: %b, action2: %b, action3: %b\n%!" key_pressed _action1 _action2 _action3; *)
  let player = if (_action2 && _action3) || _action1 then set_your_turn false player else player in
  let enemy = if (_action2 && _action3) || _action1 then List.map (fun e -> set_your_turn true e) enemy else enemy in
  let game_states = 
    game_states
    |> set_game_state_player player
    |> set_game_state_enemy enemy
    |> set_game_state_loots items
  in
  (game_states, last_time)

(**
  [update_enemy enemies player map key_pressed last_time] updates the enemy.
  @param enemies The enemy.
  @param player The player.
  @param map The map.
  @param key_pressed True if a key is pressed, false otherwise.
  @param last_time The last time.
  @return The updated enemy.
*)
let update_enemy enemy other map last_time =
  let other = List.sort (fun e1 e2 -> compare e1.id e2.id) other in
  let player = List.hd other in
  let enemy = set_entity_path enemy map player in
  let (enemy_target, in_range) = update_target_enemy enemy player in
  let enemy = move enemy_target enemy true in_range in
  let (enemy, players, _action1, msg) = player_attack enemy [player] in
  let player = List.hd players in
  (* Printf.printf "enemy: %d, player: %d posx %f posy %f\n%!" enemy.id player.id enemy.pos_x enemy.pos_y; *)

  let ((enemy, last_update_time), _action2) = new_entity_pos_pre_check map enemy other (List.nth last_time (enemy.id * 2)) in
  let last_time = replace_nth last_time (enemy.id * 2) last_update_time in
  let (enemy, last_texture_update_time) = increment_texture_id enemy (List.nth last_time (enemy.id * 2 + 1)) in
  let last_time = replace_nth last_time (enemy.id * 2 + 1) last_texture_update_time in

  let (enemy, _action3) = is_end_moving enemy in
  (* Printf.printf "action1: %b, action2: %b, action3: %b\n%!" _action1 _action2 _action3; *)
  (* Printf.printf "enemy: %d, player: %d posx %f posy %f\n%!" enemy.id player.id enemy.pos_x enemy.pos_y; *)

  let enemy = if (_action3 && _action2) || _action1 || not _action2 then set_your_turn false enemy else enemy in
  (* Printf.printf "is your turn enemy: %b\n%!" enemy.your_turn; *)
  (enemy, player, last_time, msg) 

let set_your_turn your_turn player =
  set_your_turn your_turn player

(**
  [update_shadow_cast player map] update the shadow casting
  @param player The player.
  @param map The map.
*)
let update_shadow_cast player map =
  compute_fov player 10 map.tiles map.width map.height

(**
  [save_game filename map player enemy] saves the game.
  @param filename The name of the file.
  @param map The map.
  @param player The player.
  @param enemy The enemy.
  @param loots The loots.
  @param traps_and_grounds The traps and grounds.
*)
let save_game filename game_states =
  let json = map_player_to_json game_states.map_state game_states.player_state game_states.enemies_state game_states.loots_state game_states.traps_and_grounds_state in
  write_json_to_file (map_dir ^ filename ^ ".json") json