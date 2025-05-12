open Raylib
open Models.EntityModel
open Models.EnemyModel
open Models.ItemModel
open Models.Shadowcaster
open Models.Trap_ground
open Models.Generation_map
open Models.Map_model
open Views.PlayerView
open Views.EntityView
open Views.MapView
open Views.EnemyView
open Views.ItemView
open Views.BagView
open Views.TrapGroundView
open Views.ShadowCastView
open Views.AttackView
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
  let map_textures = init_map_textures () in
  let entity_textures = init_entity_textures () in
  let items_textures = init_items_textures () in
  let bag_textures = init_bag_textures items_textures in
  let shadow_cast_texture = init_shadow_cast_view () in
  let trap_and_ground_texures = init_trap_ground_textures () in
  let attack_msg_textures = init_attack_msg_textures () in
  let (map, player, enemy, loots, traps_and_grounds) = load_map_player_from_json (map_dir ^ filename ^ ".json") in
  let new_player = 
    player
    |> set_entity_screen (screen_width / 2) (screen_height / 2)
    |> set_entity_texture_id 24
  in
  (map_textures, entity_textures, items_textures, bag_textures, shadow_cast_texture, trap_and_ground_texures, attack_msg_textures, map, new_player, enemy, loots, traps_and_grounds)

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
  @param map_textures The textures of the map.
  @param player_textures The textures of the player.
  @param enemy_textures The textures of the enemy.
  @param items_textures The textures of the items.
  @param bag_textures The textures of the bag.
  @param select The selected item.
*)
let draw_game map (player: pokemon) enemy (items : loot list) visibility traps_and_grounds map_textures entity_textures items_textures bag_textures shadow_cast_texture trap_and_ground_texures attack_msg_textures select msgs =
  if is_stairs traps_and_grounds player then begin
    begin_drawing ();
    clear_background Color.black;
    draw_floor_intro map;
    end_drawing ()
  end else begin
    begin_drawing ();
    clear_background Color.black;
    draw_map map player map_textures;
    draw_trap_ground traps_and_grounds player trap_and_ground_texures;
    draw_items items player items_textures;
    draw_player player entity_textures;
    draw_enemy enemy entity_textures player;
    draw_shadow_cast shadow_cast_texture visibility player (map.width) (map.height);
    draw_player_stats player;
    draw_open_bag player bag_textures select;
    draw_attack_msg msgs attack_msg_textures;
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
    match player.direction with
    | Up -> 
        aux player.pos_x (player.pos_y -. 1.0) items
    | Down -> 
        aux player.pos_x (player.pos_y +. 1.0) items
    | Left -> 
        aux (player.pos_x -. 1.0) player.pos_y items
    | Right -> 
        aux (player.pos_x +. 1.0) player.pos_y items
    | _ -> (player, items)
  end else
    (player, items)

(**
  [update_trap_and_ground map player trap_and_ground enemys items] updates the trap and ground.
  @param map The map.
  @param player The player.
  @param trap_and_ground The trap and ground.
  @param enemys The enemy.
  @param items The items.
  @return The updated map, player, trap and ground, enemy and items.
*)
let update_trap_and_ground map player traps_and_grounds enemys item last_time =
  if is_trap_ground traps_and_grounds (int_of_float player.pos_x) (int_of_float player.pos_y) && player.your_turn then begin
    let pos_x = int_of_float player.pos_x in
    let pos_y = int_of_float player.pos_y in
    match get_trap_ground traps_and_grounds (int_of_float player.pos_x) (int_of_float player.pos_y) with
    | None -> (map, player, traps_and_grounds, enemys, item, last_time)
    | Some trap_and_ground ->
      let traps_and_grounds = set_trap_ground_pos_visibility pos_x pos_y true traps_and_grounds in
      match trap_and_ground.nature, trap_and_ground.visibility with
      | Stairs_Up, true -> 
        Unix.sleep 1;
        let (new_map, new_player, new_trap_and_ground, new_enemys, new_items) = generation_map map.floor player.number in
        let list_of_last_time = List.init (7 + ((List.length(new_enemys))*2)) (fun _ -> 0.0) in (* Use for animations *)
        let new_map = set_map_floor new_map (new_map.floor + 1) in
        (new_map, new_player, new_trap_and_ground, new_enemys, new_items, list_of_last_time)
      | Stairs_Down, true ->
        Unix.sleep 1;
        let (new_map, new_player, new_trap_and_ground, new_enemys, new_items) = generation_map map.floor player.number in
        let list_of_last_time = List.init (7 + ((List.length(new_enemys))*2)) (fun _ -> 0.0) in (* Use for animations *)
        let new_map = set_map_floor new_map (new_map.floor - 1) in
        (new_map, new_player, new_trap_and_ground, new_enemys, new_items, list_of_last_time)
      | Bug_Switch, false ->
        let player = set_entity_step_cpt 3 player in
        (map, player, traps_and_grounds, enemys, item, last_time)
      | Chestnut_Switch, false ->
        let player = set_entity_current_hp (player.current_hp - 10) player in
        (map, player, traps_and_grounds, enemys, item, last_time)
      | Drop_Hole, false ->
        (* FIXME *)
        (map, player, traps_and_grounds, enemys, item, last_time)
      | Explosion_Switch, false ->
        let player = set_entity_current_hp (player.current_hp - 20) player in
        (map, player, traps_and_grounds, enemys, item, last_time)
      | Fan_Switch, false ->
        let random_x = Random.int 4 in
        let direction = 
          match random_x with
          | 0 -> 
            Right
          | 1 -> 
            Left
          | 2 -> 
            Up
          | 3 -> 
            Down
          | _ -> 
            Down
        in
        let player = 
          match find_wall_in_direction pos_x pos_y direction map with
              | Some (x, y) -> 
                let x = float_of_int x in
                let y = float_of_int y in
                player 
                |> set_entity_pos x y
                |> set_entity_target_pos x y
              | None -> player
        in
        (map, player, traps_and_grounds, enemys, item, last_time)    
      | Glue_Switch, false ->
        let length = List.length player.bag.items in
        if length = 0 then 
          (map, player, traps_and_grounds, enemys, item, last_time)
        else if length = 1 then
          let player = set_usable_item_bag 0 false player in
          (map, player, traps_and_grounds, enemys, item, last_time)
        else
          let rand = Random.int length in
          let player = set_usable_item_bag rand false player in
          (map, player, traps_and_grounds, enemys, item, last_time)
      | Grimer_Switch, false ->
        (* FIXME *)
        (map, player, traps_and_grounds, enemys, item, last_time)
      | Imprison_Switch, false ->
        let player = set_entity_current_hp (player.current_hp - 1) player in
        (map, player, traps_and_grounds, enemys, item, last_time)
      | Mud_Switch, false ->
        (* FIXME *)
        (map, player, traps_and_grounds, enemys, item, last_time)
      | Poison_Sting_Switch, false ->
        let player = set_entity_current_hp (player.current_hp - 1) player in
        (map, player, traps_and_grounds, enemys, item, last_time)
      | Pokemon_Switch, false ->
        (* FIXME *)
        (map, player, traps_and_grounds, enemys, item, last_time)
      | Self_Destruct_Switch, false ->
        let player = set_entity_current_hp (player.current_hp - 20) player in
        let map = set_map_exploded pos_x pos_y map in
        (map, player, traps_and_grounds, enemys, item, last_time)
      | Skill_Drop_Switch, false ->
        (* FIXME *)
        (map, player, traps_and_grounds, enemys, item, last_time)
      | Slowpoke_Switch, false ->
        let player = set_entity_speed 0.5 player in
        (map, player, traps_and_grounds, enemys, item, last_time)
      | Spin_Swith, false ->
        let player = set_entity_step_cpt 3 player in
        (map, player, traps_and_grounds, enemys, item, last_time)
      | Summon_Switch, false ->
        let rec add_random_enemy player map enemys =
          let random_x = player.pos_x +. float_of_int (Random.int 11 - 5) in
          let random_y = player.pos_y +. float_of_int (Random.int 11 - 5) in
          if not(is_wall (int_of_float random_x) (int_of_float random_y) map) then
            let new_enemy, player = create_enemy random_x random_y player in
            new_enemy :: enemys, player
          else
            add_random_enemy player map enemys
        in
        let last_time = last_time @ [0.0; 0.0] in
        let enemys, player = add_random_enemy player map enemys in
        (map, player, traps_and_grounds, enemys, item, last_time)
      | Warp_Trap, false ->
        let rec find_random_position () =
          let random_x = float_of_int (Random.int map.width) in
          let random_y = float_of_int (Random.int map.height) in
          if not (is_wall (int_of_float random_x) (int_of_float random_y) map) && not (List.exists (fun e -> e.pos_x = random_x && e.pos_y = random_y) enemys) then
            (random_x, random_y)
          else
            find_random_position ()
        in
        let (new_x, new_y) = find_random_position () in
        let player =
          player
          |> set_entity_pos new_x new_y
          |> set_entity_target_pos new_x new_y
        in
        (map, player, traps_and_grounds, enemys, item, last_time)
      | _ ->
        (map, player, traps_and_grounds, enemys, item, last_time)
  end else
    (map, player, traps_and_grounds, enemys, item, last_time)

  
(**
  [update_player player enemy map last_time] updates the player.
  @param player The player.
  @param enemy The enemy.
  @param map The map.
  @param last_time The last time.
  @return The updated player.
*)
let update_player player enemy map select items trap_and_ground last_time =
  let (map, player, trap_and_ground, enemy, items, last_time) = update_trap_and_ground map player trap_and_ground enemy items last_time in
  let (action , key_pressed) = check_key_pressed_action player in
  let player = action_player action player key_pressed in
  let (player, enemy, _action1, msg) = player_attack player enemy in
  let (player, items) = check_pickup_item player items in
  let (direction, key_pressed) = check_key_pressed player in
  let player = move direction player key_pressed false in
  
  let ((player, last_update_time), _action2) = new_entity_pos_pre_check map player enemy (List.nth last_time 0) in
  let last_time = replace_nth last_time 0 last_update_time in
  let (player, last_texture_update_time) = increment_texture_id player (List.nth last_time 1) in
  let last_time = replace_nth last_time 1 last_texture_update_time in

  let (player, _action3) = is_end_moving player player map in
  let (enter, nb_select) = check_key_pressed_bag player select in
  let player = 
    if enter then 
      player
      |> remove_item_bag select
      |> set_entity_action Nothing
    else 
      player 
  in
(*  *)
  (* Printf.printf "key_pressed: %b, action1: %b, action2: %b, action3: %b\n%!" key_pressed _action1 _action2 _action3; *)
  let player = if (_action2 && _action3) || _action1 then set_your_turn false player else player in
  let enemy = if (_action2 && _action3) || _action1 then List.map (fun e -> set_your_turn true e) enemy else enemy in

  (player, nb_select, items, enemy, trap_and_ground, last_time, msg)

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

  let (enemy, _action3) = is_end_moving enemy player map in
  (* Printf.printf "action1: %b, action2: %b, action3: %b\n%!" _action1 _action2 _action3; *)
  (* Printf.printf "enemy: %d, player: %d posx %f posy %f\n%!" enemy.id player.id enemy.pos_x enemy.pos_y; *)

  let enemy = if (_action3 && _action2) || _action1 || not _action2 then set_your_turn false enemy else enemy in
  (* Printf.printf "is your turn enemy: %b\n%!" enemy.your_turn; *)
  (enemy, player, last_time, msg) 


let add_msg msg msgs =
  if msg = "" then
    msgs
  else
    msgs @ [msg]

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
let save_game filename map player enemy loots traps_and_grounds =
  let json = map_player_to_json map player enemy loots traps_and_grounds in
  write_json_to_file (map_dir ^ filename ^ ".json") json