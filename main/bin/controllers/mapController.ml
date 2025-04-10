open Raylib
open Models.EntityModel
open Models.EnemyModel
open Models.ItemModel
open Models.Shadowcaster
open Models.Trap_ground
open Models.Generation_map
open Views.PlayerView
open Views.MapView
open Views.EnemyView
open Views.ItemView
open Views.BagView
open Views.TrapGroundView
open Views.ShadowCastView
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
  let player_textures = init_player_textures () in
  let enemy_textures = init_enemy_textures () in
  let items_textures = init_items_textures () in
  let bag_textures = init_bag_textures items_textures in
  let shadow_cast_texture = init_shadow_cast_view () in
  let trap_and_ground_texures = init_trap_ground_textures () in
  let (map, player, enemy, loots, traps_and_grounds) = load_map_player_from_json (map_dir ^ filename ^ ".json") in
  let new_player = 
    player
    |> set_entity_screen (screen_width / 2) (screen_height / 2)
    |> set_entity_texture_id 24
  in
  (map_textures, player_textures, enemy_textures, items_textures, bag_textures, shadow_cast_texture, trap_and_ground_texures, map, new_player, enemy, loots, traps_and_grounds)

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
let draw_game map (player: pokemon) enemy (items : loot list) visibility traps_and_grounds map_textures player_textures enemy_textures items_textures bag_textures shadow_cast_texture trap_and_ground_texures select =
  begin_drawing ();
  clear_background Color.black;
  draw_map map player map_textures;
  draw_trap_ground traps_and_grounds player trap_and_ground_texures;
  draw_items items player items_textures;
  draw_player player player_textures;
  draw_enemy enemy enemy_textures player;
  draw_shadow_cast shadow_cast_texture visibility player (map.width) (map.height);
  draw_player_stats player;
  draw_open_bag player bag_textures select;
  (* Uncomment the following lines to print the player and enemy positions for debugging purposes *)
  (* Printf.printf "Player position: (%f, %f), Target: (%f, %f)\n" player.pos_x player.pos_y player.target_x player.target_y;
  List.iter (fun (enemy: pokemon) ->
    Printf.printf "Enemy position: (%f, %f)\n" enemy.pos_x enemy.pos_y
  ) enemy; *)
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
  [check_key_pressed_action player] checks if a key is pressed for an action.
  @param player The player.
  @return The action and if a key is pressed.
*)
let check_key_pressed_action player =
  if not(player.moving) then begin
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
  if (player.action = OpenBag) then begin
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
  if player.action = PickUp then begin
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
let update_trap_and_ground map player trap_and_ground enemys items =
  if is_stairs trap_and_ground player then
    generation_map ()
  else
    (map, player, trap_and_ground, enemys, items)
  
(**
  [update_player player enemy map last_time] updates the player.
  @param player The player.
  @param enemy The enemy.
  @param map The map.
  @param last_time The last time.
  @return The updated player.
*)
let update_player (player: pokemon) enemy map last_time select (items: loot list) trap_and_ground =
  let (map, player, trap_and_ground, enemy, items) = update_trap_and_ground map player trap_and_ground enemy items in
  let (action , key_pressed) = check_key_pressed_action player in
  let player = action_player action player key_pressed in
  let (player, items) = check_pickup_item player items in
  let (direction, key_pressed) = check_key_pressed player in
  let player = move direction player key_pressed in
  let enemy = player_attack player enemy in
  let enemy = set_enemys_action action enemy in
  let (player, last_update_time) = new_entity_pos map player enemy (List.nth last_time 0) in
  let last_time = replace_nth last_time 0 last_update_time in
  let (player, last_texture_update_time) = increment_texture_id player (List.nth last_time 1) in
  let last_time = replace_nth last_time 1 last_texture_update_time in
  let player = is_end_moving player in
  let (enter, nb_select) = check_key_pressed_bag player select in
  let player = 
    if enter then 
      player
      |> remove_item_bag select
      |> set_entity_action Nothing
    else 
      player 
  in
  (map, player, key_pressed, last_time, nb_select, items, trap_and_ground, enemy)

(**
  [update_enemy enemies player map key_pressed last_time] updates the enemy.
  @param enemies The enemy.
  @param player The player.
  @param map The map.
  @param key_pressed True if a key is pressed, false otherwise.
  @param last_time The last time.
  @return The updated enemy.
*)
let update_enemy enemies player map key_pressed last_time =
  let rec aux index enemies updated_enemies last_time =
    match enemies with
    | [] -> (updated_enemies, last_time)
    | enemy :: rest ->
      let enemy_target = update_target_enemy enemy player in
      let enemy = move enemy_target enemy key_pressed in
      let (enemy, last) = new_entity_pos map enemy [player] (List.nth last_time index) in
      let last_time = replace_nth last_time index last in
      let (enemy, last_t) = increment_texture_id enemy (List.nth last_time (index + 1)) in
      let last_time = replace_nth last_time (index + 1) last_t in
      let enemy = is_end_moving enemy in
      aux (index + 2) rest (enemy :: updated_enemies) last_time
  in
  aux 2 enemies [] last_time

(**
  [update_shadow_cast player map] update the shadow casting
  @param player The player.
  @param map The map.
*)
let update_shadow_cast player map =
  compute_fov player 7 map.tiles map.width map.height

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