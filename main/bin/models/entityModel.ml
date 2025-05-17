open Utils.Types
open Raylib
open Combat
open A_star
open Position
open ItemModel
open Utils.Audio
open Utils.Settings_map

(**
  [set_entity_screen screen_x screen_y entity] sets the screen position of an entity.
  @param screen_x: int
  @param screen_y: int
  @param entity: pokemon
  @return entity
*)
let set_entity_screen (screen_x: int) (screen_y: int) (entity: pokemon) : pokemon =
  {entity with position = set_screen screen_x screen_y entity.position}

(**
  [get_entity_screen entity] gets the screen position of an entity.
  @param entity: pokemon
  @return screen_x, screen_y
*)
let get_entity_screen (entity: pokemon) : int * int =
  entity.position.screen_x, entity.position.screen_y

(**
  [set_entity_target target_x target_y entity] sets the target position of an entity.
  @param target_x: float
  @param target_y: float
  @param entity: pokemon
  @return entity
*)
let set_entity_target (target_x: float) (target_y: float) (entity: pokemon) : pokemon =
  {entity with position = set_target target_x target_y entity.position}

(**
  [get_entity_target entity] gets the target position of an entity.
  @param entity: pokemon
  @return target_x, target_y
*)
let get_entity_target (entity: pokemon) : float * float =
  entity.position.target_x, entity.position.target_y

(**
  [set_entity_position pos_x pos_y entity] sets the position of an entity.
  @param pos_x: float
  @param pos_y: float
  @param entity: pokemon
  @return entity
*)
let set_entity_position (pos_x: float) (pos_y: float) (entity: pokemon) : pokemon =
  {entity with position = set_world pos_x pos_y entity.position}

(**
  [get_entity_position entity] gets the position of an entity.
  @param entity: pokemon
  @return pos_x, pos_y
*)
let get_entity_position (entity: pokemon) : float * float =
  entity.position.world_x, entity.position.world_y

(**
  [set_entity_moving moving entity] sets the moving state of an entity.
  @param moving: bool
  @param entity: pokemon
  @return entity
*)
let set_entity_moving (moving: bool) (entity: pokemon) : pokemon =
  {entity with moving = moving}

(**
  [set_entity_texture_id id entity] sets the texture id of an entity.
  @param id : int
  @param entity: pokemon
  @return entity
*)
let set_entity_texture_id (id: int) (entity: pokemon) : pokemon =
  {entity with entity_textures_id = id}

(**
  [set_entity_direction direction entity] sets the direction of an entity.
  @param direction: direction
  @param entity: pokemon
  @return entity
*)
let set_entity_direction (direction: direction) (entity: pokemon) : pokemon =
  {entity with direction = direction}

(**
  [set_entity_state state entity] sets the state of an entity.
  @param state: entityState
  @param entity: pokemon
  @return entity
*)
let set_entity_state (state: entityState) (entity: pokemon) : pokemon =
  {entity with state = state}

(**
  [set_entity_action action entity] sets the action of an entity.
  @param action: interaction
  @param entity: pokemon
  @return entity
*)
let set_entity_action (action: interaction) (entity: pokemon) : pokemon =
  {entity with action = action}
  
(**
  [set_entity_bag bag entity] sets the bag of an entity.
  @param bag: bag
  @param entity: pokemon
  @return entity
*)
let set_entity_bag (bag: bag) (entity: pokemon) : pokemon =
  {entity with bag = bag}

(**
  [set_your_turn your_turn entity] sets the your_turn state of an entity.
  @param your_turn: bool
  @param entity: pokemon
  @return entity
*)
let set_your_turn (your_turn: bool) (entity: pokemon) : pokemon =
  {entity with your_turn = your_turn}

(**
  [set_entity_step_cpt step_cpt entity] sets the step count of an entity.
  @param step_cpt: int
  @param entity: pokemon
  @return entity
*)
let set_entity_step_cpt (step_cpt: int) (entity: pokemon) : pokemon =
  {entity with step_cpt = step_cpt}

(**
  [set_entity_speed speed entity] sets the speed of an entity.
  @param speed: float
  @param entity: pokemon
  @return entity
*)
let set_entity_speed (speed: float) (entity: pokemon) : pokemon =
  {entity with speed = speed}

(**
  [set_entity_current_hp current_hp entity] sets the current hp of an entity.
  @param current_hp: int
  @param entity: pokemon
  @return entity
*)
let set_entity_current_hp (current_hp: int) (entity: pokemon) : pokemon =
  {entity with current_hp = current_hp}

(**
  [set_entity_bag_selected selected entity] sets the selected item in the bag of an entity.
  @param selected: int
  @param entity: pokemon
  @return entity
*)
let set_entity_bag_selected (selected: int) (entity: pokemon) : pokemon =
  {entity with bag = {entity.bag with selected_item = selected}}

(**
  [get_i_competence i entity] gets the i th competence of an entity.
  @param i: int
  @param entity: pokemon
  @return competence
*)
let get_i_competence (i: int) (entity: pokemon) : competence =
  List.nth entity.competence i

(**
  [update_a_star tiles start goal] updates the A* pathfinding for an entity.
  @param tiles: tile list
  @param start: pokemon
  @param goal: pokemon
  @return entity
*)
let update_a_star (tiles : tile list) (start : pokemon) (goal : pokemon) : pokemon =
  let (start_x, start_y) = get_entity_position start in
  let (goal_x, goal_y) = get_entity_position goal in
  { start with
    path = a_star tiles (int_of_float start_x, int_of_float start_y) (int_of_float goal_x, int_of_float goal_y);
  }

(**
  [set_entity_target_with_direction dir entity] sets the target position of an entity based on the direction.
  @param dir: direction
  @param entity: pokemon
  @return entity
*)
let set_entity_target_with_direction (dir : direction) (entity: pokemon) : pokemon =
  let (target_x, target_y) = get_entity_target entity in
  match dir with
  | Up -> set_entity_target (target_x) (target_y -. 1.0) entity
  | Down -> set_entity_target (target_x) (target_y +. 1.0) entity
  | Left -> set_entity_target (target_x -. 1.0) (target_y) entity
  | Right -> set_entity_target (target_x +. 1.0) (target_y) entity
  | DiagonalUpLeft -> set_entity_target (target_x -. 1.0) (target_y -. 1.0) entity
  | DiagonalUpRight -> set_entity_target (target_x +. 1.0) (target_y -. 1.0) entity
  | DiagonalDownLeft -> set_entity_target (target_x -. 1.0) (target_y +. 1.0) entity
  | DiagonalDownRight -> set_entity_target (target_x +. 1.0) (target_y +. 1.0) entity
  | No_move -> entity

(**
  [add_competence competence entity] adds a competence to the entity.
  @param competence: competence
  @param entity: pokemon
  @return entity
*)
let add_competence (competence: competence) (entity: pokemon) : pokemon =
  let new_competence = competence :: entity.competence in
  {entity with competence = new_competence}

(**
  [set_i_competence_puissance i puissance entity] sets the power of the i th competence of the entity.
  @param i: int
  @param puissance: int
  @param entity: pokemon
  @return entity
*)
let set_i_competence_puissance (i: int) (puissance: int) (entity: pokemon) : pokemon =
  if i < 0 || i >= List.length entity.competence then
    raise (Invalid_argument "Index out of bounds")
  else
    let new_competence = List.mapi (fun j c -> if j = i then {c with puissance = puissance} else c) entity.competence in
    {entity with competence = new_competence}

(**
  [set_entity_path entity map player] sets the path of the entity using A* algorithm.
  @param entity: pokemon
  @param map: map
  @param player: pokemon
  @return entity
*)
let set_entity_path (entity: pokemon) (map: map) (player: pokemon) : pokemon =
  let (pos_x, pos_y) = get_entity_position entity in
  let (player_x, player_y) = get_entity_position player in
  if (List.length entity.path <= 2 || manhattan_distance ((int_of_float (floor pos_x), int_of_float (floor pos_y)), (int_of_float (floor player_x), int_of_float (floor player_y))) <= range_a_star_enemy)  && entity.your_turn && not(entity.moving) then
    if entity.your_turn && not(entity.moving) then
      update_a_star map.tiles entity player
    else
      entity
  else
    entity

(**
  [pop_entity_path entity] removes the first element of the path of the entity.
  @param entity: pokemon
  @return entity with path without the first pair
*)
let pop_entity_path (entity: pokemon) : pokemon =
  match entity.path with
  | [] -> entity
  | _ :: rest -> { entity with path = rest }

(**
  [add_item_bag item entity] adds an item to the entity bag.
  @param item: loot
  @param entity: pokemon
  @return entity
*)
let add_item_bag (item: loot) (entity: pokemon) : pokemon =
  let bag = entity.bag in
  let new_items = item :: bag.items in
  let new_bag = {items = new_items; max_size = bag.max_size; selected_item = bag.selected_item} in
  set_entity_bag new_bag entity

(**
  [set_usable_item_bag index usable entity] sets the usability of an item in the entity bag.
  @param index: int
  @param usable: bool
  @param entity: pokemon
  @return entity
*)
let set_usable_item_bag (index : int) (usable: bool) (entity: pokemon) : pokemon =
  if index < 0 || index >= List.length entity.bag.items then
    raise (Invalid_argument "Item not found in bag")
  else
    let bag = entity.bag in
    let new_items = 
      let rec aux i acc lst =
        match lst with
        | [] -> List.rev acc
        | x :: xs ->
          if i = index then List.rev_append acc ((set_item_usable x usable ) :: xs)
          else aux (i + 1) (x :: acc) xs
      in
      aux 0 [] bag.items
    in
    let new_bag = {items = new_items; max_size = bag.max_size; selected_item = bag.selected_item} in
    { entity with bag = new_bag }

(**
  [get_i_item_bag i entity] gets the i th item in the entity bag.
  @param i: int
  @param entity: pokemon
  @return item
*)
let get_i_item_bag (i: int) (entity: pokemon) : loot =
  if i < 0 || i >= List.length entity.bag.items then
    raise (Invalid_argument "Item not found in bag")
  else
    List.nth entity.bag.items i

(**
  [remove_item_bag nth entity] removes the nth item from the entity bag.
  @param nth: int
  @param entity: pokemon
  @return entity
*)
let remove_item_bag (nth: int) (entity: pokemon) : pokemon * loot =
  if nth < 0 || nth >= List.length entity.bag.items then
    raise (Invalid_argument "Item not found in bag")
  else
    let bag = entity.bag in
    let (new_items, item) = 
      let rec aux (i : int) (acc : loot list) (lst : loot list) : loot list * int =
        match lst with
        | [] -> (List.rev acc, -1)
        | x :: xs ->
          if i = nth then (List.rev_append acc xs, i)
          else aux (i + 1) (x :: acc) xs
      in
      aux 0 [] bag.items
    in
    let item_use = List.nth bag.items item in
    let entity = item_effets entity item_use in
    let entity = if item_use.description = "Super Bonbon" then level_up entity else entity in
    let new_bag = {items = new_items; max_size = bag.max_size; selected_item = bag.selected_item} in
    (set_entity_bag new_bag entity, item_use)

(**
  [is_enemy_at_target target_x target_y enemys] checks if there is an enemy at the target position.
  @param target_x The x coordinate of the target position.
  @param target_y The y coordinate of the target position.
  @param enemys The list of enemies.
  @return True if there is an enemy at the target position, false otherwise.
*)
let is_enemy_at_target (target_x: int) (target_y: int) (enemys: pokemon list) : bool =
  List.exists (
    fun (e: pokemon) -> 
      let (e_pos_x, e_pos_y) = get_entity_position e in
      int_of_float e_pos_x = target_x && int_of_float e_pos_y = target_y
  ) enemys

(**
  [is_wall_at_target target_x target_y tiles] checks if there is a wall at the target position.
  @param target_x The x coordinate of the target position.
  @param target_y The y coordinate of the target position.
  @param tiles The list of tiles.
  @return True if there is a wall at the target position, false otherwise.
*)
let is_wall_at_target (target_x: int) (target_y: int) (tiles: tile list) : bool =
  not (List.exists (
    fun tile -> tile.x = target_x && tile.y = target_y && tile.texture_id = 1
  ) tiles)

(**
  [is_targeted_by_enemy target_x target_y enemys] checks if the target position is targeted by an enemy.
  @param target_x The x coordinate of the target position.
  @param target_y The y coordinate of the target position.
  @param enemys The list of enemies.
  @return True if the target position is targeted by an enemy, false otherwise.
*)
let is_targeted_by_enemy (target_x: int) (target_y: int) (enemys: pokemon list) : bool =
  List.exists (
    fun (e: pokemon) ->
      let (e_target_x, e_target_y) = get_entity_target e in
      int_of_float e_target_x = target_x && int_of_float e_target_y = target_y
  ) enemys

(**
  [is_obstacle map entity enemys] checks if the entity is facing an obstacle.
  @param map The map.
  @param entity The entity.
  @param enemys The list of enemies.
  @return True if the entity is facing an obstacle, false otherwise.
*)
let is_obstacle (map: map) (entity: pokemon) (enemys: pokemon list) : bool =
  let tiles = map.tiles in
  let (target_x, target_y) = get_entity_target entity in
  let target_x = int_of_float target_x in
  let target_y = int_of_float target_y in
  is_enemy_at_target target_x target_y enemys ||
  is_wall_at_target target_x target_y tiles ||
  is_targeted_by_enemy target_x target_y enemys

(**
  [move direction entity key_pressed] moves the entity in the given direction.
  @param direction The direction.
  @param entity The entity.
  @param key_pressed True if a key is pressed, false otherwise.
  @param _in_range True if the entity is in range, false otherwise.
  @return The updated entity.
*)
let move (direction : direction) (entity : pokemon) (key_pressed : bool) (_in_range : bool) : pokemon =
  if key_pressed && not _in_range && entity.action <> OpenBag && entity.action <> PickUp && not(entity.moving) && entity.your_turn then begin
    let new_entity = 
      entity
      |> set_entity_target_with_direction direction
      |> set_entity_direction direction
      |> set_entity_moving true
      |> set_entity_state Moving
    in
    new_entity
  end else if _in_range && key_pressed && entity.your_turn then
    let new_entity = 
    entity
    |> set_entity_direction direction
    |> set_entity_state Idle
    |> set_entity_action Attack
    |> set_entity_moving false
    in
    new_entity
  else
    entity

(**
  [action_player action entity key_pressed] sets the action of the entity.
  @param action The action.
  @param entity The entity.
  @param key_pressed True if a key is pressed, false otherwise.
  @return The updated entity.
*)
let action_player (action : interaction) (entity : pokemon) (key_pressed : bool) : pokemon =
  if key_pressed && entity.your_turn then
    match action with
    | Attack -> 
      entity
        |> set_entity_action Attack
    | OpenBag -> 
      entity
        |> set_entity_action OpenBag
    | PickUp -> 
      entity
        |> set_entity_action PickUp
    | Nothing -> 
      entity
        |> set_entity_action Nothing
  else
    entity

(**
  [is_end_moving entity] checks if the pokemon has reached its target position.
  @param entity The pokemon.
  @return The updated pokemon.
*)
let is_end_moving (entity: pokemon) : pokemon * bool =
  let (pos_x, pos_y) = get_entity_position entity in
  let (target_x, target_y) = get_entity_target entity in
  if pos_x = target_x && pos_y = target_y && entity.moving then
    (entity
    |> set_entity_moving false
    |> set_entity_action Nothing,
    true)
  else 
    entity, false

(**
  [new_entity_pos map entity last_update_time] updates the entity position.
  @param entity The entity.
  @param last_update_time The last time the entity position was updated.
  @return The updated entity and the last time the entity position was updated.
*)
let new_entity_pos (entity : pokemon) (last_update_time : float) : pokemon * float=
  let current_time = get_time () in
  
  (* Check if the entity is facing a wall *)
  let new_entity = 
    let (pos_x, pos_y) = get_entity_position entity in
    if entity.step_cpt > 0 && current_time -. last_update_time >= 3.0 then
      entity
      |> set_entity_step_cpt (entity.step_cpt - 1)
      |> set_entity_moving false
      |> set_entity_target (floor pos_x) (floor pos_y)
    else
      entity
  in
  (* Update the entity state *)
  let new_entity = 
    if new_entity.moving then
      new_entity
      |> set_entity_state Moving
    else
      new_entity
      |> set_entity_state Idle
  in
  (* new_entity *)

  if current_time -. last_update_time >= 0.01 && new_entity.moving && new_entity.step_cpt = 0 then
    let (new_pos_x, new_pos_y) = get_entity_position new_entity in
    let (new_target_x, new_target_y) = get_entity_target new_entity in
    let dx = new_target_x -. new_pos_x in
    let dy = new_target_y -. new_pos_y in
    let step = 0.05 in
    let new_x = if abs_float dx < step then new_target_x else new_pos_x +. (if dx > 0.0 then step else -.step) in
    let new_y = if abs_float dy < step then new_target_y else new_pos_y +. (if dy > 0.0 then step else -.step) in
    (* Printf.printf "Entity %d pos_x: %f pos_y: %f target_x: %f target_y: %f\n%!" new_entity.id new_entity.pos_x new_entity.pos_y new_entity.target_x new_entity.target_y; *)
    (* Printf.printf "new_x: %f new_y: %f\n\n%!" new_x new_y; *)
    let new_entity = 
        new_entity
        |> set_entity_position new_x new_y
    in
    (new_entity, current_time)
  else
    (new_entity, last_update_time)

(**
  [new_entity_pos_pre_check map entity enemy last_update_time] checks if the entity is facing an obstacle and updates its position.
  @param map The map.
  @param entity The entity.
  @param enemy The enemy.
  @param last_update_time The last time the entity position was updated.
  @return The updated entity and a boolean indicating if the entity is moving.
*)
let new_entity_pos_pre_check (map : map) (entity : pokemon) (enemy : pokemon list) (last_update_time : float) : (pokemon * float) * bool =
  if entity.direction != No_move && entity.moving then
    let obstacle = is_obstacle map entity enemy in
      if obstacle then
        let (pos_x, pos_y) = get_entity_position entity in
        let new_entity = 
          entity
          |> set_entity_moving false
          |> set_entity_target (floor pos_x) (floor pos_y)
        in
        (new_entity_pos new_entity last_update_time), false
      else
        let (target_x, target_y) = get_entity_target entity in
        let new_entity = 
          entity
          |> set_entity_moving true
          |> set_entity_target (floor target_x) (floor target_y)
        in
        (new_entity_pos new_entity last_update_time), true
  else
    (entity, last_update_time), false
        

(**
  [update_entity_texture_id entity] updates the texture id of the entity.
  @param entity: pokemon
  @return entity
*)
let update_entity_texture_id (entity: pokemon) : pokemon =
  let current_id = entity.entity_textures_id in
  if current_id > 40 then (* 40 is the last id for the entity textures *)
    set_entity_texture_id entity.entity_textures_id entity
  else begin
    let base_id = match entity.state with
      | Moving -> 16 (* 16 is the base id for moving textures *)
      | Idle -> 0 (* 0 is the base id for idle textures *)
    in
    let direction_offset = match entity.direction with
      | Down -> 0
      | Up -> 1
      | Left -> 2
      | Right -> 3
      | DiagonalUpLeft -> 4
      | DiagonalUpRight -> 5
      | DiagonalDownLeft -> 6
      | DiagonalDownRight -> 7
      | No_move -> -1
    in
    let next_id = match entity.state with
      | Moving -> if ((current_id + 1 > (base_id + direction_offset * 3)) && (current_id + 1 <= (base_id + direction_offset * 3 + 2))) then current_id + 1 else base_id + direction_offset * 3
      | Idle -> if ((current_id + 1 > (base_id + direction_offset * 2)) && (current_id + 1 <= (base_id + direction_offset * 2 + 1))) then current_id + 1 else base_id + direction_offset * 2
    in
    set_entity_texture_id next_id entity
  end

(**
  [increment_texture_id entity last_texture_update_time] increments the texture id of the player.
  @param entity The entity.
  @param last_texture_update_time The last time the texture was updated.
  @return The updated entity and the last time the texture was updated.
*)
let increment_texture_id (entity : pokemon) (last_texture_update_time : float) : pokemon * float =
  if get_time () -. last_texture_update_time >= 0.2 then begin
    (update_entity_texture_id entity, get_time ())
  end
  else begin
    (entity, last_texture_update_time)
  end

(**
  [set_enemys_action action entitys] sets the action of all enemies.
  @param action The action to set.
  @param entitys The list of enemies.
  @return The updated list of enemies.
*)
let set_enemys_action (action : interaction) (entitys: pokemon list) : pokemon list =
  List.map (fun (e: pokemon) ->
    set_entity_action action e
  ) entitys

(**
  [player_get_target player] gets the target position of the player based on its direction. 
  @param player The player.
  @return The target position (x, y).
*)
let player_get_target (player: pokemon) : float * float =
  let (pos_x, pos_y) = get_entity_position player in
  match player.direction with
  | Up -> (pos_x, pos_y -. 1.0)
  | Down -> (pos_x, pos_y +. 1.0)
  | Left -> (pos_x -. 1.0, pos_y)
  | Right -> (pos_x +. 1.0, pos_y)
  | DiagonalUpLeft -> (pos_x -. 1.0, pos_y -. 1.0)
  | DiagonalUpRight -> (pos_x +. 1.0, pos_y -. 1.0)
  | DiagonalDownLeft -> (pos_x -. 1.0, pos_y +. 1.0)
  | DiagonalDownRight -> (pos_x +. 1.0, pos_y +. 1.0)
  | No_move -> (pos_x, pos_y)

(**
  [player_attack player enemy] handles the attack action of the player.
  @param player The player.
  @param enemy The list of enemies.
  @return The updated player, the updated list of enemies, a boolean indicating if the attack was successful, and a message.
*)
let player_attack (player: pokemon) (enemy: pokemon list) : pokemon * pokemon list * bool * string =
  if player.action = Attack && player.your_turn && not(player.moving) then begin
    let target_x, target_y = player_get_target player in
    let rec aux (player : pokemon) (enemy : pokemon list) (acc : pokemon list) (msg : string) : pokemon * pokemon list * bool * string =
      match enemy with
      | [] -> (player |> set_entity_action Nothing, acc, true, msg)
      | e :: rest ->
        let (e_pos_x, e_pos_y) = get_entity_position e in
        let choose_competence = List.nth player.competence (Random.int (List.length player.competence)) in
        if int_of_float e_pos_x = int_of_float target_x && int_of_float e_pos_y = int_of_float target_y then begin
          play_sound "resources/audio/sound/attack.mp3";
          let degat = calcul_degats player e choose_competence in
          let msg = Printf.sprintf "%s utilise %s inflige %d dégâts à %s !\n%!" player.nom choose_competence.name degat e.nom in
          let new_enemy = {e with current_hp = e.current_hp - degat} in
          if new_enemy.current_hp <= 0 then begin
            let updated_player = is_level_up player new_enemy in
            aux updated_player rest acc msg
          end else begin
            aux player rest (new_enemy :: acc) msg
          end
        end else begin
          aux player rest (e :: acc) msg
        end
    in
    aux player enemy [] ""
  end else begin
    (player, enemy, false, "")
  end



