open Utils.Types
open ItemModel
open Raylib
open Combat
open A_star

(**
  Set the entity screen position
  @param screen_x: int
  @param screen_y: int
  @param entity: pokemon
  @return entity
*)
let set_entity_screen (screen_x: int) (screen_y: int) (entity: pokemon) =
  {entity with screen_x = screen_x; screen_y = screen_y}

(**
  Set the entity target position
  @param target_x: float
  @param target_y: float
  @param entity: pokemon
  @return entity
*)
let set_entity_target (dir : direction) (entity: pokemon) =
  match dir with
  | Up -> {entity with target_x = entity.pos_x; target_y = entity.pos_y -. 1.0}
  | Down -> {entity with target_x = entity.pos_x; target_y = entity.pos_y +. 1.0}
  | Left -> {entity with target_x = entity.pos_x -. 1.0; target_y = entity.pos_y}
  | Right -> {entity with target_x = entity.pos_x +. 1.0; target_y = entity.pos_y}
  | DiagonalUpLeft -> {entity with target_x = entity.pos_x -. 1.0; target_y = entity.pos_y -. 1.0}
  | DiagonalUpRight -> {entity with target_x = entity.pos_x +. 1.0; target_y = entity.pos_y -. 1.0}
  | DiagonalDownLeft -> {entity with target_x = entity.pos_x -. 1.0; target_y = entity.pos_y +. 1.0}
  | DiagonalDownRight -> {entity with target_x = entity.pos_x +. 1.0; target_y = entity.pos_y +. 1.0}
  | No_move -> {entity with target_x = entity.pos_x; target_y = entity.pos_y}

let set_entity_target_pos (target_x: float) (target_y: float) (entity: pokemon) =
  {entity with target_x = target_x; target_y = target_y}

  let set_usable_item_bag (index : int) (usable: bool) (entity: pokemon) =
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
    let new_bag = {items = new_items; max_size = bag.max_size} in
    {
      entity with
      bag = new_bag
    }
  


(**
  Set the entity can moving
  @param moving: bool
  @param entity: pokemon
  @return entity
*)
let set_entity_moving (moving: bool) (entity: pokemon) =
  {entity with moving = moving}


(**
  Set the current entity position
  @param pos_x: float
  @param pos_y: float
  @param entity: pokemon
  @return entity
*)
let set_entity_pos (x: float) (y: float) (entity: pokemon) =
  {entity with pos_x = x; pos_y = y}


(**
  Set the entity direction
  @param id : int
  @param entity: pokemon
  @return entity
*)
let set_entity_texture_id (id: int) (entity: pokemon) =
  {entity with entity_textures_id = id}

(**
  Set the entity direction
  @param direction: direction
  @param entity: pokemon
  @return entity
*)
let set_entity_direction (direction: direction) (entity: pokemon) =
  {entity with direction = direction}

(**
  Set the entity state
  @param state: entityState
  @param entity: pokemon
  @return entity
*)
let set_entity_state (state: entityState) (entity: pokemon) =
  {entity with state = state}

(**
  Set the entity action
  @param action: interaction
  @param entity: pokemon
  @return entity
*)
let set_entity_action (action: interaction) (entity: pokemon) =
  {entity with action = action}
  
(**
  Set the entity bag
  @param bag: bag
  @param entity: pokemon
  @return entity
*)
let set_entity_bag (bag: bag) (entity: pokemon) =
  {entity with bag = bag}

let set_entity_path (entity: pokemon) (map: map) (player: pokemon) =
  if entity.your_turn && not(entity.moving) then
    update_a_star map.tiles entity player
  else
    entity

let set_your_turn (your_turn: bool) (entity: pokemon) =
  {entity with your_turn = your_turn}

let set_entity_step_cpt (step_cpt: int) (entity: pokemon) =
  {entity with step_cpt = step_cpt}

let set_entity_speed (speed: float) (entity: pokemon) =
  {entity with speed = speed}

let set_entity_current_hp (current_hp: int) (entity: pokemon) =
  {entity with current_hp = current_hp}


(**
  Add an item to the entity bag
  @param item: loot
  @param entity: pokemon
  @return entity
*)
let add_item_bag (item: loot) (entity: pokemon) =
  let bag = entity.bag in
  let new_items = item :: bag.items in
  let new_bag = {items = new_items; max_size = bag.max_size} in
  set_entity_bag new_bag entity

(**
  Remove an item from the entity bag
  @param nth: int
  @param entity: pokemon
  @return entity
*)
let remove_item_bag (nth: int) (entity: pokemon) =
  let bag = entity.bag in
  let new_items = 
    let rec aux i acc lst =
      match lst with
      | [] -> List.rev acc
      | x :: xs ->
        if i = nth then List.rev_append acc xs
        else aux (i + 1) (x :: acc) xs
    in
    aux 0 [] bag.items
  in
  let new_bag = {items = new_items; max_size = bag.max_size} in
  set_entity_bag new_bag entity

(**
  [is_objstacle map entity] checks if the entity is facing a wall.
  @param map The map.
  @param entity The entity.
  @param enemy The enemy.
  @return True if the entity is facing a wall, false otherwise.
*)
let is_obstacle map (entity: pokemon) (enemy: pokemon list) =
  let tiles = map.tiles in
  let target_x = int_of_float entity.target_x in
  let target_y = int_of_float entity.target_y in
  List.exists (fun (e: pokemon) -> int_of_float e.pos_x = target_x && int_of_float e.pos_y = target_y) enemy ||
  not (List.exists (fun tile -> tile.x = target_x && tile.y = target_y && tile.texture_id = 1) tiles)

(**
  [move direction entity key_pressed] moves the entity in the given direction.
  @param direction The direction.
  @param entity The entity.
  @param key_pressed True if a key is pressed, false otherwise.
  @return The updated entity.
*)
let move direction (entity: pokemon) key_pressed _in_range =
  if key_pressed && not _in_range && entity.action <> OpenBag && entity.action <> PickUp && not(entity.moving) then begin
    let new_entity = 
      entity
      |> set_entity_target direction
      |> set_entity_direction direction
      |> set_entity_moving true
      |> set_entity_state Moving
    in
    new_entity
  end else if _in_range && key_pressed then
    let new_entity = 
    entity
    |> set_entity_target No_move
    |> set_entity_direction direction
    |> set_entity_state Idle
    |> set_entity_action Attack
    |> set_entity_moving false
    in
    new_entity
  else
    entity

let action_player action (entity: pokemon) key_pressed =
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
let is_end_moving (entity: pokemon) (_target: pokemon) (_map: map) =
  if entity.pos_x = entity.target_x && entity.pos_y = entity.target_y && entity.moving then 
    (entity
    |> set_entity_moving false
    |> set_entity_action Nothing,
    true)
  else 
    entity, false

(**
  [new_entity_pos map entity last_update_time] updates the entity position.
  @param map The map.
  @param entity The entity.
  @param enemy The enemy.
  @param last_update_time The last time the entity position was updated.
  @return The updated entity and the last time the entity position was updated.
*)
let new_entity_pos _map entity (_enemy: pokemon list) last_update_time =
  let current_time = get_time () in
  
  (* Check if the entity is facing a wall *)
  let new_entity = 
    if entity.step_cpt > 0 && current_time -. last_update_time >= 3.0 then
      entity
      |> set_entity_step_cpt (entity.step_cpt - 1)
      |> set_entity_moving false
      |> set_entity_target_pos (floor entity.pos_x) (floor entity.pos_y)
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
    let dx = new_entity.target_x -. new_entity.pos_x in
    let dy = new_entity.target_y -. new_entity.pos_y in
    let step = 0.05 in
    let new_x = if abs_float dx < step then new_entity.target_x else new_entity.pos_x +. (if dx > 0.0 then step else -.step) in
    let new_y = if abs_float dy < step then new_entity.target_y else new_entity.pos_y +. (if dy > 0.0 then step else -.step) in
    (* Printf.printf "Entity %d pos_x: %f pos_y: %f target_x: %f target_y: %f\n%!" new_entity.id new_entity.pos_x new_entity.pos_y new_entity.target_x new_entity.target_y; *)
    (* Printf.printf "new_x: %f new_y: %f\n\n%!" new_x new_y; *)
    let new_entity = 
        new_entity
        |> set_entity_pos new_x new_y
    in
    (new_entity, current_time)
  else
    (new_entity, last_update_time)


let new_entity_pos_pre_check map entity enemy last_update_time =
  if entity.direction != No_move && entity.moving then
    let obstacle = is_obstacle map entity enemy in
      if obstacle then
        let new_entity = 
          entity
          |> set_entity_moving false
          |> set_entity_target_pos (floor entity.pos_x) (floor entity.pos_y)
        in
        (new_entity_pos map new_entity enemy last_update_time), false
      else
        let new_entity = 
          entity
          |> set_entity_moving true
          |> set_entity_target_pos (floor entity.target_x) (floor entity.target_y)
        in
        (new_entity_pos map new_entity enemy last_update_time), true
  else
    (entity, last_update_time), false
        

(**
  Update the entity texture id
  @param entity: pokemon
  @return entity
*)
let update_entity_texture_id (entity: pokemon) =
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
let increment_texture_id entity last_texture_update_time =
  if get_time () -. last_texture_update_time >= 0.2 then begin
    (update_entity_texture_id entity, get_time ())
  end
  else begin
    (entity, last_texture_update_time)
  end

let set_enemys_action action (entitys: pokemon list) =
  List.map (fun (e: pokemon) ->
    set_entity_action action e
  ) entitys

let player_get_target (player: pokemon) =
  match player.direction with
  | Up -> (player.pos_x, player.pos_y -. 1.0)
  | Down -> (player.pos_x, player.pos_y +. 1.0)
  | Left -> (player.pos_x -. 1.0, player.pos_y)
  | Right -> (player.pos_x +. 1.0, player.pos_y)
  | DiagonalUpLeft -> (player.pos_x -. 1.0, player.pos_y -. 1.0)
  | DiagonalUpRight -> (player.pos_x +. 1.0, player.pos_y -. 1.0)
  | DiagonalDownLeft -> (player.pos_x -. 1.0, player.pos_y +. 1.0)
  | DiagonalDownRight -> (player.pos_x +. 1.0, player.pos_y +. 1.0)
  | No_move -> (player.pos_x, player.pos_y)
  
let player_attack (player: pokemon) (enemy: pokemon list) =
  if player.action = Attack && player.your_turn && not(player.moving) then begin
    let target_x, target_y = player_get_target player in
    let rec aux player enemy acc msg =
      match enemy with
      | [] -> (player |> set_entity_action Nothing, acc, true, msg)
      | e :: rest ->
        if int_of_float e.pos_x = int_of_float target_x && int_of_float e.pos_y = int_of_float target_y then begin
          let degat = calcul_degats player e (List.nth player.competence 0) in
          let msg = Printf.sprintf "Entity id: %d Degat: %d on Cible: %d\n%!" player.id degat e.id in
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



