open Utils.Types
open Raylib

(**
  Set the entity screen position
  @param screen_x: int
  @param screen_y: int
  @param entity: pokemon
  @return entity
*)
let set_entity_screen (screen_x: int) (screen_y: int) (entity: pokemon) =
  {pos_x = entity.pos_x; pos_y = entity.pos_y; screen_x = screen_x; screen_y = screen_y; entity_textures_id = entity.entity_textures_id ; target_x = entity.target_x; target_y = entity.target_y; moving = entity.moving; state = entity.state; direction = entity.direction; current_hp = entity.current_hp; max_hp = entity.max_hp; level = entity.level; current_xp = entity.current_xp; max_xp = entity.max_xp; attacking = entity.attacking}

(**
  Set the entity target position
  @param target_x: float
  @param target_y: float
  @param entity: pokemon
  @return entity
*)
let set_entity_target (x: float) (y: float) (entity: pokemon) =
  {pos_x = entity.pos_x; pos_y = entity.pos_y; screen_x = entity.screen_x; screen_y = entity.screen_y; entity_textures_id = entity.entity_textures_id; target_x = x; target_y = y ; moving = entity.moving; state = entity.state; direction = entity.direction; current_hp = entity.current_hp; max_hp = entity.max_hp; level = entity.level; current_xp = entity.current_xp; max_xp = entity.max_xp; attacking = entity.attacking}

(**
  Set the entity can moving
  @param moving: bool
  @param entity: pokemon
  @return entity
*)
let set_entity_moving (moving: bool) (entity: pokemon) =
  {pos_x = entity.pos_x; pos_y = entity.pos_y; screen_x = entity.screen_x; screen_y = entity.screen_y; entity_textures_id = entity.entity_textures_id; target_x = entity.target_x; target_y = entity.target_y; moving = moving; state = entity.state; direction = entity.direction; current_hp = entity.current_hp; max_hp = entity.max_hp; level = entity.level; current_xp = entity.current_xp; max_xp = entity.max_xp; attacking = entity.attacking}

(**
  Set the current entity position
  @param pos_x: float
  @param pos_y: float
  @param entity: pokemon
  @return entity
*)
let set_entity_pos (x: float) (y: float) (entity: pokemon) =
  {pos_x = x; pos_y = y; screen_x = entity.screen_x; screen_y = entity.screen_y; entity_textures_id = entity.entity_textures_id; target_x = entity.target_x; target_y = entity.target_y; moving = entity.moving; state = entity.state; direction = entity.direction; current_hp = entity.current_hp; max_hp = entity.max_hp; level = entity.level; current_xp = entity.current_xp; max_xp = entity.max_xp; attacking = entity.attacking}

(**
  Set the entity direction
  @param id : int
  @param entity: pokemon
  @return entity
*)
let set_entity_texture_id (id: int) (entity: pokemon) =
  {pos_x = entity.pos_x; pos_y = entity.pos_y; screen_x = entity.screen_x; screen_y = entity.screen_y; entity_textures_id = id; target_x = entity.target_x; target_y = entity.target_y; moving = entity.moving; state = entity.state; direction = entity.direction; current_hp = entity.current_hp; max_hp = entity.max_hp; level = entity.level; current_xp = entity.current_xp; max_xp = entity.max_xp; attacking = entity.attacking}

(**
  Set the entity direction
  @param direction: direction
  @param entity: pokemon
  @return entity
*)
let set_entity_direction (direction: direction) (entity: pokemon) =
  {pos_x = entity.pos_x; pos_y = entity.pos_y; screen_x = entity.screen_x; screen_y = entity.screen_y; entity_textures_id = entity.entity_textures_id; target_x = entity.target_x; target_y = entity.target_y; moving = entity.moving; state = entity.state; direction = direction; current_hp = entity.current_hp; max_hp = entity.max_hp; level = entity.level; current_xp = entity.current_xp; max_xp = entity.max_xp; attacking = entity.attacking}

(**
  Set the entity state
  @param state: entityState
  @param entity: pokemon
  @return entity
*)
let set_entity_state (state: entityState) (entity: pokemon) =
  {pos_x = entity.pos_x; pos_y = entity.pos_y; screen_x = entity.screen_x; screen_y = entity.screen_y; entity_textures_id = entity.entity_textures_id; target_x = entity.target_x; target_y = entity.target_y; moving = entity.moving; state = state; direction = entity.direction; current_hp = entity.current_hp; max_hp = entity.max_hp; level = entity.level; current_xp = entity.current_xp; max_xp = entity.max_xp; attacking = entity.attacking}

let set_entity_attacking (attacking: bool) (entity: pokemon) =
  {pos_x = entity.pos_x; pos_y = entity.pos_y; screen_x = entity.screen_x; screen_y = entity.screen_y; entity_textures_id = entity.entity_textures_id; target_x = entity.target_x; target_y = entity.target_y; moving = entity.moving; state = entity.state; direction = entity.direction; current_hp = entity.current_hp; max_hp = entity.max_hp; level = entity.level; current_xp = entity.current_xp; max_xp = entity.max_xp; attacking = attacking}


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
let move direction (entity: pokemon) key_pressed =
  if key_pressed then
    match direction with
    | Up -> 
      entity
        |> set_entity_target entity.pos_x (entity.pos_y -. 1.0)
        |> set_entity_direction Up
        |> set_entity_moving true
    | Down -> 
      entity
        |> set_entity_target entity.pos_x (entity.pos_y +. 1.0)
        |> set_entity_direction Down
        |> set_entity_moving true
    | Left ->
      entity
        |> set_entity_target (entity.pos_x -. 1.0) entity.pos_y
        |> set_entity_direction Left
        |> set_entity_moving true
    | Right ->
      entity
        |> set_entity_target (entity.pos_x +. 1.0) entity.pos_y
        |> set_entity_direction Right
        |> set_entity_moving true
    | _ -> entity
  else
    entity

let action_player action (entity: pokemon) key_pressed =
  if key_pressed then
    match action with
    | Attack -> 
      entity
        |> set_entity_attacking true
    | _ -> entity
  else
    entity



(**
  [is_end_moving entity] checks if the pokemon has reached its target position.
  @param entity The pokemon.
  @return The updated pokemon.
*)
let is_end_moving (entity: pokemon) =
  if entity.pos_x = entity.target_x && entity.pos_y = entity.target_y then 
    set_entity_moving false entity |> set_entity_attacking false
  else 
    entity
    
(**
  [new_entity_pos map entity last_update_time] updates the entity position.
  @param map The map.
  @param entity The entity.
  @param enemy The enemy.
  @param last_update_time The last time the entity position was updated.
  @return The updated entity and the last time the entity position was updated.
*)
let new_entity_pos map entity (enemy: pokemon list) last_update_time =
  let current_time = get_time () in
  (* Check if the entity is facing a wall *)
  let new_entity = 
    if is_obstacle map entity enemy then
      entity
      |> set_entity_moving false
      |> set_entity_target (floor entity.pos_x) (floor entity.pos_y)
    else
      entity
      |> set_entity_target (floor entity.target_x) (floor entity.target_y)
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
  (* Update the entity position *)
  if current_time -. last_update_time >= 0.01 then
    let dx = new_entity.target_x -. new_entity.pos_x in
    let dy = new_entity.target_y -. new_entity.pos_y in
    let step = 0.05 in
    let new_x = if abs_float dx < step then new_entity.target_x else new_entity.pos_x +. (if dx > 0.0 then step else -.step) in
    let new_y = if abs_float dy < step then new_entity.target_y else new_entity.pos_y +. (if dy > 0.0 then step else -.step) in
    let new_entity = 
        new_entity
        |> set_entity_pos new_x new_y
    in
    (new_entity, current_time)
  else
    (new_entity, last_update_time)

(**
  Update the entity texture id
  @param entity: pokemon
  @return entity
*)
let update_entity_texture_id (entity: pokemon) =
  let current_id = entity.entity_textures_id in
  if current_id > 31 then (* 31 is the last id for the entity textures *)
    set_entity_texture_id entity.entity_textures_id entity
  else begin
    let base_id = match entity.state with
      | Moving -> 0 (* 0 is the base id for moving textures *)
      | Idle -> 24 (* 24 is the base id for idle textures *)
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

let player_attack (player: pokemon) (enemy: pokemon list) =
  if player.attacking then begin
    match List.find_opt (fun (e: pokemon) -> e.pos_x = player.target_x && e.pos_y = player.target_y) enemy with
      | Some p -> begin
        let tmp = p in
        let (new_en: pokemon) = {tmp with current_hp = tmp.current_hp - 5} in
        new_en :: (List.filter (fun (e: pokemon) -> e.pos_x <> player.target_x || e.pos_y <> player.target_y) enemy)
      end
      | None -> enemy
    end
  else
    enemy


