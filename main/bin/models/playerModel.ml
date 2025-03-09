open Utils.Types

(**
  Set the player screen position
  @param player: player
  @param screen_x: int
  @param screen_y: int
  @return player
*)
let set_player_screen (player: player) (screen_x: int) (screen_y: int) =
  {pos_x = player.pos_x; pos_y = player.pos_y; screen_x = screen_x; screen_y = screen_y; player_textures_id = player.player_textures_id ; target_x = player.target_x; target_y = player.target_y; moving = player.moving; state = player.state; direction = player.direction; current_hp = player.current_hp; max_hp = player.max_hp; level = player.level; current_xp = player.current_xp; max_xp = player.max_xp}

(**
  Set the player target position
  @param player: player
  @param target_x: float
  @param target_y: float
  @return player
*)
let set_target (player: player) (x: float) (y: float) =
  {pos_x = player.pos_x; pos_y = player.pos_y; screen_x = player.screen_x; screen_y = player.screen_y; player_textures_id = player.player_textures_id; target_x = x; target_y = y ; moving = player.moving; state = player.state; direction = player.direction; current_hp = player.current_hp; max_hp = player.max_hp; level = player.level; current_xp = player.current_xp; max_xp = player.max_xp}

(**
  Set the player can moving
  @param player: player
  @param moving: bool
  @return player
*)
let set_player_moving (player: player) (moving: bool) =
  {pos_x = player.pos_x; pos_y = player.pos_y; screen_x = player.screen_x; screen_y = player.screen_y; player_textures_id = player.player_textures_id; target_x = player.target_x; target_y = player.target_y; moving = moving; state = player.state; direction = player.direction; current_hp = player.current_hp; max_hp = player.max_hp; level = player.level; current_xp = player.current_xp; max_xp = player.max_xp}

(**
  Set the current player position
  @param player: player
  @param pos_x: float
  @param pos_y: float
  @return player
*)
let set_player_pos (player: player) (x: float) (y: float) =
  {pos_x = x; pos_y = y; screen_x = player.screen_x; screen_y = player.screen_y; player_textures_id = player.player_textures_id; target_x = player.target_x; target_y = player.target_y; moving = player.moving; state = player.state; direction = player.direction; current_hp = player.current_hp; max_hp = player.max_hp; level = player.level; current_xp = player.current_xp; max_xp = player.max_xp}

(**
  Set the player direction
  @param player: player
  @return player
*)
let is_end_moving (player: player) =
  if player.pos_x = player.target_x && player.pos_y = player.target_y then 
    set_player_moving player false
  else 
    player

(**
  Set the player direction
  @param player: player
  @param id : int
  @return player
*)
let set_player_texture_id (player: player) (id: int) =
  {pos_x = player.pos_x; pos_y = player.pos_y; screen_x = player.screen_x; screen_y = player.screen_y; player_textures_id = id; target_x = player.target_x; target_y = player.target_y; moving = player.moving; state = player.state; direction = player.direction; current_hp = player.current_hp; max_hp = player.max_hp; level = player.level; current_xp = player.current_xp; max_xp = player.max_xp}

(**
  Set the player direction
  @param player: player
  @param direction: direction
  @return player
*)
let set_player_direction (player: player) (direction: direction) =
  {pos_x = player.pos_x; pos_y = player.pos_y; screen_x = player.screen_x; screen_y = player.screen_y; player_textures_id = player.player_textures_id; target_x = player.target_x; target_y = player.target_y; moving = player.moving; state = player.state; direction = direction; current_hp = player.current_hp; max_hp = player.max_hp; level = player.level; current_xp = player.current_xp; max_xp = player.max_xp}

(**
  Set the player state
  @param player: player
  @param state: playerState
  @return player
*)
let set_player_state (player: player) (state: playerState) =
  {pos_x = player.pos_x; pos_y = player.pos_y; screen_x = player.screen_x; screen_y = player.screen_y; player_textures_id = player.player_textures_id; target_x = player.target_x; target_y = player.target_y; moving = player.moving; state = state; direction = player.direction; current_hp = player.current_hp; max_hp = player.max_hp; level = player.level; current_xp = player.current_xp; max_xp = player.max_xp}

(**
  Update the player texture id
  @param player: player
  @return player
*)
let update_player_texture_id (player: player) =
  let current_id = player.player_textures_id in
  if current_id > 31 then (* 31 is the last id for the player textures *)
    set_player_texture_id player player.player_textures_id
  else begin
    let base_id = match player.state with
      | Moving -> 0 (* 0 is the base id for moving textures *)
      | Idle -> 24 (* 24 is the base id for idle textures *)
    in
    let direction_offset = match player.direction with
      | Down -> 0
      | Up -> 1
      | Left -> 2
      | Right -> 3
      | DiagonalUpLeft -> 4
      | DiagonalUpRight -> 5
      | DiagonalDownLeft -> 6
      | DiagonalDownRight -> 7
    in
    let next_id = match player.state with
      | Moving -> if ((current_id + 1 > (base_id + direction_offset * 3)) && (current_id + 1 <= (base_id + direction_offset * 3 + 2))) then current_id + 1 else base_id + direction_offset * 3
      | Idle -> if ((current_id + 1 > (base_id + direction_offset * 2)) && (current_id + 1 <= (base_id + direction_offset * 2 + 1))) then current_id + 1 else base_id + direction_offset * 2
    in
    set_player_texture_id player next_id
  end