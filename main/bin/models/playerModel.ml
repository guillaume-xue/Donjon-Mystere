open Utils.Types

(**
  Set the player screen position
  @param player: player
  @param screen_x: int
  @param screen_y: int
  @return player
*)
let set_player_screen (player: player) (screen_x: int) (screen_y: int) =
  {pos_x = player.pos_x; pos_y = player.pos_y; screen_x = screen_x; screen_y = screen_y; player_textures_id = player.player_textures_id ; target_x = player.target_x; target_y = player.target_y; moving = player.moving; state = player.state; direction = player.direction}

(**
  Set the player target position
  @param player: player
  @param target_x: int
  @param target_y: int
  @return player
*)
let set_target (player: player) (x: float) (y: float) =
  {pos_x = player.pos_x; pos_y = player.pos_y; screen_x = player.screen_x; screen_y = player.screen_y; player_textures_id = player.player_textures_id; target_x = x; target_y = y ; moving = player.moving; state = player.state; direction = player.direction}

(**
  Set the player can moving
  @param player: player
  @param moving: bool
  @return player
*)
let set_player_moving (player: player) (moving: bool) =
  {pos_x = player.pos_x; pos_y = player.pos_y; screen_x = player.screen_x; screen_y = player.screen_y; player_textures_id = player.player_textures_id; target_x = player.target_x; target_y = player.target_y; moving = moving; state = player.state; direction = player.direction}

(**
  Set the current player position
  @param player: player
  @param pos_x: int
  @param pos_y: int
  @return player
*)
let set_player_pos (player: player) (x: float) (y: float) =
  {pos_x = x; pos_y = y; screen_x = player.screen_x; screen_y = player.screen_y; player_textures_id = player.player_textures_id; target_x = player.target_x; target_y = player.target_y; moving = player.moving; state = player.state; direction = player.direction}

(**
  Set the player direction
  @param player: player
  @param direction: direction
  @return player
*)
let is_end_movin (player: player) =
  if player.pos_x = player.target_x && player.pos_y = player.target_y then 
    set_player_moving player false
  else 
    player

(**
  Set the player direction
  @param player: player
  @param direction: direction
  @return player
*)
let set_player_texture_id (player: player) (id: int) =
  {pos_x = player.pos_x; pos_y = player.pos_y; screen_x = player.screen_x; screen_y = player.screen_y; player_textures_id = id; target_x = player.target_x; target_y = player.target_y; moving = player.moving; state = player.state; direction = player.direction}

(**
  Set the player direction
  @param player: player
  @param direction: direction
  @return player
*)
let set_player_direction (player: player) (direction: direction) =
  {pos_x = player.pos_x; pos_y = player.pos_y; screen_x = player.screen_x; screen_y = player.screen_y; player_textures_id = player.player_textures_id; target_x = player.target_x; target_y = player.target_y; moving = player.moving; state = player.state; direction = direction}

let set_player_state (player: player) (state: playerState) =
  {pos_x = player.pos_x; pos_y = player.pos_y; screen_x = player.screen_x; screen_y = player.screen_y; player_textures_id = player.player_textures_id; target_x = player.target_x; target_y = player.target_y; moving = player.moving; state = state; direction = player.direction}

(**
  Update the player texture id
  @param player: player
  @return player
*)
let update_player_texture_id (player: player) =
  let base_id = match player.state with
    | Moving -> 0
    | Idle -> 24
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
  let current_id = player.player_textures_id in
  let next_id = match player.state with
    | Moving -> if ((current_id + 1 > (base_id + direction_offset * 3)) && (current_id + 1 <= (base_id + direction_offset * 3 + 2))) then current_id + 1 else base_id + direction_offset * 3
    | Idle -> if ((current_id + 1 > (base_id + direction_offset * 2)) && (current_id + 1 <= (base_id + direction_offset * 2 + 1))) then current_id + 1 else base_id + direction_offset * 2
  in
  Printf.printf "base_id: %d, direction_offset: %d, current_id: %d, next_id: %d\n" base_id direction_offset current_id next_id;
  set_player_texture_id player next_id