open Utils.Types

(**
  Set the player screen position
  @param player: player
  @param screen_x: int
  @param screen_y: int
  @return player
*)
let set_player_screen (player: player) (screen_x: int) (screen_y: int) =
  {pos_x = player.pos_x; pos_y = player.pos_y; screen_x = screen_x; screen_y = screen_y; player_textures_id = player.player_textures_id ; target_x = player.target_x; target_y = player.target_y; moving = player.moving}

(**
  Set the player target position
  @param player: player
  @param target_x: int
  @param target_y: int
  @return player
*)
let set_target (player: player) (x: float) (y: float) =
  {pos_x = player.pos_x; pos_y = player.pos_y; screen_x = player.screen_x; screen_y = player.screen_y; player_textures_id = player.player_textures_id; target_x = x; target_y = y ; moving = player.moving}

(**
  Set the player can moving
  @param player: player
  @param moving: bool
  @return player
*)
let set_player_moving (player: player) (moving: bool) =
  {pos_x = player.pos_x; pos_y = player.pos_y; screen_x = player.screen_x; screen_y = player.screen_y; player_textures_id = player.player_textures_id; target_x = player.target_x; target_y = player.target_y; moving = moving}

(**
  Set the current player position
  @param player: player
  @param pos_x: int
  @param pos_y: int
  @return player
*)
let set_player_pos (player: player) (x: float) (y: float) =
  {pos_x = x; pos_y = y; screen_x = player.screen_x; screen_y = player.screen_y; player_textures_id = player.player_textures_id; target_x = player.target_x; target_y = player.target_y; moving = player.moving}

let is_end_movin (player: player) =
  if player.pos_x = player.target_x && player.pos_y = player.target_y then 
    set_player_moving player false
  else 
    player
