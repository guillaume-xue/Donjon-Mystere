open Utils.Types

(**
  Set the player screen position
  @param player: player
  @param screen_x: int
  @param screen_y: int
  @return player
*)
let set_player_screen (player: player) (screen_x: int) (screen_y: int) =
  {pos_x = player.pos_x; pos_y = player.pos_y; screen_x = screen_x; screen_y = screen_y; player_textures_id = player.player_textures_id}

(**
  Set the player position
  @param player: player
  @param x: int
  @param y: int
  @return player
*)
let move_player (player: player) (x: int) (y: int) =
  {pos_x = player.pos_x + x; pos_y = player.pos_y + y; screen_x = player.screen_x; screen_y = player.screen_y; player_textures_id = player.player_textures_id}