open Utils.Types

let player = ref { pos_x = 0; pos_y = 0; screen_x = 0; screen_y = 0; player_textures_id = 0 }

(**
  Get the player position
  @return player: player
*)
let set_player_screen (screen_x: int) (screen_y: int) =
  player := { !player with screen_x = screen_x; screen_y = screen_y } 


(**
  Set the player position
  @param x: int
  @param y: int
*)
let move_player (x: int) (y: int) =
  player := { !player with pos_x = !player.pos_x + x; pos_y = !player.pos_y + y }