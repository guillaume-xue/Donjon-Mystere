open Utils.Types

let player = ref { pos_x = 0; pos_y = 0; screen_x = 0; screen_y = 0; player_textures_id = 0 }

let set_player_screen (screen_x: int) (screen_y: int) =
  player := { !player with screen_x = screen_x; screen_y = screen_y } 