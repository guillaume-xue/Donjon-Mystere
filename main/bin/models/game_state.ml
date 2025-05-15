open Utils.Types

let set_game_state_map map game_state =
  { game_state with map_state = map }

let set_game_state_player player game_state =
  { game_state with player_state = player }

let set_game_state_enemy enemy game_state =
  { game_state with enemies_state = enemy }

let set_game_state_trap_and_ground trap_and_ground game_state =
  { game_state with traps_and_grounds_state = trap_and_ground }

let set_game_state_loots loots game_state =
  { game_state with loots_state = loots }

let set_game_state_msg msgs_state game_state =
  { game_state with msgs_state = msgs_state }

let add_game_state_msg msg game_state =
  if msg = "" then
    game_state
  else
    { game_state with msgs_state = (game_state.msgs_state @ [msg]) }