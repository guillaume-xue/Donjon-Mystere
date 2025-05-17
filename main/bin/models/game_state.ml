open Utils.Types

(**
  [set_game_state_map map game_state] sets the map of the game state.
  @param map The map to set.
  @param game_state The game state to update.
  @return The game state with the updated map.
*)
let set_game_state_map (map : map) (game_state : game_state) : game_state =
  { game_state with map_state = map }

(**
  [set_game_state_player player game_state] sets the player of the game state.
  @param player The player to set.
  @param game_state The game state to update.
  @return The game state with the updated player.
*)
let set_game_state_player (player : pokemon) (game_state : game_state) : game_state =
  { game_state with player_state = player }

(**
  [set_game_state_enemy enemy game_state] sets the enemy of the game state.
  @param enemy The enemy to set.
  @param game_state The game state to update.
  @return The game state with the updated enemy.
*)
let set_game_state_enemy (enemy : pokemon list) (game_state : game_state) : game_state =
  { game_state with enemies_state = enemy }

(**
  [set_game_state_trap_and_ground trap_and_ground game_state] sets the traps and grounds of the game state.
  @param trap_and_ground The traps and grounds to set.
  @param game_state The game state to update.
  @return The game state with the updated traps and grounds.
*)
let set_game_state_trap_and_ground (trap_and_ground : trap_and_ground list) (game_state : game_state) : game_state =
  { game_state with traps_and_grounds_state = trap_and_ground }

(**
  [set_game_state_loots loots game_state] sets the loots of the game state.
  @param loots The loots to set.
  @param game_state The game state to update.
  @return The game state with the updated loots.
*)
let set_game_state_loots (loots : loot list) (game_state : game_state) : game_state =
  { game_state with loots_state = loots }

(**
  [set_game_state_msg msgs_state game_state] sets the messages of the game state.
  @param msgs_state The messages to set.
  @param game_state The game state to update.
  @return The game state with the updated messages.
*)
let set_game_state_msg (msgs_state : string list) (game_state : game_state) : game_state =
  { game_state with msgs_state = msgs_state }

(**
  [add_game_state_msg msg game_state] adds a message to the game state.
  @param msg The message to add.
  @param game_state The game state to update.
  @return The game state with the added message.
*)
let add_game_state_msg (msg : string) (game_state : game_state) : game_state =
  if msg = "" then
    game_state
  else
    { game_state with msgs_state = (game_state.msgs_state @ [msg]) }