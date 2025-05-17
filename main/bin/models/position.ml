open Utils.Types

(**
  [set_world new_world_x new_world_y position] sets the world coordinates of a position.
  @param new_world_x New world x coordinate.
  @param new_world_y New world y coordinate.
  @param position The position to update.
  @return A new position with updated world coordinates.
*)
let set_world (new_world_x : float) (new_world_y : float) (position : position) : position =
  { position with world_x = new_world_x; world_y = new_world_y }

(**
  [set_screen new_screen_x new_screen_y position] sets the screen coordinates of a position.
  @param new_screen_x New screen x coordinate.
  @param new_screen_y New screen y coordinate.
  @param position The position to update.
  @return A new position with updated screen coordinates.
*)
let set_screen (new_screen_x : int) (new_screen_y : int) (position : position) : position =
  { position with screen_x = new_screen_x; screen_y = new_screen_y }

(**
  [set_target new_target_x new_target_y position] sets the target coordinates of a position.
  @param new_target_x New target x coordinate.
  @param new_target_y New target y coordinate.
  @param position The position to update.
  @return A new position with updated target coordinates.
*)
let set_target (new_target_x : float) (new_target_y : float) (position : position) : position =
  { position with target_x = new_target_x; target_y = new_target_y }