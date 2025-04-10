open Utils.Types

(**
  [is_stairs trap_and_ground player] checks if the player is on stairs.
  @param trap_and_ground The list of traps and grounds.
  @param player The player.
  @return True if the player is on stairs, false otherwise.
 *)
let is_stairs (trap_and_ground: trap_and_ground list) (player: pokemon) =
  let rec aux res =
    match res with
    | [] -> false
    | {nature; pos_x; pos_y} :: rest ->
      if (nature = Stairs_Up || nature = Stairs_Down) && int_of_float player.pos_x = pos_x && int_of_float player.pos_y = pos_y then
        true
      else
        aux rest
  in
  aux trap_and_ground