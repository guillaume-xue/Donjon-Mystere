open Utils.Types

(**
  [new_enemy_pos enemy player] updates the enemy position.
  @param enemy The enemy.
  @param player The player.
  @return The updated enemy.
*)

let update_target_enemy enemy player =
  if not enemy.moving && player.moving then
    let direction = Random.int 4 in
    match direction with
    | 0 -> 
      Up
    | 1 -> 
      Down
    | 2 -> 
      Left
    | 3 -> 
      Right
    | _ -> enemy.direction
  else
    enemy.direction