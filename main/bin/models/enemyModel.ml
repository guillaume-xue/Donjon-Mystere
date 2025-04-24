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

let create_enemy (pos_x : float) (pos_y : float) =
  {
    pos_x = pos_x;
    pos_y = pos_y;
    screen_x = 0;
    screen_y = 0;
    entity_textures_id = 0;
    target_x = pos_x;
    target_y = pos_y;
    moving = false;
    state = Idle;
    direction = Down;
    current_hp = 10;
    max_hp = 10;
    level = 1;
    current_xp = 0;
    max_xp = 100;
    attacking = false;
    action = Nothing;
    bag = { items = []; max_size = 5 };
    step_cpt = 0;
    speed = 1.0;
  }