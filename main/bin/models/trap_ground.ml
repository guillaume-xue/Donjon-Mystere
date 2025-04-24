open Utils.Types

let set_trap_ground_visibility (visibility: bool) (trap_and_ground: trap_and_ground) =
  {
    nature = trap_and_ground.nature;
    tag_pos_x = trap_and_ground.tag_pos_x;
    tag_pos_y = trap_and_ground.tag_pos_y;
    visibility = visibility
  }

let set_trap_ground_pos_visibility (x: int) (y: int) (visibility: bool) (trap_and_ground: trap_and_ground list) =
  let rec aux res =
    match res with
    | [] -> res
    | {nature; tag_pos_x; tag_pos_y; _} :: rest ->
      if x = tag_pos_x && y = tag_pos_y then
        {nature; tag_pos_x; tag_pos_y; visibility} :: rest
      else
        aux rest
  in
  aux trap_and_ground

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
    | {nature; tag_pos_x; tag_pos_y; _} :: rest ->
      if (nature = Stairs_Up || nature = Stairs_Down) && int_of_float player.pos_x = tag_pos_x && int_of_float player.pos_y = tag_pos_y then
        true
      else
        aux rest
  in
  aux trap_and_ground

let is_trap_ground (trap_and_ground: trap_and_ground list) x y =
  let rec aux res =
    match res with
    | [] -> false
    | {tag_pos_x; tag_pos_y; visibility; _} :: rest ->
      if x = tag_pos_x && y = tag_pos_y && visibility = false then
        true
      else
        aux rest
  in
  aux trap_and_ground

let get_trap_ground (trap_and_ground: trap_and_ground list) x y =
  let rec aux res =
    match res with
    | [] -> None
    | {tag_pos_x; tag_pos_y; _} as trap_and_ground :: rest ->
      if x = tag_pos_x && y = tag_pos_y then
        Some trap_and_ground
      else
        aux rest
  in
  aux trap_and_ground