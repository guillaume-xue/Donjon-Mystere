open Utils.Types
open EntityModel

(**
  [set_trap_ground_visibility visibility trap_and_ground] sets the visibility of a trap and ground.
  @param visibility The visibility to set.
  @param trap_and_ground The trap and ground to set the visibility for.
  @return The trap and ground with the updated visibility.
*)
let set_trap_ground_visibility (visibility : bool) (trap_and_ground : trap_and_ground) : trap_and_ground =
  {
    nature = trap_and_ground.nature;
    tag_pos_x = trap_and_ground.tag_pos_x;
    tag_pos_y = trap_and_ground.tag_pos_y;
    visibility = visibility
  }

(**
  [set_trap_ground_pos_visibility x y visibility trap_and_ground] sets the visibility of a trap and ground at the given position.
  @param x The x coordinate of the trap and ground.
  @param y The y coordinate of the trap and ground.
  @param visibility The visibility to set.
  @param trap_and_ground The list of traps and grounds.
  @return The list of traps and grounds with the updated visibility.
*)
let set_trap_ground_pos_visibility (x : int) (y : int) (visibility : bool) (trap_and_ground : trap_and_ground list) : trap_and_ground list =
  let rec aux (res : trap_and_ground list) : trap_and_ground list =
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
let is_stairs (trap_and_ground : trap_and_ground list) (player : pokemon) : bool=
  let rec aux (res : trap_and_ground list) : bool =
    match res with
    | [] -> false
    | {nature; tag_pos_x; tag_pos_y; _} :: rest ->
      let (pos_x, pos_y) = get_entity_position player in
      if (nature = Stairs_Up || nature = Stairs_Down) && int_of_float pos_x = tag_pos_x && int_of_float pos_y = tag_pos_y then
        true
      else
        aux rest
  in
  aux trap_and_ground

(**
  [is_trap_ground trap_and_ground x y] checks if there is a trap or ground at the given position.
  @param trap_and_ground The list of traps and grounds.
  @param x The x coordinate of the trap and ground.
  @param y The y coordinate of the trap and ground.
  @return True if there is a trap or ground at the given position, false otherwise.
*)
let is_trap_ground (trap_and_ground : trap_and_ground list) (x : int) (y : int) : bool =
  let rec aux (res : trap_and_ground list) : bool =
    match res with
    | [] -> false
    | {tag_pos_x; tag_pos_y; _} :: rest ->
      if x = tag_pos_x && y = tag_pos_y then
        true
      else
        aux rest
  in
  aux trap_and_ground

(**
  [get_trap_ground trap_and_ground x y] gets the trap and ground at the given position.
  @param trap_and_ground The list of traps and grounds.
  @param x The x coordinate of the trap and ground.
  @param y The y coordinate of the trap and ground.
  @return The trap and ground at the given position, or None if there is none.
*)
let get_trap_ground (trap_and_ground : trap_and_ground list) (x : int) (y : int) : trap_and_ground option =
  let rec aux (res : trap_and_ground list) :trap_and_ground option =
    match res with
    | [] -> None
    | {tag_pos_x; tag_pos_y; _} as trap_and_ground :: rest ->
      if x = tag_pos_x && y = tag_pos_y then
        Some trap_and_ground
      else
        aux rest
  in
  aux trap_and_ground