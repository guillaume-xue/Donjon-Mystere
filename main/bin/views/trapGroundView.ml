open Raylib
open Utils.Settings_map
open Utils.Types
open Utils.Funcs

(**
  [init_trap_ground_textures ()] initializes the textures trap and ground.
  @return The textures of the trap and ground.
*)
let init_trap_ground_textures () =
  let image_stairs = load_image ("resources/images/trap_ground/stairs.jpg") in
  let image_traps = load_image ("resources/images/trap_ground/traps.jpg") in
  if is_image_ready image_stairs && is_image_ready image_traps then
    let images_textures = init_textures 0 2 image_stairs [] in
    let images_textures = init_textures 0 17 image_traps images_textures in
    unload_image image_stairs;
    unload_image image_traps;
    List.rev images_textures
  else begin
    Printf.printf "Failed to load image: %s\n" ("resources/images/map/throw_items.png");
    []
  end

(**
  [draw_trap_ground loots player loots_textures] draws the items on the map.
  @param loots The list of items.
  @param player The player.
  @param loots_textures The textures of the items.
*)
let draw_trap_ground (traps_and_grounds : trap_and_ground list) (player : pokemon) traps_grounds_textures =
  List.iter (fun trap_and_ground ->
    if trap_and_ground.visibility = false then
      ()
    else
      let texture = List.nth traps_grounds_textures (trap_ground_to_int trap_and_ground.nature) in
      draw_texture texture 
        (int_of_float (float_of_int player.screen_x +. float_of_int trap_and_ground.tag_pos_x *. tile_texture_size -. player.pos_x *. tile_texture_size)) 
        (int_of_float (float_of_int player.screen_y +. float_of_int trap_and_ground.tag_pos_y *. tile_texture_size -. player.pos_y *. tile_texture_size)) 
        Color.white
  ) traps_and_grounds
