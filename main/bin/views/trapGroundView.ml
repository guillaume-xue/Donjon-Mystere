open Raylib
open Utils.Settings_map
open Utils.Types

(**
  [init_trap_ground_textures ()] initializes the textures trap and ground.
  @return The textures of the trap and ground.
*)
let init_trap_ground_textures () =
  let image = load_image ("resources/images/trap_ground/stairs.jpg") in
  if is_image_ready image then
    let rec init_loots x loots_textures =
      if x < 2 then
        begin
          let source_rec = Rectangle.create (tile_texture_size *. float_of_int(x)) 0. tile_texture_size tile_texture_size in
          let tex = load_texture_from_image (image_from_image image source_rec) in
          init_loots (x + 1) (tex :: loots_textures)
        end
      else
        loots_textures
    in
    let loots_textures = init_loots 0 [] in
    unload_image image;
    List.rev loots_textures
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
let draw_trap_ground (trap_and_ground : trap_and_ground list) (player : pokemon) trap_ground_textures =
  List.iter (fun trap_and_ground ->
    let texture = List.nth trap_ground_textures (
      match trap_and_ground.nature with 
      | Stairs_Up -> 0
      | Stairs_Down -> 1
      | _ -> 0
      ) in
    draw_texture texture 
      (int_of_float (float_of_int player.screen_x +. float_of_int trap_and_ground.pos_x *. tile_texture_size -. player.pos_x *. tile_texture_size)) 
      (int_of_float (float_of_int player.screen_y +. float_of_int trap_and_ground.pos_y *. tile_texture_size -. player.pos_y *. tile_texture_size)) 
      Color.white
  ) trap_and_ground
