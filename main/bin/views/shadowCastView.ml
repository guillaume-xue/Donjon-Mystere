open Raylib
open Utils.Settings_map
open Utils.Funcs
open Models.EntityModel

(**
  [init_shadow_cast_view ()] initializes the shadow cast view.
  @return The shadow cast texture.
*)
let init_shadow_cast_view () :Texture2D.t list =
  let image_path = "resources/images/map/light_cast.png" in
  init_textures 0 10 (load_image image_path) []

(**
  [draw_shadow_cast shadow_cast_texture visibility player max_x max_y] draws the shadow cast.
  @param shadow_cast_texture The shadow cast texture.
  @param visibility The visibility array.
  @param player The player.
  @param max_x The maximum x coordinate of the grid.
  @param max_y The maximum y coordinate of the grid.
*)
let draw_shadow_cast (shadow_cast_texture : Texture2D.t list) (visibility : float array array) (player : Utils.Types.pokemon) (max_x : int) (max_y : int) : unit =
  let (screen_x, screen_y) = get_entity_screen player in
  let (pos_x, pos_y) = get_entity_position player in
  let rec print_grid (visibility : float array array) (start_x : float) (start_y : float) (y : int) : unit =
    if y >= max_y then ()
    else (
      let rec print_row (x : int) : unit =
        if x >= max_x then ()
        else (
          if visibility.(y).(x) > 0.0 then begin
            draw_texture (List.nth shadow_cast_texture (int_of_float(visibility.(y).(x) *. 10.0))) (screen_x + x * int_of_float(tile_texture_size) - int_of_float(start_x)) (screen_y + y * int_of_float(tile_texture_size) - int_of_float(start_y)) Color.white;
            print_row (x + 1);
          end else if x = int_of_float(pos_x) && y = int_of_float(pos_y) then begin
            draw_texture (List.nth shadow_cast_texture 0) (screen_x + x * int_of_float(tile_texture_size) - int_of_float(start_x)) (screen_y + y * int_of_float(tile_texture_size) - int_of_float(start_y)) Color.white;
            print_row (x + 1);
          end else begin
            draw_texture (List.nth shadow_cast_texture 9) (screen_x + x * int_of_float(tile_texture_size) - int_of_float(start_x)) (screen_y + y * int_of_float(tile_texture_size) - int_of_float(start_y)) Color.white;
            print_row (x + 1);
          end
        )
      in
      print_row 0;
      print_grid visibility start_x start_y (y + 1)
    )
  in
 
  print_grid visibility (pos_x *. tile_texture_size) (pos_y *. tile_texture_size) 0;