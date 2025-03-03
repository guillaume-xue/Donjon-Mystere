open Raylib
open Utils.Types

let player_textures = ref []

(**
  [init_player ()] initializes the textures for the player.
*)
let init_player () =
  let image_path = "resources/player/hericendre.png" in
  if not (file_exists image_path) then
    Printf.printf "Image file does not exist: %s\n" image_path
  else
    let image = load_image image_path in
    if is_image_ready image then
      let rec init_movement x y =
        if x < 3 then
          begin
            let source_rec = Rectangle.create (float_of_int(21 * x) +. float_of_int(x)) (float_of_int(21 * y) +. float_of_int(y)) 21.0 21.0 in
            let tex = load_texture_from_image (image_from_image image source_rec) in
            player_textures := tex :: !player_textures;
            init_movement (x + 1) y
          end
        else if y < 7 then
          init_movement 0 (y + 1)
      in
      let rec init_idle x y =
        if x < 2 then
          begin
            let source_rec = Rectangle.create (float_of_int(21 * x) +. float_of_int(x) +. float_of_int(64)) (float_of_int(21 * y) +. float_of_int(y)) 21.0 21.0 in
            let tex = load_texture_from_image (image_from_image image source_rec) in
            player_textures := tex :: !player_textures;
            init_idle (x + 1) y
          end
        else if y < 7 then
          init_idle 0 (y + 1)
      in
      init_movement 0 0;
      init_idle 0 0;
      player_textures := List.rev !player_textures;
      unload_image image
    else
      Printf.printf "Failed to load image: %s\n" image_path

(**
  [draw_player player] draws the player on the screen.
  @param player The player to draw.
*)
let draw_player (player: player) =
  let texture = List.nth !player_textures player.player_textures_id in
  draw_texture texture (player.screen_x) (player.screen_y) Color.white
