open Raylib
open Utils.Types

let player_textures = ref []

(**
  [init_player ()] initializes the textures for the player.
*)

let init_player () =
  let image = load_image "resources/player/hericendre.png" in
  let rec init_textures x y =
    if x < 4 then
      begin
        let source_rec = Rectangle.create (float_of_int(24 * x) +. float_of_int(x * 4) +. float_of_int(118)) (float_of_int(24 * y) +. float_of_int(y * 4) +. float_of_int(20)) 24.0 24.0 in
        let tex = load_texture_from_image (image_from_image image source_rec) in
        player_textures := tex :: !player_textures;
        init_textures (x + 1) y
      end
    else if y < 4 then
      init_textures 0 (y + 1)
  in
  init_textures 0 0;
  player_textures := List.rev !player_textures;
  unload_image image

(**
  [draw_player player] draws the player on the screen.
  @param player The player to draw.
*)

let draw_player (player: player) =
  let texture = List.nth !player_textures player.player_textures_id in
  draw_texture texture (player.screen_x) (player.screen_y) Color.white;
