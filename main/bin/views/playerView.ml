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

(**
  [draw_player_stats player] draws the player's stats on the screen.
  @param player The player whose stats to draw.
*)
let draw_player_stats (player: player) =
  let stats_text = "Level: " ^ string_of_int player.level ^ "     HP: " ^ string_of_int player.current_hp ^ "/" ^ string_of_int player.max_hp in
  let font = get_font_default () in
  let text_position = Vector2.create 10.0 10.0 in
  draw_text_ex font stats_text text_position 20.0 1.0 Color.black;
  draw_text_ex font stats_text (Vector2.create (Vector2.x text_position +. 1.0) (Vector2.y text_position +. 1.0)) 20.0 1.0 Color.orange;

  (* Draw health bar next to the text *)
  let text_width = measure_text_ex font stats_text 20.0 1.0 |> Vector2.x in
  let bar_width = 200.0 in
  let bar_height = 20.0 in
  let bar_x = Vector2.x text_position +. text_width +. 30.0 in
  let bar_y = Vector2.y text_position in
  let health_percentage = float_of_int player.current_hp /. float_of_int player.max_hp in
  let health_bar_width = bar_width *. health_percentage in
  draw_rectangle (int_of_float bar_x) (int_of_float (bar_y -. 2.0)) (int_of_float bar_width) (int_of_float (bar_height +. 4.0)) Color.white;
  draw_rectangle (int_of_float bar_x) (int_of_float bar_y) (int_of_float bar_width) (int_of_float bar_height) Color.orange;
  draw_rectangle (int_of_float bar_x) (int_of_float bar_y) (int_of_float health_bar_width) (int_of_float bar_height) Color.green
