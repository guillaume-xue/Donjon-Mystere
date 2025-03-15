open Raylib
open Utils.Types
open Utils.Settings_map

(**
  [init_player_textures ()] initializes the player textures.
  @return The list of player textures.
*)
let init_player_textures () =
  let image_path = "resources/images/player/hericendre.png" in
  if not (file_exists image_path) then begin
    Printf.printf "Image file does not exist: %s\n" image_path;
    []
  end else begin
    let player_textures = [] in
    let image = load_image image_path in
    if is_image_ready image then begin
      let rec init_movement x y player_textures =
        if x < 3 then
          begin
            let source_rec = Rectangle.create (player_texture_size *. float_of_int(x) +. float_of_int(x)) (player_texture_size *. float_of_int(y) +. float_of_int(y)) player_texture_size player_texture_size in
            let tex = load_texture_from_image (image_from_image image source_rec) in
            
            init_movement (x + 1) y (tex :: player_textures)
          end
        else if y < 7 then
          init_movement 0 (y + 1) player_textures
        else
          player_textures
      in
      let rec init_idle x y player_textures =
        if x < 2 then
          begin
            let source_rec = Rectangle.create (player_texture_size *. float_of_int(x) +. float_of_int(x) +. float_of_int(64)) (player_texture_size *. float_of_int(y) +. float_of_int(y)) player_texture_size player_texture_size in
            let tex = load_texture_from_image (image_from_image image source_rec) in
            init_idle (x + 1) y (tex :: player_textures)
          end
        else if y < 7 then
          init_idle 0 (y + 1) player_textures
        else
          player_textures
      in
      let player_textures = init_movement 0 0 player_textures in
      let player_textures = init_idle 0 0 player_textures in
      unload_image image;
      List.rev player_textures
    end else begin
      Printf.printf "Failed to load image: %s\n" image_path;
      []
    end
  end

(**
  [draw_player player player_textures] draws the player on the screen.
  @param player The player to draw.
  @param player_textures The list of player textures.
*)
let draw_player player player_textures =
  let texture = List.nth player_textures player.entity_textures_id in
  draw_texture texture (player.screen_x) (player.screen_y) Color.white

(**
  [draw_player_stats player] draws the player's stats on the screen.
  @param player The player whose stats to draw.
*)
let draw_player_stats (player: pokemon) =
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
