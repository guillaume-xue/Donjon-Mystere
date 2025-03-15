open Raylib
open Utils.Types
open Utils.Settings_map

(**
  [init_enemy_textures ()] initializes the textures for the enemy.
  @return The textures of the enemy.
*)
let init_enemy_textures () =
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
  [draw_enemy enemy enemy_textures player] draws the enemy.
  @param enemy The enemy.
  @param enemy_textures The textures of the enemy.
  @param player The player.
*)
let draw_enemy enemy enemy_textures player =
  let texture = List.nth enemy_textures enemy.entity_textures_id in
  draw_texture texture (int_of_float(float_of_int(player.screen_x) +. enemy.pos_x *. tile_texture_size -. player.pos_x *. tile_texture_size)) (int_of_float(float_of_int(player.screen_y) +. enemy.pos_y *. tile_texture_size -. player.pos_y *. tile_texture_size)) Color.white;
  