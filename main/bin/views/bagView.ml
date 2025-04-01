open Raylib
open Utils.Types
open Utils.Settings_map

(**
  [init_bag_textures ()] initializes the textures for the bag.
  @return The textures of the bag.
*)
let init_bag_textures () =
  let bag_image_path = "resources/images/bag/bag_tex.png" in
  let bag_image = load_image bag_image_path in
  image_resize (addr bag_image) screen_width screen_height;
  let bag_texture = Some (load_texture_from_image bag_image) in
  unload_image bag_image;

  let square_image_path = "resources/images/bag/square_select.png" in
  let square_image = load_image square_image_path in
  image_resize (addr square_image) 50 50;
  let square_texture = Some (load_texture_from_image square_image) in
  unload_image square_image;

  (bag_texture, square_texture)

(**
  [draw_bag player items_textures] draws the player's bag.
  @param player The player.
*)
let draw_bag player bag_textures items_textures =
  let (bag_texture, squarte_texture) = bag_textures in
  match bag_texture with
  | None -> ()
  | Some texture ->
    draw_texture texture 0 0 Color.white;
    draw_text "Your bag" 80 80 26 Color.white;
    let draw_square =
      match squarte_texture with
      | None -> ()
      | Some square_texture ->
        draw_texture square_texture 78 168 Color.white;
    in
    draw_square;
    let draw_items =
      List.iteri (fun i item ->
        let item_texture = List.nth items_textures item.item_skin_id in
        let item_texture_image = load_image_from_texture item_texture in
        image_resize (addr item_texture_image) 50 50;
        let item_tex = load_texture_from_image item_texture_image in
        unload_image item_texture_image;
        draw_texture item_tex (80 + (i mod 7 * 60)) (120 + (i / 7 * 60 + 50)) Color.white;
      ) player.bag.items
    in
    draw_items
