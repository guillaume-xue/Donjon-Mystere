open Raylib
open Utils.Types
open Utils.Settings_map

(**
  [init_bag_textures ()] initializes the textures for the bag.
  @return The textures of the bag.
*)
let init_bag_textures items_textures =
  (* Load the bag texture and resize it to fit the screen *)
  let bag_image_path = "resources/images/bag/bag_tex.png" in
  let bag_image = load_image bag_image_path in
  image_resize (addr bag_image) screen_width screen_height;
  let bag_texture = Some (load_texture_from_image bag_image) in
  unload_image bag_image;
  (* Load the square texture and resize it to fit the screen *)
  let square_image_path = "resources/images/bag/square_select.png" in
  let square_image = load_image square_image_path in
  image_resize (addr square_image) 50 50;
  let square_texture = Some (load_texture_from_image square_image) in
  unload_image square_image;
  (* Load the item textures and resize them to fit the screen *)
  let items_tex = 
    let rec modif_items items_textures acc =
      match items_textures with
      | [] -> acc
      | item_texture :: rest ->
        let item_texture_image = load_image_from_texture item_texture in
        image_resize (addr item_texture_image) 40 40;
        let item_tex = load_texture_from_image item_texture_image in
        unload_image item_texture_image;
        modif_items rest (item_tex :: acc)
    in
    List.rev (modif_items items_textures []);
  in
  
  (bag_texture, square_texture, items_tex)

(**
  [draw_bag player items_textures] draws the player's bag.
  @param player The player.
*)
let draw_bag player bag_textures select =
  let (bag_texture, squarte_texture, items_tex) = bag_textures in
  match bag_texture with
  | None -> ()
  | Some texture ->
    (* Draw the bag texture *)
    draw_texture texture 0 0 Color.white;
    draw_text "Mon sac" 80 80 26 Color.white;
    (* Draw the text for the actions *)
    draw_text "[Enter] Utiliser" 580 480 20 Color.white;
    draw_text "[i]        Quitter" 580 510 20 Color.white;
    (* Draw the items in the bag *)
    match player.bag.items with
    | [] -> ()
    | _ ->
      List.iteri (fun i item ->
        let item_texture = List.nth items_tex item.item_skin_id in
        draw_texture item_texture (85 + (i mod 7 * 60)) (125 + (i / 7 * 60 + 50)) Color.white;
        
      ) player.bag.items;
      let description_lines = 
        let rec split_text text acc =
          if String.length text <= 35 then
            List.rev (text :: acc)
          else
            let space_index = 
            try String.rindex_from text 34 ' ' 
            with Not_found -> 35 
            in
            let line = String.sub text 0 space_index in
            let rest = String.sub text (space_index + 1) (String.length text - space_index - 1) in
            split_text rest (line :: acc)
        in
        split_text ((List.nth player.bag.items select).description) []
      in
      (* Draw the description of the selected item *)
      List.iteri (fun j line ->
        draw_text line 80 (480 + j * 20) 20 Color.white;
      ) description_lines;
    (* Draw the square around the selected item *)
    match squarte_texture with
    | None -> ()
    | Some square_texture ->
      draw_texture square_texture (78 + (select mod 7 * 60)) (168 + (select / 7 * 60)) Color.white;


