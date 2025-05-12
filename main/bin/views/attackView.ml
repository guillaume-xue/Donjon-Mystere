open Raylib
open Utils.Settings_map

let init_attack_msg_textures () =
  (* Load the bag texture and resize it to fit the screen *)
  let msg_image_path = "resources/images/attack/msg.png" in
  let msg_image = load_image msg_image_path in
  image_resize (addr msg_image) screen_width screen_height;
  Some (load_texture_from_image msg_image)

let draw_attack_msg msgs msg_textures =
  match msg_textures with
  | None -> ()
  | Some texture ->
    (* Draw the bag texture *)
    draw_texture texture 0 0 Color.white;
    match msgs with
    | [] -> ()
    | _ ->
      let rec take_last n lst =
        let len = List.length lst in
        if len <= n then lst else take_last n (List.tl lst)
      in
      let msgs_to_draw = take_last 2 msgs in
      List.iteri
        (fun i msg ->
          draw_text msg 80 (485 + i * 30) 20 Color.white)
        msgs_to_draw