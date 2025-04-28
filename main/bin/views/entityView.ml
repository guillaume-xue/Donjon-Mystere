open Raylib
open Utils.Funcs

(**
  [init_entity_textures ()] initializes the entity textures.
  @return The list of entity textures.
*)
let init_entity_textures () =
  let image_paths = Sys.readdir "resources/images/player/" |> Array.to_list |> List.filter (fun file -> Filename.check_suffix file ".png") in
  let player_textures = [] in
  let images = List.map (fun file -> load_image (Printf.sprintf "resources/images/player/%s" file)) image_paths in
  let player_textures = 
    let rec load_textures acc = function
      | [] -> List.rev acc
      | img::rest ->
        let texture = init_textures 0 40 img acc in
        load_textures (texture) rest
    in
    load_textures player_textures images
  in
  player_textures

