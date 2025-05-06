open Raylib
open Utils.Funcs

(**
  [init_entity_textures ()] initializes the entity textures.
  @return The list of entity textures.
*)
let init_entity_textures () =
  let player_textures = [] in
  let images = List.mapi (fun i _ -> load_image (Printf.sprintf "resources/images/player/pokemon_%d.png" i)) (List.init 11 (fun _ -> ())) in
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

