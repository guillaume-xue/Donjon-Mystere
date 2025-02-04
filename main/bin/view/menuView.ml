open Raylib

let draw_menu title background screen_height screen_width image_height image_width =
  let new_width = (screen_width - image_width) / 2 in
  let new_height = (screen_height - image_height) / 4 in
  (* Afficher l'image de fond *)
  draw_texture background 0 0 Color.white;
  (* Afficher l'image redimensionnée *)
  draw_texture title new_width new_height Color.white;
  (* Afficher les boutons *)
  draw_text "Appuie pour jouer" (screen_width / 2 - 80) (screen_height - (screen_height / 4)) 20 Color.black;