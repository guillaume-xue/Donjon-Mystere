open Raylib

let draw_menu title background screen_height screen_width image_height image_width =
  (* Calculer la position de l'image titre*)
  let new_width = (screen_width - image_width) / 2 in
  let new_height = (screen_height - image_height) / 4 in
  (* Faire clignoter le texte "Appuie pour jouer" *)
  let time = get_time () in
  let blink = int_of_float (time *. 2.0) mod 2 = 0 in
  (* Afficher l'image de fond *)
  draw_texture background 0 0 Color.white;
  (* Afficher l'image du titre *)
  draw_texture title new_width new_height Color.white;
  (* Afficher le texte *)
  if blink then
    draw_text "Appuie pour jouer" (screen_width / 2 - 80) (screen_height - (screen_height / 4)) 20 Color.white;