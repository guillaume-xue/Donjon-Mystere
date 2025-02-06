open Raylib

(* Variables globales pour stocker les textures *)
let title_texture = ref None
let background_texture = ref None
let title_pos_x = ref 0
let title_pos_y = ref 0
let text_pos_x = ref 0
let text_pos_y = ref 0

(* Fonction d'initialisation *)
let init_menu screen_width screen_height =
  (* Charger et redimensionner l'image du titre *)
  let title = load_image "resources/menu/title.png" in
  let image_width = Image.width title in
  let image_height = Image.height title in
  let new_width = image_width * 2 in
  let new_height = image_height * 2 in
  image_resize (addr title) new_width new_height;
  title_texture := Some (load_texture_from_image title);
  unload_image title;
  title_pos_x := (screen_width - new_width) / 2;
  title_pos_y := (screen_height - new_height) / 5;

  (* Charger une image de fond aléatoire *)
  Random.self_init ();
  let nb_bg = Random.int 7 + 1 in
  let file = "resources/menu/image-menu-" ^ string_of_int nb_bg ^ ".png" in
  let background = load_image file in
  image_resize (addr background) screen_width screen_height;
  background_texture := Some (load_texture_from_image background);
  unload_image background;

  text_pos_x := screen_width / 2 - 80;
  text_pos_y := screen_height - 150

(* Fonction pour dessiner le menu *)
let draw_menu () =
  let time = get_time () in
  let blink = int_of_float (time *. 2.0) mod 3 = 0 in

  begin_drawing ();
  clear_background Color.raywhite;
  (* Dessiner le fond *)
  (match !background_texture with
  | Some texture -> draw_texture texture 0 0 Color.white
  | None -> ());

  (* Dessiner le titre *)
  (match !title_texture with
  | Some texture -> draw_texture texture !title_pos_x !title_pos_y Color.white
  | None -> ());

  if blink then
    draw_text "Appuie pour jouer" !text_pos_x !text_pos_y 20 Color.white;
  end_drawing ()