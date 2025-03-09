open Raylib
open Utils.Settings_map

(* Variables globales pour stocker les textures *)
let title_texture = ref None
let background_texture = ref None
let select_texture = ref None
let select_other_texture = ref None
let select_new_texture = ref None
let list_of_maps = ref []
let arrow_texture = ref None

(* Variables pour le titre *)
let title_pos_x = ref 0
let title_pos_y = ref 0
let text_pos_x = ref 0
let text_pos_y = ref 0

(* Variable pour la fleche *)
let arrow_pos_x = ref 70
let arrow_pos_y = ref 58

(**
  [init_menu screen_width screen_height] initialise les textures pour le menu.
  @param screen_width La largeur de l'écran.
  @param screen_height La hauteur de l'écran.
*)
let init_menu list_of_maps_param =
  (* Charger et redimensionner l'image du titre *)
  let title = load_image "resources/images/menu/title.png" in
  let image_width = Image.width title in
  let image_height = Image.height title in
  let new_width = image_width * 2 in
  let new_height = image_height * 2 in
  image_resize (addr title) new_width new_height;
  title_texture := Some (load_texture_from_image title);
  unload_image title;
  title_pos_x := (screen_width - new_width) / 2;
  title_pos_y := (screen_height - new_height) / 5;

  (* Charger la liste des cartes *)
  list_of_maps := list_of_maps_param;

  (* Charger une image de fond aléatoire *)
  Random.self_init ();
  let nb_bg = Random.int 7 + 1 in
  let file = "resources/images/menu/image-menu-" ^ string_of_int nb_bg ^ ".png" in
  let background = load_image file in
  image_resize (addr background) screen_width screen_height;
  background_texture := Some (load_texture_from_image background);
  unload_image background;

  text_pos_x := screen_width / 2 - 80;
  text_pos_y := screen_height - 150;

  (* Charger select texture *)
  let select = load_image "resources/images/menu/select.png" in
  image_resize (addr select) screen_width screen_height;
  select_texture := Some (load_texture_from_image select);
  unload_image select;

  (* Charger select texture *)
  let select_other = load_image "resources/images/menu/select_other.png" in
  image_resize (addr select_other) screen_width screen_height;
  select_other_texture := Some (load_texture_from_image select_other);
  unload_image select_other;

  (* Charger select texture *)
  let select_new = load_image "resources/images/menu/select_new.png" in
  image_resize (addr select_new) screen_width screen_height;
  select_new_texture := Some (load_texture_from_image select_new);
  unload_image select_new;

  (* Charger arrow texture *)
  let arrow = load_image "resources/images/menu/arrow.png" in
  image_resize (addr arrow) 20 20;
  arrow_texture := Some (load_texture_from_image arrow);
  unload_image arrow

(**
  [draw_intro ()] dessine le menu.
*)
let draw_intro () =
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

(**
  [draw_select ()] dessine le menu de sélection.
*)

let draw_select () =
  let time = get_time () in
  let blink = int_of_float (time *. 3.0) mod 2 = 0 in
  begin_drawing ();
  clear_background Color.raywhite;
  (* Dessiner le fond *)
  (match !background_texture with
  | Some texture -> draw_texture texture 0 0 Color.white
  | None -> ());

  (* Dessiner le fond *)
  (match !select_texture with
  | Some texture -> draw_texture texture 0 0 Color.white
  | None -> ());
  
  (* Dessiner une fleche *)
  if blink then 
    (match !arrow_texture with
    | Some texture -> draw_texture texture !arrow_pos_x !arrow_pos_y Color.white
    | None -> ());
  (* Dessiner texte *)
  draw_text "Nouvelle Partie" 100 60 20 Color.white;
  draw_text "Autre" 100 90 20 Color.white;
  draw_text "Voulez-vous creer une nouvelle partie ?" 100 450 20 Color.white;
  end_drawing ()

let draw_select_other () =
  let time = get_time () in
  let blink = int_of_float (time *. 3.0) mod 2 = 0 in
  begin_drawing ();
  clear_background Color.raywhite;
  (* Dessiner le fond *)
  (match !background_texture with
  | Some texture -> draw_texture texture 0 0 Color.white
  | None -> ());

  (* Dessiner le fond *)
  (match !select_other_texture with
  | Some texture -> draw_texture texture 0 0 Color.white
  | None -> ());
  
  (* Dessiner une fleche *)
  if blink then 
    (match !arrow_texture with
    | Some texture -> draw_texture texture !arrow_pos_x !arrow_pos_y Color.white
    | None -> ());
  (* Dessiner texte *)
  draw_text "Nouvelle Partie" 100 60 20 Color.white;
  draw_text "Autre" 100 90 20 Color.white;
  draw_text "Voulez-vous continuer la partie en cours ?" 100 450 20 Color.white;

  (* Dessiner la liste des cartes *)
  List.iteri (fun i map ->
    draw_text map 450 (60 + i * 30) 20 Color.white
  ) !list_of_maps;

  end_drawing ()

let draw_select_new map_name =
  let time = get_time () in
  let blink = int_of_float (time *. 3.0) mod 2 = 0 in
  begin_drawing ();
  clear_background Color.raywhite;
  (* Dessiner le fond *)
  (match !background_texture with
  | Some texture -> draw_texture texture 0 0 Color.white
  | None -> ());

  (* Dessiner le fond *)
  (match !select_new_texture with
  | Some texture -> draw_texture texture 0 0 Color.white
  | None -> ());

  (* Dessiner une fleche *)
  if blink then 
    (match !arrow_texture with
    | Some texture -> draw_texture texture 240 210 Color.white
    | None -> ());
  
  (* Dessiner texte *)
  draw_text map_name 270 210 20 Color.white;
  draw_text "Nomez votre nouvelle partie." 100 450 20 Color.white;
  end_drawing ()

(**
  [set_arrow_up ()] déplace la flèche vers le haut.
*)
let set_arrow_y (y : int) =
  arrow_pos_y := 58 + y * 30

let set_arrow_x (x : int) =
  arrow_pos_x := 70 + x * 350

(**
  [get_index_selected ()] récupère l'index de la carte sélectionnée.
  @return L'index de la carte sélectionnée.
*)

let get_index_selected () =
  (!arrow_pos_y - 58) / 30

(**
  [is_arrow_up ()] vérifie si la flèche est en haut.
  @return [true] si la flèche est en haut, [false] sinon.
*)
let is_arrow_up () =
  !arrow_pos_y = 58