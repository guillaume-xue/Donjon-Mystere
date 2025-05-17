open Utils.Types
open A_star
open Entity_gen
open EntityModel
open Utils.Competences_data
open Combat

(** 
  [random_direction] retourne une direction aléatoire.

  @return Une direction aléatoire.
*)
let random_direction () : direction =
  match Random.int 4 with
  | 0 -> Up
  | 1 -> Down
  | 2 -> Left
  | _ -> Right

(** 
  [auto_target] calcule la direction à prendre pour aller de (x1, y1) à (x2, y2).

  @param (x1, y1) Position de départ.
  @param (x2, y2) Position cible.
  @return direction - La direction à prendre.
*)
let auto_target ((x1, y1): int * int) ((x2, y2): int * int) : direction =
  let dx = x2 - x1 in
  let dy = y2 - y1 in
  if dx > 0 then
    Right
  else if dx < 0 then
    Left
  else if dy > 0 then
    Down
  else if dy < 0 then
    Up
  else No_move

(** 
  [update_target_enemy] met à jour la direction et l'état d'attaque d'un ennemi en fonction de la position du joueur.

  @param enemy L'ennemi à mettre à jour.
  @param player Le joueur cible.
  @return direction * bool - La nouvelle direction et un booléen indiquant si l'ennemi est en position d'attaque.
*)
let update_target_enemy (enemy: pokemon) (player: pokemon) : direction * bool =
  if enemy.your_turn && not(enemy.moving) then
      let (e_pos_x, e_pos_y) = get_entity_position enemy in
      let (p_pos_x, p_pos_y) = get_entity_position player in
      if List.length enemy.path <= 1 && manhattan_distance ((int_of_float (floor e_pos_x), int_of_float (floor e_pos_y)), (int_of_float (floor p_pos_x), int_of_float (floor p_pos_y))) <= 1 then begin
        auto_target (int_of_float (floor e_pos_x), int_of_float (floor e_pos_y)) (int_of_float (floor p_pos_x), int_of_float (floor p_pos_y)), true
      end else
        let (x,y) = if List.length enemy.path > 0 then begin
          List.hd enemy.path
        end else
          (int_of_float (floor e_pos_x), int_of_float (floor e_pos_y)) in
        auto_target (int_of_float (floor e_pos_x), int_of_float (floor e_pos_y)) (x,y), false
  else
      enemy.direction, false

(** 
  [create_enemy] crée un ennemi à une position donnée, basé sur le joueur.
  
  @param pos_x Position x de l'ennemi.
  @param pos_y Position y de l'ennemi.
  @param player Le joueur servant de référence pour le niveau.
  @param game_state L'état du jeu.
  @return pokemon * pokemon - L'ennemi créé et le joueur avec last_id mis à jour.
*)
let create_enemy (pos_x : float) (pos_y : float) (player : pokemon) (game_state : game_state) : pokemon * pokemon =
  let lvl = randLvl player in
  let (cur_hp, att, def, att_sp, def_sp) = finalGen (float_of_int lvl) in
  let (p_pos_x, p_pos_y) = get_entity_position player in
  let random = (Random.int 8) + 3 in
  {
    nom = List.nth nom_pokemon random;
    id = player.last_id;
    last_id = 0;
    number = random;
    position = {
      world_x = pos_x;
      world_y = pos_y;
      screen_x = 0;
      screen_y = 0;
      target_x = pos_x;
      target_y = pos_y;
    };
    entity_textures_id = 0;
    moving = false;
    state = Idle;
    direction = Down;
    current_hp = cur_hp;
    max_hp = 10;
    level = 1;
    current_xp = 0;
    max_xp = 100;
    action = Nothing;
    bag = { items = []; max_size = 5; selected_item = 0};
    step_cpt = 0;
    speed = 1.0;
    attaque = att;
    defense = def;
    attaque_speciale = att_sp;
    defense_speciale = def_sp;
    element = List.nth element_types (random-3);
    competence = [attaque_charge(); List.nth competences (element_to_index (List.nth element_types (random-3)))];
    path = a_star [] (int_of_float pos_x, int_of_float pos_y) (int_of_float p_pos_x, int_of_float p_pos_y) (List.map (fun e -> let (x, y) = get_entity_position e in (int_of_float x, int_of_float y)) game_state.enemies_state);
    your_turn = false;
    money = 0;
  }, {player with last_id = player.last_id + 1}