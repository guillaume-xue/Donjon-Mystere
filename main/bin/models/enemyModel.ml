open Utils.Types
open A_star
open Entity_gen
open Utils.Competences_data

let random_direction () =
  match Random.int 4 with
  | 0 -> Up
  | 1 -> Down
  | 2 -> Left
  | _ -> Right

let auto_target (x1,y1) (x2,y2) =
  let dx = x2 - x1 in
  let dy = y2 - y1 in
  (* if dx > 0 && dy > 0 then
    DiagonalDownRight
  else if dx < 0 && dy > 0 then
    DiagonalDownLeft
  else if dx > 0 && dy < 0 then
    DiagonalUpRight
  else if dx < 0 && dy < 0 then
    DiagonalUpLeft *)
  if dx > 0 then
    Right
  else if dx < 0 then
    Left
  else if dy > 0 then
    Down
  else if dy < 0 then
    Up
  else No_move

let update_target_enemy enemy player =
  if enemy.your_turn && not(enemy.moving) then
      if List.length enemy.path <= 1 && manhattan_distance (int_of_float (floor enemy.pos_x), int_of_float (floor enemy.pos_y)) (int_of_float (floor player.pos_x), int_of_float (floor player.pos_y)) <= 1  then
        auto_target (int_of_float (floor enemy.pos_x), int_of_float (floor enemy.pos_y)) (int_of_float (floor player.pos_x), int_of_float (floor player.pos_y)), true
      else
        let (x,y) = List.hd enemy.path in
        auto_target (int_of_float (floor enemy.pos_x), int_of_float (floor enemy.pos_y)) (x,y), false
  else
      enemy.direction, false




let create_enemy (pos_x : float) (pos_y : float) (player : pokemon) =
  let lvl = randLvl player in
  let (cur_hp, att, def, att_sp, def_sp) = finalGen (float_of_int lvl) () in
  let random = (Random.int 8) + 3 in
  {
    id = player.last_id;
    last_id = 0;
    number = random;
    pos_x = pos_x;
    pos_y = pos_y;
    screen_x = 0;
    screen_y = 0;
    entity_textures_id = 0;
    target_x = pos_x;
    target_y = pos_y;
    moving = false;
    state = Idle;
    direction = Down;
    current_hp = cur_hp;
    max_hp = 10;
    level = 1;
    current_xp = 0;
    max_xp = 100;
    action = Nothing;
    bag = { items = []; max_size = 5 };
    step_cpt = 0;
    speed = 1.0;
    attaque = att;
    defense = def;
    attaque_speciale = att_sp;
    defense_speciale = def_sp;
    element = Feu;
    competence = [attaque_grosyeux()];
    path = a_star [] (int_of_float pos_x, int_of_float pos_y) (int_of_float player.pos_x, int_of_float player.pos_y);
    your_turn = false;
  }, {player with last_id = player.last_id + 1}