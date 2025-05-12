open Utils.Types

let rand_value () =
  Random.float 0.16 +. 0.85

let is_critical () =
  let random = Random.int 16 in
  if random = 0 then
    1.5
  else
    1.0

let is_weakness (competence: competence) (pokemon2: pokemon) = 
  match competence.element, pokemon2.element with
  | Feu, Plante -> 2.0
  | Plante, Eau -> 2.0
  | Eau, Feu -> 2.0
  | Feu, Feu -> 0.5
  | Plante, Plante -> 0.5
  | Eau, Eau -> 0.5
  | Plante, Feu -> 0.5
  | Eau, Plante -> 0.5
  | Feu, Eau -> 0.5
  | _ -> 1.0

let is_same_type (competence: competence) (pokemon2: pokemon) =
  match competence.element, pokemon2.element with
  | Feu, Feu -> 1.5
  | Plante, Plante -> 1.5
  | Eau, Eau -> 1.5
  | _ -> 1.0

let calcul_degats (pokemon1: pokemon) (pokemon2: pokemon) (competence: competence)=
  if competence.attaqueType = Attaque then begin
    int_of_float((((2.0 *. (float_of_int pokemon1.level) /. 5.0 +. 2.0) *. (float_of_int competence.puissance) *. ((float_of_int pokemon1.attaque) /. (float_of_int pokemon2.defense)) /. 50.0) +. 2.0) *. (rand_value ()) *. (is_critical ()) *. (is_weakness competence pokemon2) *. (is_same_type competence pokemon1))
  end
  else if competence.attaqueType = AttaqueSpeciale then
    int_of_float((((2.0 *. (float_of_int pokemon1.level) /. 5.0 +. 2.0) *. (float_of_int competence.puissance) *. ((float_of_int pokemon1.attaque_speciale) /. (float_of_int pokemon2.defense_speciale)) /. 50.0) +. 2.0) *. (rand_value ()) *. (is_critical ()) *. (is_weakness competence pokemon2) *. (is_same_type competence pokemon1))
  else
    0

let neededXp lvl =
  int_of_float (0.8 *. ((Float.pow (float_of_int (lvl+1)) 3.0) -. (Float.pow (float_of_int lvl) 3.0)))

let xp_gain (pokemon1: pokemon) (pokemon2: pokemon)= 
  int_of_float (float_of_int pokemon2.level *. 4.5 *. (Float.max 1.0 (float_of_int pokemon2.level -. float_of_int pokemon1.level)))

let level_up (pokemon: pokemon) =
  let new_hp = pokemon.max_hp + Random.int 5 in
  let new_att = pokemon.attaque + Random.int 5 in 
  let new_def = pokemon.defense + Random.int 5 in
  let new_att_sp = pokemon.attaque_speciale + Random.int 5 in
  let new_def_sp = pokemon.defense_speciale + Random.int 5 in
  Printf.printf "cur_hp: %d, att: %d, def: %d, att_sp: %d, def_sp: %d\n%!" new_hp new_att new_def new_att_sp new_def_sp;
  {pokemon with 
    level = pokemon.level + 1;
    max_hp = new_hp;
    current_hp = pokemon.current_hp + (new_hp-pokemon.current_hp);
    attaque = new_att;
    defense = new_def;
    attaque_speciale = new_att_sp;
    defense_speciale = new_def_sp;
    max_xp = neededXp (pokemon.level + 1);
  }
  

let is_level_up (pokemon1: pokemon) (pokemon2: pokemon) =
  let xp = xp_gain pokemon1 pokemon2 in
  let new_xp = pokemon1.current_xp + xp in
  Printf.printf "xp: %d, new_xp: %d, max_xp: %d\n%!" xp new_xp pokemon1.max_xp;
  if new_xp >= pokemon1.max_xp then
    let new_pokemon = level_up pokemon1 in
    new_pokemon
  else
    {pokemon1 with current_xp = pokemon1.current_xp + xp}

