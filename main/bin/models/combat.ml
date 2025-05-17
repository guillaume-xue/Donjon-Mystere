open Utils.Types
open Utils.Competences_data
open Utils.Audio

let competences = [
  flammeche ();
  pistolet_a_eau ();
  tranche_herbe ();
  attaque_charge ();
  eclair ();
  choc_mental ();
  morsure ();
  eclat_glace ();
]

let element_types = [
  Feu;
  Eau;
  Plante;
  Normal;
  Electrique;
  Psy;
  Tenebre;
  Glace;
]

let item_effets pokemon loot =
  match loot.item_skin_id with
  | 0 -> potion pokemon
  | 1 -> super_potion pokemon
  | 2 -> hyper_potion pokemon
  | 3 -> super_bonbon pokemon
  | 4 -> money_pokemon5 pokemon
  | 5 -> money_pokemon10 pokemon
  | 6 -> money_pokemon20 pokemon
  | 7 -> money_pokemon50 pokemon
  | 8 -> money_pokemon100 pokemon
  | 9 -> money_pokemon200 pokemon
  | _ -> pokemon

let item_nom = function
  | 0 -> "Potion"
  | 1 -> "Super Potion"
  | 2 -> "Hyper Potion"
  | 3 -> "Super Bonbon"
  | 4 -> "5 Poke Dollars"
  | 5 -> "10 Poke Dollars"
  | 6 -> "20 Poke Dollars"
  | 7 -> "50 Poke Dollars"
  | 8 -> "100 Poke Dollars"
  | 9 -> "200 Poke Dollars"
  | _ -> ""

(* Fonction de génération d'un nombre aléatoire entre 0.85 et 1.0 *)

let rand_value () =
  Random.float 0.16 +. 0.85

let is_critical () =
  let random = Random.int 16 in
  if random = 0 then
    1.5
  else
    1.0

(* Conversion d'un élément en indice pour la matrice *)
let element_to_index = function
  | Feu -> 0
  | Eau -> 1
  | Plante -> 2
  | Normal -> 3
  | Electrique -> 4
  | Psy -> 5
  | Tenebre -> 6
  | Glace -> 7

(* Matrice des faiblesses [attaquant][défenseur] *)
let weakness_matrix = [|
  (* Feu      Eau      Plante   Normal   Electrique Psy     Tenebre  Glace *)
  [| 0.5;     0.5;     2.0;     1.0;     1.0;      1.0;    1.0;     2.0 |];  (* Feu *)
  [| 2.0;     0.5;     0.5;     1.0;     1.0;      1.0;    1.0;     1.0 |];  (* Eau *)
  [| 0.5;     2.0;     0.5;     1.0;     1.0;      1.0;    1.0;     1.0 |];  (* Plante *)
  [| 1.0;     1.0;     1.0;     1.0;     1.0;      1.0;    1.0;     1.0 |];  (* Normal *)
  [| 1.0;     2.0;     0.5;     1.0;     0.5;      1.0;    1.0;     1.0 |];  (* Electrique *)
  [| 1.0;     1.0;     1.0;     1.0;     1.0;      0.5;    0.0;     1.0 |];  (* Psy *)
  [| 1.0;     1.0;     1.0;     1.0;     1.0;      2.0;    0.5;     1.0 |];  (* Tenebre *)
  [| 0.5;     0.5;     2.0;     1.0;     1.0;      1.0;    1.0;     0.5 |];  (* Glace *)
|]

let is_weakness (competence: competence) (pokemon2: pokemon) =
  let i = element_to_index competence.element in
  let j = element_to_index pokemon2.element in
  weakness_matrix.(i).(j)

let is_same_type (competence: competence) (pokemon2: pokemon) =
  if competence.element = pokemon2.element then
    1.5
  else
    1.0
  

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
  play_sound "resources/audio/sound/levelUp.mp3";
  let new_hp = pokemon.max_hp + Random.int 5 in
  let new_att = pokemon.attaque + Random.int 5 in 
  let new_def = pokemon.defense + Random.int 5 in
  let new_att_sp = pokemon.attaque_speciale + Random.int 5 in
  let new_def_sp = pokemon.defense_speciale + Random.int 5 in
  (* Printf.printf "cur_hp: %d, att: %d, def: %d, att_sp: %d, def_sp: %d\n%!" new_hp new_att new_def new_att_sp new_def_sp; *)
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
  if new_xp >= pokemon1.max_xp then
    let new_pokemon = level_up pokemon1 in
    new_pokemon
  else
    {pokemon1 with current_xp = pokemon1.current_xp + xp}

