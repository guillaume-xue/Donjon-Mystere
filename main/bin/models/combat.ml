open Utils.Types
open Utils.Competences_data
open Utils.Audio

(**
  [competences] is a list of all the competences available in the game.
*)
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

(**
  [element_types] is a list of all the element types available in the game.
*)
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

(**
  [item_effects] applies the effect of an item on a pokemon.
  @param pokemon The pokemon to apply the item effect on.
  @param loot The item to apply.
  @return The updated pokemon after applying the item effect.
*)
let item_effets (pokemon : pokemon) (loot: loot) : pokemon =
  match loot.description with
  | "Potion" -> potion pokemon
  | "Super Potion" -> super_potion pokemon
  | "Hyper Potion" -> hyper_potion pokemon
  | "Super Bonbon" -> super_bonbon pokemon
  | "5 Poke Dollars" -> money_pokemon5 pokemon
  | "10 Poke Dollars" -> money_pokemon10 pokemon
  | "20 Poke Dollars" -> money_pokemon20 pokemon
  | "50 Poke Dollars" -> money_pokemon50 pokemon
  | "100 Poke Dollars" -> money_pokemon100 pokemon
  | "200 Poke Dollars" -> money_pokemon200 pokemon
  | _ -> pokemon

(**
  [item_nom] returns the name of the item based on its id.
  @param id The id of the item.
  @return The name of the item.
*)
let item_nom (id : int) : string =
  match id with
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
let rand_value () : float =
  Random.float 0.16 +. 0.85

(* Fonction de génération de critique *)
let is_critical () : float =
  let random = Random.int 16 in
  if random = 0 then
    1.5
  else
    1.0

(* Conversion d'un élément en indice pour la matrice *)
let element_to_index (element: element) : int =
  match element with
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

(**
  [is_weakness] checks if the competence is a weakness against the pokemon2.
  @param competence The competence to check.
  @param pokemon2 The pokemon to check against.
  @return True if the competence is a weakness, false otherwise.
*)
let is_weakness (competence: competence) (pokemon2: pokemon) : float =
  let i = element_to_index competence.element in
  let j = element_to_index pokemon2.element in
  weakness_matrix.(i).(j)

(**
  [is_same_type] checks if the competence is the same type as the pokemon2.
  @param competence The competence to check.
  @param pokemon2 The pokemon to check against.
  @return True if the competence is the same type, false otherwise.
*)
let is_same_type (competence: competence) (pokemon2: pokemon) : float =
  if competence.element = pokemon2.element then
    1.5
  else
    1.0
  
(**
  [calcul_degats] calculates the damage dealt by pokemon1 to pokemon2 using competence.
  @param pokemon1 The attacking pokemon.
  @param pokemon2 The defending pokemon.
  @param competence The competence used by pokemon1.
  @return The calculated damage.
*)
let calcul_degats (pokemon1: pokemon) (pokemon2: pokemon) (competence: competence) : int =
  if competence.attaqueType = Attaque then begin
    int_of_float((((2.0 *. (float_of_int pokemon1.level) /. 5.0 +. 2.0) *. (float_of_int competence.puissance) *. ((float_of_int pokemon1.attaque) /. (float_of_int pokemon2.defense)) /. 50.0) +. 2.0) *. (rand_value ()) *. (is_critical ()) *. (is_weakness competence pokemon2) *. (is_same_type competence pokemon1))
  end
  else if competence.attaqueType = AttaqueSpeciale then
    int_of_float((((2.0 *. (float_of_int pokemon1.level) /. 5.0 +. 2.0) *. (float_of_int competence.puissance) *. ((float_of_int pokemon1.attaque_speciale) /. (float_of_int pokemon2.defense_speciale)) /. 50.0) +. 2.0) *. (rand_value ()) *. (is_critical ()) *. (is_weakness competence pokemon2) *. (is_same_type competence pokemon1))
  else
    0

(**
  [neededXp] calculates the needed XP for the next level.
  @param lvl The current level of the pokemon.
  @return The needed XP for the next level.
*)
let neededXp (lvl : int) : int =
  int_of_float (0.8 *. ((Float.pow (float_of_int (lvl+1)) 3.0) -. (Float.pow (float_of_int lvl) 3.0)))

(**
  [xp_gain] calculates the XP gained after a battle.
  @param pokemon1 The attacking pokemon.
  @param pokemon2 The defending pokemon.
  @return The XP gained.
*)
let xp_gain (pokemon1: pokemon) (pokemon2: pokemon) : int = 
  int_of_float (float_of_int pokemon2.level *. 4.5 *. (Float.max 1.0 (float_of_int pokemon2.level -. float_of_int pokemon1.level)))

(**
  [level_up] levels up the pokemon and increases its stats.
  @param pokemon The pokemon to level up.
  @return The leveled up pokemon.
*)
let level_up (pokemon: pokemon) : pokemon =
  play_sound "resources/audio/sound/levelUp.mp3";
  let new_hp = pokemon.max_hp + Random.int 5 in
  let new_att = pokemon.attaque + Random.int 5 in 
  let new_def = pokemon.defense + Random.int 5 in
  let new_att_sp = pokemon.attaque_speciale + Random.int 5 in
  let new_def_sp = pokemon.defense_speciale + Random.int 5 in
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
  
(**
  [is_level_up] checks if the pokemon can level up after a battle.
  @param pokemon1 The attacking pokemon.
  @param pokemon2 The defending pokemon.
  @return The leveled up pokemon if it can level up, otherwise the original pokemon.
*)
let is_level_up (pokemon1: pokemon) (pokemon2: pokemon) : pokemon =
  let xp = xp_gain pokemon1 pokemon2 in
  let new_xp = pokemon1.current_xp + xp in
  if new_xp >= pokemon1.max_xp then
    let new_pokemon = level_up pokemon1 in
    new_pokemon
  else
    {pokemon1 with current_xp = pokemon1.current_xp + xp}

