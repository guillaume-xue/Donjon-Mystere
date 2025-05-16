open Types

(** 
  [attaque_charge] retourne la compétence "Charge".
  @return La compétence "Charge".
*)
let attaque_charge () : competence = 
  {
    id = 0;
    name = "Charge";
    description = "Une attaque de type normal qui inflige des dégâts.";
    puissance = 20;
    precision = 100;
    attaqueType = Attaque;
    element = Normal;
  }

(** 
  [attaque_grosyeux] retourne la compétence "Gros Yeux".
  @return La compétence "Gros Yeux".
*)
let attaque_grosyeux () : competence = 
  {
    id = 1;
    name = "Gros Yeux";
    description = "Baisse la défense de l'ennemi.";
    puissance = 0;
    precision = 100;
    attaqueType = Passive;
    element = Normal;
  }

(** 
  [pistolet_a_eau] retourne la compétence "Pistolet à Eau".
  @return La compétence "Pistolet à Eau".
*)
let pistolet_a_eau () : competence = 
  {
    id = 2;
    name = "Pistolet à Eau";
    description = "Une attaque de type eau qui inflige des dégâts.";
    puissance = 40;
    precision = 100;
    attaqueType = AttaqueSpeciale;
    element = Eau;
  }

(** 
  [eclair] retourne la compétence "Éclair".
  @return La compétence "Éclair".
*)
let eclair () : competence = 
  {
    id = 3;
    name = "Éclair";
    description = "Une attaque de type électrique qui inflige des dégâts.";
    puissance = 40;
    precision = 100;
    attaqueType = AttaqueSpeciale;
    element = Electrique;
  }

(** 
  [flammeche] retourne la compétence "Flammèche".
  @return La compétence "Flammèche".
*)
let flammeche () : competence = 
  {
    id = 4;
    name = "Flammèche";
    description = "Une attaque de type feu qui inflige des dégâts.";
    puissance = 40;
    precision = 100;
    attaqueType = AttaqueSpeciale;
    element = Feu;
  }

(** 
  [choc_mental] retourne la compétence "Choc Mental".
  @return La compétence "Choc Mental".
*)
let choc_mental () : competence = 
  {
    id = 5;
    name = "Choc Mental";
    description = "Une attaque de type psy qui inflige des dégâts.";
    puissance = 40;
    precision = 100;
    attaqueType = AttaqueSpeciale;
    element = Psy;
  }

(** 
  [morsure] retourne la compétence "Morsure".
  @return La compétence "Morsure".
*)
let morsure () : competence = 
  {
    id = 6;
    name = "Morsure";
    description = "Une attaque de type ténèbres qui inflige des dégâts.";
    puissance = 40;
    precision = 100;
    attaqueType = Attaque;
    element = Tenebre;
  }

(** 
  [tranche_herbe] retourne la compétence "Tranche Herbe".
  @return La compétence "Tranche Herbe".
*)
let tranche_herbe () : competence = 
  {
    id = 7;
    name = "Tranche Herbe";
    description = "Une attaque de type plante qui inflige des dégâts.";
    puissance = 40;
    precision = 100;
    attaqueType = Attaque;
    element = Plante;
  }

(** 
  [eclat_glace] retourne la compétence "Éclat Glace".
  @return La compétence "Éclat Glace".
*)
let eclat_glace () : competence = 
  {
    id = 8;
    name = "Éclat Glace";
    description = "Une attaque de type glace qui inflige des dégâts.";
    puissance = 40;
    precision = 100;
    attaqueType = Attaque;
    element = Glace;
  }

let potion player =
  if player.current_hp + 5 > player.max_hp then
    {player with current_hp = player.max_hp}
  else
    {player with current_hp = player.current_hp + 5}

let super_potion player =
  if player.current_hp + 15 > player.max_hp then
    {player with current_hp = player.max_hp}
  else
    {player with current_hp = player.current_hp + 15}

let hyper_potion player =
  if player.current_hp + 30 > player.max_hp then
    {player with current_hp = player.max_hp}
  else
    {player with current_hp = player.current_hp + 30}

let super_bonbon player =
  { player with current_xp = player.max_xp }

let money_pokemon5 player =
  { player with money = player.money + 5 }
let money_pokemon10 player =
  { player with money = player.money + 10 }
let money_pokemon20 player =
  { player with money = player.money + 20 }
let money_pokemon50 player =
  { player with money = player.money + 50 }
let money_pokemon100 player =
  { player with money = player.money + 100 }
let money_pokemon200 player =
  { player with money = player.money + 200 }  
