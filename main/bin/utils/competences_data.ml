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

(**
  [potion player] restaure 5 pv au joueur.

  @param player Le joueur.

  @return Le joueur avec les points de vie augmentés.
*)
let potion (player : pokemon) : pokemon =
  if player.current_hp + 5 > player.max_hp then
    {player with current_hp = player.max_hp}
  else
    {player with current_hp = player.current_hp + 5}

(**
  [super_potion player] restaure 15 pv au joueur.

  @param player Le joueur.

  @return Le joueur avec les points de vie augmentés.
*)
let super_potion (player : pokemon) : pokemon =
  if player.current_hp + 15 > player.max_hp then
    {player with current_hp = player.max_hp}
  else
    {player with current_hp = player.current_hp + 15}

(**
  [hyper_potion player] restaure 30 pv au joueur.

  @param player Le joueur.

  @return Le joueur avec les points de vie augmentés.
*)
let hyper_potion (player : pokemon) : pokemon =
  if player.current_hp + 30 > player.max_hp then
    {player with current_hp = player.max_hp}
  else
    {player with current_hp = player.current_hp + 30}

(**
  [super_bonbon player] augmente le niveau du joueur.

  @param player Le joueur.

  @return Le joueur avec le maximum d'xp pour son niveau actuel.
*)
let super_bonbon (player : pokemon) : pokemon =
  { player with current_xp = player.max_xp }

(**
  [money_pokemon5 player] augmente la monnaie du joueur de 5.

  @param player Le joueur.

  @return Le joueur avec la monnaie augmentée.
*)
let money_pokemon5 (player : pokemon) : pokemon =
  { player with money = player.money + 5 }

(**
  [money_pokemon10 player] augmente la monnaie du joueur de 10.

  @param player Le joueur.

  @return Le joueur avec la monnaie augmentée.
*)
let money_pokemon10 (player : pokemon) : pokemon =
  { player with money = player.money + 10 }

(**
  [money_pokemon20 player] augmente la monnaie du joueur de 20.

  @param player Le joueur.

  @return Le joueur avec la monnaie augmentée.
*)
let money_pokemon20 (player : pokemon) : pokemon =
  { player with money = player.money + 20 }

(**
  [money_pokemon50 player] augmente la monnaie du joueur de 50.

  @param player Le joueur.

  @return Le joueur avec la monnaie augmentée.
*)
let money_pokemon50 (player : pokemon) : pokemon =
  { player with money = player.money + 50 }

(**
  [money_pokemon100 player] augmente la monnaie du joueur de 100.

  @param player Le joueur.

  @return Le joueur avec la monnaie augmentée.
*)
let money_pokemon100 (player : pokemon) : pokemon =
  { player with money = player.money + 100 }

(**
  [money_pokemon200 player] augmente la monnaie du joueur de 200.

  @param player Le joueur.

  @return Le joueur avec la monnaie augmentée.
*)
let money_pokemon200 (player : pokemon) : pokemon =
  { player with money = player.money + 200 }  
