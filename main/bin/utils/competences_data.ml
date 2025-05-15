open Types

let attaque_charge() : competence = 
  {
    id = 0;
    name = "Charge";
    description = "Une attaque de type normal qui inflige des dégâts.";
    puissance = 20;
    precision = 100;
    attaqueType = Attaque;
    element = Normal;
  }

let attaque_grosyeux() : competence = 
  {
    id = 1;
    name = "Gros Yeux";
    description = "Baisse la défense de l'ennemi.";
    puissance = 0;
    precision = 100;
    attaqueType = Passive;
    element = Normal;
  }

let pistolet_a_eau() : competence = 
  {
    id = 2;
    name = "Pistolet à Eau";
    description = "Une attaque de type eau qui inflige des dégâts.";
    puissance = 40;
    precision = 100;
    attaqueType = AttaqueSpeciale;
    element = Eau;
  }

let eclair() : competence = 
  {
    id = 3;
    name = "Éclair";
    description = "Une attaque de type électrique qui inflige des dégâts.";
    puissance = 40;
    precision = 100;
    attaqueType = AttaqueSpeciale;
    element = Electrique;
  }

let flammeche() : competence = 
  {
    id = 4;
    name = "Flammèche";
    description = "Une attaque de type feu qui inflige des dégâts.";
    puissance = 40;
    precision = 100;
    attaqueType = AttaqueSpeciale;
    element = Feu;
  }

let choc_mental() : competence = 
  {
    id = 5;
    name = "Choc Mental";
    description = "Une attaque de type psy qui inflige des dégâts.";
    puissance = 40;
    precision = 100;
    attaqueType = AttaqueSpeciale;
    element = Psy;
  }

let morsure() : competence = 
  {
    id = 6;
    name = "Morsure";
    description = "Une attaque de type ténèbres qui inflige des dégâts.";
    puissance = 40;
    precision = 100;
    attaqueType = Attaque;
    element = Tenebre;
  }

let tranche_herbe() : competence = 
  {
    id = 7;
    name = "Tranche Herbe";
    description = "Une attaque de type plante qui inflige des dégâts.";
    puissance = 40;
    precision = 100;
    attaqueType = Attaque;
    element = Plante;
  }

let eclat_glace() : competence = 
  {
    id = 8;
    name = "Éclat Glace";
    description = "Une attaque de type glace qui inflige des dégâts.";
    puissance = 40;
    precision = 100;
    attaqueType = Attaque;
    element = Glace;
  }