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