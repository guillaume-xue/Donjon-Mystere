# Projet de Programmation - Jeu vidéo sous Ocaml

## Concept

Un jeu de type roguelike inspiré de la saga "donjon mystère" où le joueur explore un donjon généré aléatoirement, combat des monstres, trouve des trésors, et doit survivre le plus longtemps possible en progressant dans des niveaux de difficulté croissante.

## Fonctionnalités Clés et Points Techniques

- Génération procédurale du donjon :

On commence par utiliser des **automates cellulaires** pour générer des cavernes ou des zones "naturelles" dans le donjon, on trie pour garder uniquement les zones d'une taille suffisamment intéressante, la suppression se fera avec l'**algorithme de remplissage par diffusion**.

Puis on crée les chemins avec une garantie d'un accès à chaque zone avec **Prim** et on crée les biomes avec **Voronoï**.

Enfin, on ajoute de façon aléatoire et répartie entre les zones, les ennemis, items, pièges...

- Système de FOV ou "brouillard de guerre"

L'algorithme **Shadowcasting** permettra d’implémenter un système de visibilité efficace en 2D qui calculera les zones à cacher par rapport à l'emplacement du joueur.
La bibliothèque Graphics d'Ocaml peut être utilisée pour ombrer les cases non visibles.

- IA des monstres

Les déplacements d'une entité vers le joueur seront décidés par l'algorithme **A\* (A star)**, on utilisera un graphe ou une matrice pour modéliser les chemins possibles.

À chaque création d'une map, chaque entité fera un premier calcul, puis le recalculera seulement lorsqu'elle sera assez proche du joueur, pour éviter une surcharge de calcul inutile.

- Système de Combat au Tour par Tour

Le jeu doit gérer une séquence d’actions où chaque personnage (joueur et ennemis) effectue son action à tour de rôle.

Le calcul des combats utilisera un système de probabilité pour déterminer les chances de réussite d’une attaque, en fonction des statistiques d’attaque et de défense de chaque personnage mais aussi pour les dégâts, on essaie de se rapprocher au plus du calcul originel de Pokémon.

- Gestion de l’Inventaire et des Objets

Une liste pour l'inventaire de chaque joueur et chaque objet représenté par un type de données en Ocaml.
Chaque joueur pourra ramasser des objets, les utiliser.

Affichage des informations dans une zone de texte.
Messages contextuels pour informer le joueur de chaque action (ex : "Vous avez trouvé une épée !").

- Méthodes de développement :

Conception fonctionnelle : Un jeu à la gestion de l’état dynamique. La gestion des entités (joueur, ennemis, objets), utilisation des types algébriques et des fonctions pures.

La bibliothèque **Graphics** nous servira pour un affichage en 2D de base avec des symboles ou des couleurs représentant les ennemis, les objets, le joueur, etc.

Architecture du Code : L’utilisation de la structure MVC aidera à séparer la logique du jeu, la gestion des données et l’interface utilisateur.

- Aspects supplémentaires

Équilibrage du jeu :\
Implémenter un système d’évolution de la difficulté où chaque niveau devient plus difficile. Utiliser des probabilités pour déterminer les monstres et trésors générés.

Gestion des sauvegardes :\
Implémenter un système de sauvegarde en sérialisant les données de jeu, comme la position du joueur, son inventaire, et les niveaux explorés.

Génération de quêtes et d’événements aléatoires :\
Ajout de quêtes secondaires ou d’événements aléatoires (pièges, apparition de nouveaux ennemis) pour enrichir l’expérience de jeu.

## Testabilité du Projet

Tests unitaires pour vérifier que chaque fonctionnalité fonctionne indépendamment.

Configurer CI/CD (à voir comment ça fonctionne et comment l'adapter à Ocaml)

## Calendrier

- Génération procédurale du donjon : 3 semaines
- Système de FOV ou "brouillard de guerre" : 2 semaines
- IA des monstres : 2 semaines
- Système de Combat au Tour par Tour : 1 semaine
- Gestion de l’Inventaire et des Objets : 2 semaines
- Affichage et Interface Utilisateur : 2 semaines
- Aspects supplémentaires (À voir)

## Pré-requis

Pour MacOS, assurez-vous que XQuartz est installé et en cours d'exécution.

```bash
https://www.xquartz.org
```

Utilisation de Homebrew :

```bash
brew install xquartz
brew install sdl2 sdl2_mixer
opam install tsdl tsdl-mixer
```

Pour les utilisateurs Linux :

Installation de Ocaml :
```bash
sudo apt-get update
sudo apt-get upgrade
sudo apt-get install git make opam
opam init -y
opam update         
opam upgrade
opam install ocaml-lsp-server odoc ocamlformat utop lwt dune
opam switch create 5.2.1 5.2.1
opam switch 5.2.1
eval $(opam env --switch=5.2.1)
```

Installation des composants :
```bash
opam install raylib yojson tsdl tsdl-mixer ounit2
```

## Compilation

Pour exécuter le projet :

```bash
  make run
```

Pour nettoyer les fichiers build :

```bash
  make clean
```
