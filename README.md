# Projet de Programmation - Jeu vidéo sous Ocaml

## Concept

Un jeu de type roguelike inspiré de la saga "donjon mystère" et "Deep Rock Galactic" où le joueur explore un donjon généré aléatoirement, combat des monstres, trouve des trésors, et doit survivre le plus longtemps possible en progressant dans des niveaux de difficulté croissante.

## Fonctionnalités Clés et Points Techniques

- Génération procédurale du donjon :

On utilise l’algorithme des **arbres de partition binaire** pour découper l’espace et générer une carte cohérente dans lequel on attribue chaque zone à une salle ou couloirs etc.

Puis on utilise des **automates cellulaires** pour générer des cavernes ou des zones "naturelles" dans le donjon dans lesquelles on vient ajouter des obstacles, des trésors, et des ennemis.\
Pour chaque type de case, il est nécessaire d'implémenter les conditions, les probabilités ou les événements qui pourront déclancher son apparition.

Utilisation des algorithmes de bruit (Worley) pour generé les differents zones de la map.

Enfin on peut utilser l'agorithme d'**Aldous-Broder ou de Prim** pour rendre l'agencement plus aléatoire et linéaire.

Précision : l'idée serait de fragmenter la carte en plusieurs sous zones et dans chacune de ces sous zones on lance un automate sur une case avec un type aléatoire/définit puis à la fin on effectue un parcours avec prim pour relier ces zones entres eux si un chemin n'existe aps déjà.

<!--
-Structure des données du donjon
Le donjon peut être représenté comme une matrice 2D de cases, chaque case représentant un type de terrain (sol, mur, porte, etc.).
type cell = Wall | Floor | Door | Monster of int | Treasure of int
 -->

- Système de FOV ou "brouillard de guerre"

L'algorithme **Shadowcasting** permettra d’implémenter un système de visibilité efficace en 2D qui calculera les zones à cacher par rapport à l'emplacement du joueur.
La bibliothèque Graphics de Ocaml peut être utilisée pour ombrer les cases non visibles.

- IA des monstres

Les déplacements d'une entité vers le joueur sera décidé par l'algorithme **A\* (A star)**, on utilisera un graphe ou une matrice pour modéliser les chemins possibles.

Le comportement de chaque entité sera définit par une liste d'action possible :\
Poursuite directe : Un monstre suit le joueur dès qu'il est visible.\
Attaque à distance : Un ennemi tire à distance s’il voit le joueur dans son champ de vision.\
Fuite : Certains ennemis peuvent fuir si leurs points de vie sont bas.\
(etc, d'autres actions possible à personnaliser)

<!-- -Organisation du code IA
Modéliser les comportements sous forme de fonctions OCaml indépendantes pour chaque type d’ennemi. Cela rend le système de comportement plus facilement extensible. -->

- Système de Combat au Tour par Tour

Le jeu doit gérer une séquence d’actions où chaque personnage (joueur et ennemis) effectue son action à tour de rôle. Implémenter une file d'attente pour ordonner les actions de chaque personnage serait une idée préférable.

Garder un compteur de points d’action (PA) pour limiter le nombre de déplacements ou d’attaques qu’un personnage peut effectuer par tour.

Le calcul des combats utilisera un système de probabilité pour déterminer les chances de réussite d’une attaque, en fonction des statistiques d’attaque et de défense de chaque personnage mais aussi pour les dégats (les effets de status ?, à personnaliser).

- Gestion de l’Inventaire et des Objets

Une liste pour l'inventaire de chaque joueur et chaque objet représenté par un type de donnée en Ocaml.
Chaque joueur pourra ramasser des objets, les utiliser, les donner ou les abondonner.
(Objets à effet spécifique, à personnaliser)

Affichage des informations dans une zone de texte (par exemple, les statistiques du joueur, les actions récentes).
Messages contextuels pour informer le joueur de chaque action (ex : "Vous avez trouvé une épée !").

- Méthodes de développement :

Conception fonctionnelle : Un jeu a la gestion de l’état dynamique . La gestion des entités (joueur, ennemis, objets), utilisation des types algébriques et des fonctions pures.

La bibliothèque **Graphics** nous servira pour un affichage en 2D de base avec des symboles ou des couleurs représentant les ennemis, les objets, le joueur etc.

Architecture du Code : L’utilisation de la structure MVC aidera à séparer la logique du jeu, la gestion des données, et l’interface utilisateur.

- Aspects supplémentaires

Équilibrage du jeu :\
Implémenter un système d’évolution de la difficulté où chaque niveau devient plus difficile. Utilise des probabilités pour déterminer les monstres et trésors générés.

Gestion des sauvegardes :\
Implémenter un système de sauvegarde en sérialisant les données de jeu, comme la position du joueur, son inventaire, et les niveaux explorés.

Multijoueur :\
Implémenter la partie réseau dans le cas de plusieur joueur.

Génération de quêtes et d’événements aléatoires :\
Ajout des quêtes secondaires ou des événements aléatoires (pièges, apparition de nouveaux ennemis) pour enrichir l’expérience de jeu.

## Testabilité du Projet

Testes unitaire pour verifier que chaque fonctionnalité fonctionne indépendamment.

Configurer CI/CD (à voir comment ça fonctionne et comment l'adapter à du ocaml)

## Calendrier

- Génération procédurale du donjon : 3 semaines
- Système de FOV ou "brouillard de guerre" : 2 semaines
- IA des monstres : 2 semaine
- Système de Combat au Tour par Tour : 1 semaine
- Gestion de l’Inventaire et des Objets : 2 semaine
- Affichage et Interface Utilisateur : 2 semaine
- Aspects supplémentaires (À voir)
