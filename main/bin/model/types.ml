(** 
  Type [tile] représente une tuile de la carte.

  @param x Coordonnée x de la tuile.
  @param y Coordonnée y de la tuile.
  @param texture_id Identifiant de la texture associée à la tuile.
*)
type tile = {
  x: int;
  y: int;
  texture_id: int;
}

(** 
  Type [zone] représente une un groupe de tuiles connectées.
  (la map est chargée avec des zones distinctes au premier passage)

  @param id Identifiant unique de la zone.
  @param size Taille de la zone (nombre de tuiles).
  @param tiles Liste des tuiles appartenant à la zone.
*)
type zone = {
  id: int;
  (* size: int; *)
  tiles: tile list;
}

(** 
  Type [map] représente une carte.

  @param width La largeur de la carte.
  @param height La hauteur de la carte.
  @param tiles La liste des tuiles de la carte.
  @param regions La liste des zones distinctes de la carte.
*)
type map = {
  width: int;
  height: int;
  tiles: tile list;
  regions: zone list;
}

(** 
  Type [arete] représentant une arête entre deux zones.

  @param zone1 La première zone.
  @param zone2 La deuxième zone.
  @param coord1 Les coordonnées du bord le plus proche de la première zone.
  @param coord2 Les coordonnées du bord le plus proche de la deuxième zone.
  @param distance La distance entre les deux zones.
*)
type arete = {
  zone1: zone;
  zone2: zone;
  coord1: (int * int);
  coord2: (int * int);
  distance: float;
}

(** 
  Module implémentant une file de priorité simple en utilisant une liste de paires (valeur, priorité). 
*)
module PriorityQueue = struct
  (** Le type d'une file de priorité, où 'a est le type des valeurs stockées dans la file. *)
  type 'a t = ('a * float) list ref

  (** [create] crée une nouvelle file de priorité vide. *)
  let create () = ref []

  (** [add] ajoute une valeur avec la priorité donnée à la file de priorité [pq]. 
      La file est maintenue triée par priorité, avec la plus petite priorité en premier. *)
  let add pq priority value =
    pq := List.merge (fun (_, p1) (_, p2) -> compare p1 p2) !pq [(value, priority)]

  (** [pop] enlève et retourne la valeur avec la plus petite priorité de la file de priorité [pq].
      Lève [Failure "PriorityQueue is empty"] si la file est vide. *)
  let pop pq =
    match !pq with
    | [] -> failwith "PriorityQueue is empty"
    | (value, priority) :: rest ->
      pq := rest;
      (priority, value)

  (** [is_empty] retourne [true] si la file de priorité [pq] est vide, et [false] sinon. *)
  let is_empty pq = !pq = []
end