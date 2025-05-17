open ItemModel
open Utils.Types

(** 
  [set_usable_item_bag] met à jour l'état "utilisable" d'un objet dans le sac d'un Pokémon.

  @param index l'indice de l'objet dans le sac
  @param usable nouvel état utilisable
  @param entity le Pokémon dont le sac doit être modifié
  @return le Pokémon avec le sac mis à jour
*)
let set_usable_item_bag (index : int) (usable: bool) (entity: pokemon) : pokemon =
  let bag = entity.bag in
  let new_items = 
    let rec aux (i:int) (acc:loot list) (lst:loot list) : loot list =
      match lst with
      | [] -> List.rev acc
      | x :: xs ->
        if i = index then List.rev_append acc ((set_item_usable x usable ) :: xs)
        else aux (i + 1) (x :: acc) xs
    in
    aux 0 [] bag.items
  in
  let new_bag = {items = new_items; max_size = bag.max_size; selected_item = bag.selected_item} in
  {
    entity with
    bag = new_bag
  }
