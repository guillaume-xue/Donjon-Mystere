open ItemModel
open Utils.Types

let set_usable_item_bag (index : int) (usable: bool) (entity: pokemon) =
  let bag = entity.bag in
  let new_items = 
    let rec aux i acc lst =
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
