open Utils.Types

(**
  [set_item_usable item usable] sets the usable property of an item.
  @param item The item to update.
  @param usable The new usable value.
  @return A new item with the updated usable property.
*)
let set_item_usable (item: loot) (usable: bool) : loot =
  { 
    item_id = item.item_id;
    item_skin_id = item.item_skin_id;
    quantity = item.quantity;
    pos_x = item.pos_x;
    pos_y = item.pos_y;
    screen_x = item.screen_x;
    screen_y = item.screen_y;
    description = item.description;
    usable = usable;
  }

(**
  [set_item_quantity item quantity] sets the quantity of an item.
  @param item The item to update.
  @param quantity The new quantity.
  @return A new item with the updated quantity.
*)
let remove_item_in_list (loot: loot) (items: loot list)  : loot list =
  let rec aux acc lst =
    match lst with
    | [] -> List.rev acc
    | item :: res ->
      if item.item_id = loot.item_id && item.item_skin_id = loot.item_skin_id then
        List.rev_append acc res
      else
        aux (item :: acc) res
  in
  aux [] items

(**
  [use_item_print item entity] returns a string indicating that the item has been used.
  @param item The item that has been used.
  @param entity The entity that used the item.
  @return A string indicating that the item has been used.
*)
let use_item_print (item: loot) (entity: pokemon) : string =
  let msg = Printf.sprintf "%s a utilisé l'item %s" entity.nom item.description in 
    msg 
