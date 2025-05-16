open Utils.Types

let set_item_usable (item: loot) (usable: bool) =
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

let remove_item_in_list (loot: loot) (items: loot list) =
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

let use_item_print (item: loot) (entity: pokemon) =
  let msg = Printf.sprintf "%s a utilisé l'item %s" entity.nom item.description
  in 
  msg 
