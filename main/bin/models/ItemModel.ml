open Utils.Types

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