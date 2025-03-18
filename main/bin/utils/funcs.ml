(**
  Remplace l'élément à l'index `n` par `new_value` dans la liste `lst`
  @param lst La liste
  @param n L'index
  @param new_value La nouvelle valeur
  @return La liste avec l'élément à l'index `n` remplacé par `new_value`
*)
let rec replace_nth lst n new_value =
  match lst with
  | [] -> failwith "Index out of bounds" (* Si l'index dépasse la taille de la liste *)
  | _ :: tl when n = 0 -> new_value :: tl (* Remplace l'élément à l'index `n` *)
  | hd :: tl -> hd :: replace_nth tl (n - 1) new_value (* Continue à parcourir la liste *)
