open OUnit2
open Utils.Types
open Models.ItemModel

let test_remove_item_in_list _ =
  let item1 = { item_id = 1; item_skin_id = 101; quantity = 1; pos_x = 0.0; pos_y = 0.0; screen_x = 0; screen_y = 0; description = "Item 1"; usable = true } in
  let item2 = { item_id = 2; item_skin_id = 102; quantity = 1; pos_x = 0.0; pos_y = 0.0; screen_x = 0; screen_y = 0; description = "Item 2"; usable = true } in
  let item3 = { item_id = 3; item_skin_id = 103; quantity = 1; pos_x = 0.0; pos_y = 0.0; screen_x = 0; screen_y = 0; description = "Item 3"; usable = true } in
  let items = [item1; item2; item3] in

  (* Test removing an existing item *)
  let result = remove_item_in_list item2 items in
  assert_equal [item1; item3] result;

  (* Test removing a non-existing item *)
  let non_existing_item = { item_id = 4; item_skin_id = 104; quantity = 1; pos_x = 0.0; pos_y = 0.0; screen_x = 0; screen_y = 0; description = "Non-existing Item"; usable = true } in
  let result = remove_item_in_list non_existing_item items in
  assert_equal items result;

  (* Test removing the only item in the list *)
  let result = remove_item_in_list item1 [item1] in
  assert_equal [] result

(* Export the suite *)
let suite = "ItemModel Tests" >::: ["test_remove_item_in_list" >:: test_remove_item_in_list]
