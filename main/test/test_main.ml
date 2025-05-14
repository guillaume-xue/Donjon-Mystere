open OUnit2

let suite =
  "All Tests" >::: [
    Test_entity_model.suite;
    Test_map_model.suite;
    Test_item_model.suite;
  ]

let () = run_test_tt_main suite