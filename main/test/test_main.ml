open OUnit2

let suite =
  "All Tests" >::: [
    Test_entity_model.suite;
    Test_map_model.suite;
    Test_item_model.suite;
    Test_trap_ground.suite;
    Test_bag_model.suite;
    Test_a_star.suite;
    Test_shadowcaster.suite;
    Test_automate.suite;
    Test_prim.suite;
  ]

let () = run_test_tt_main suite