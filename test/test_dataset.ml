open OUnit2
open Core

let test_create _ = 
  let features = [| [| 1.; 2.; 3. |]; [| 4.; 5.; 6. |] |] in
  let labels = [| 1.; 2. |] in
  let dataset = Dataset.create features labels in
  assert_equal dataset.features features;
  assert_equal dataset.labels labels

let test_create_invalid _ =
  let features = [| [| 1.; 2.; 3. |]; [| 4.; 5.; 6. |] |] in
  let labels = [| 1.; 2.; 3. |] in
  assert_raises (Invalid_argument "Dataset.create: features and labels must have the same number of rows") (fun () -> Dataset.create features labels)

let test_suite = "Dataset tests" >::: [
  "test_create" >:: test_create;
  "test_create_invalid" >:: test_create_invalid;
]

let _ = run_test_tt_main test_suite