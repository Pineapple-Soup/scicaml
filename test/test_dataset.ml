open OUnit2
open Core


(* Test Cases *)
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

let test_shape _ =
  let features = [| [| 1.; 2.; 3. |]; [| 4.; 5.; 6. |] |] in
  let labels = [| 1.; 2. |] in
  let dataset = Dataset.create features labels in
  assert_equal ((2, 3), 2) (Dataset.shape dataset)

let test_describe _ =
  let features = [| [| 1.; 2.; 3. |]; [| 4.; 5.; 6. |] |] in
  let labels = [| 1.; 2. |] in
  let dataset = Dataset.create features labels in
  assert_equal "Dataset with 2 rows and 3 columns" (Dataset.describe dataset)

let test_standardize _ =
  let features = [| [| 1.; 2.; 3. |]; [| 3.; 6.; 9. |] |] in
  let labels = [| 1.; 2. |] in
  let dataset = Dataset.create features labels in
  let dataset' = Dataset.standardize dataset in
  let features' = [| [| -1.; -1.; -1. |]; [| 1.; 1.; 1. |] |] in
  assert_equal features' dataset'.features;
  assert_equal dataset.labels dataset'.labels

let test_from_csv _ =
  let dataset = Dataset.from_csv "../../../test/test_data.csv" 3 in
  let ((row, col), label_shape) = Dataset.shape dataset in
  assert_equal (10, 3) (row, col);
  assert_equal 10 label_shape;
  assert_equal [| 0.; 0.; 1.; 1.; 2.; 2.; 2.; 0.; 2.; 0.|] dataset.labels;
  assert_equal [| 5.1; 3.5; 1.4 |] dataset.features.(0)
  

(* Test Suite *)
let test_suite = "Test Suite for Dataset" >::: [
  "test_create" >:: test_create;
  "test_create_invalid" >:: test_create_invalid;
  "test_shape" >:: test_shape;
  "test_describe" >:: test_describe;
  "test_standardize" >:: test_standardize;
  "test_from_csv" >:: test_from_csv
]

let _ = run_test_tt_main test_suite