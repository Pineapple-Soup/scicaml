open OUnit2
open Linear_model.Logistic_regression


(* Test Cases *)
let test_init _ =
  let model = LogisticRegression.init 3 in
  assert_equal 3 (Array.length model.weights);
  assert_equal 0.0 model.bias

let test_sigmoid _ =
  assert_equal 0.5 (LogisticRegression.sigmoid 0.0);
  assert_equal 0.7310585786300049 (LogisticRegression.sigmoid 1.0) 

let test_predict _ =
  let model = LogisticRegression.init 3 in
  model.weights <- [| 2.0; 2.0; 2.0 |];
  model.bias <- 1.0;
  let x = [| [| 1.0; 1.0; 1.0 |]; [|3.0; 2.0; 5.0|]; [|-3.0; 1.0; -3.0|] |] in
  let prediction = LogisticRegression.predict model x in
  assert_equal 3 (Array.length prediction);
  assert_equal (LogisticRegression.sigmoid 7.) prediction.(0);
  assert_equal (LogisticRegression.sigmoid 21.) prediction.(1);
  assert_equal (LogisticRegression.sigmoid (-9.)) prediction.(2)
  
(* Test Suite *)
let test_suite = "Test Suite for Logistic Regression" >::: [
  "test_init" >:: test_init;
  "test_sigmoid" >:: test_sigmoid;
  "test_predict" >:: test_predict
]

let _ = run_test_tt_main test_suite