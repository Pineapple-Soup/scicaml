open OUnit2
open Linear_model.Linear_regression


(* Test Cases*)
let test_init _ =
  let model = LinearRegression.init 3 in
  assert_equal 3 (Array.length model.weights);
  assert_equal 0.0 model.bias

let test_predict _ = 
  let model = LinearRegression.init 3 in
  model.weights <- [| 2.0; 2.0; 2.0 |];
  model.bias <- 1.0;
  let x = [| [| 1.0; 1.0; 1.0 |]; [|2.0; 2.0; 2.0|]; [|3.0; 3.0; 3.0|] |] in
  assert_equal [|7.0; 13.0; 19.0|] (LinearRegression.predict model x)

let test_quadratic_loss _ =
  let model = LinearRegression.init 3 in
  model.weights <- [| 2.0; 2.0; 2.0 |];
  model.bias <- 1.0;
  let x = [| [| 1.0; 1.0; 1.0 |]; [|2.0; 2.0; 2.0|]; [|3.0; 3.0; 3.0|] |] in
  let y = [| 8.0; 14.0; 20.0 |] in
  assert_equal 3.0 (LinearRegression.quadratic_loss model x y)

let test_mse _ =
  let model = LinearRegression.init 3 in
  model.weights <- [| 2.0; 2.0; 2.0 |];
  model.bias <- 1.0;
  let x = [| [| 1.0; 1.0; 1.0 |]; [|2.0; 2.0; 2.0|]; [|3.0; 3.0; 3.0|] |] in
  let y = [| 8.0; 14.0; 20.0 |] in
  assert_equal 1.0 (LinearRegression.mse model x y)

let test_gradient _ =
  let model = LinearRegression.init 3 in
  model.weights <- [| 2.0; 2.0; 2.0 |];
  model.bias <- 1.0;
  let x = [| [| 1.0; 1.0; 1.0 |]; [|2.0; 2.0; 2.0|]; [|3.0; 3.0; 3.0|] |] in
  let y = [| 8.0; 14.0; 20.0 |] in
  let (grad_weights, grad_bias) = LinearRegression.gradient model x y in
  assert_equal [| 6.0; 6.0; 6.0 |] grad_weights;
  assert_equal 3.0 grad_bias

(* Test Suite *)
let test_suite = "Test Suite for Linear Regression" >::: [
  "test_init" >:: test_init;
  "test_predict" >:: test_predict;
  "test_quadratic_loss" >:: test_quadratic_loss;
  "test_mse" >:: test_mse;
  "test_gradient" >:: test_gradient
]

let _ = run_test_tt_main test_suite