open OUnit2
open Core.Matrix

(* Test Cases *)

let test_create _ =
  let m = create 2 2 1.0 in
  assert_equal 2 (Array.length m);
  assert_equal 2 (Array.length m.(0));
  assert_equal 1.0 m.(0).(0);
  assert_equal 1.0 m.(0).(1);
  assert_equal 1.0 m.(1).(0);
  assert_equal 1.0 m.(1).(1)

let test_create_negative _ =
  assert_raises (Invalid_argument "Matrix.create: size must be non-negative") (fun () -> create (-2) 2 1.0)

let test_zeroes _ = 
  let m = zeroes 2 2 in
  assert_equal 2 (Array.length m);
  assert_equal 2 (Array.length m.(0));
  assert_equal 0.0 m.(0).(0);
  assert_equal 0.0 m.(0).(1);
  assert_equal 0.0 m.(1).(0);
  assert_equal 0.0 m.(1).(1)

let test_identity _ =
  let m = identity 3 in
  assert_equal [|[|1.0; 0.0; 0.0|]; [|0.0; 1.0; 0.0|]; [|0.0; 0.0; 1.0|]|] m

let test_shape _ = 
  let m = create 4 5 1.0 in
  assert_equal (4, 5) (shape m)

let test_add _ =
  let m1 = create 2 2 1.0 in
  let m2 = create 2 2 2.0 in
  let m3 = create 2 2 3.0 in
  let m = add m1 m2 in
  assert_equal m3 m

let test_sub _ =
  let m1 = create 2 2 1.0 in
  let m2 = create 2 2 2.0 in
  let m3 = create 2 2 (-1.0) in
  let m = sub m1 m2 in
  assert_equal m3 m

let test_scale _ = 
  let m = scale (create 2 2 2.0) 2.0 in
  assert_equal (create 2 2 4.0) m

let test_transpose _ =
  let m = [|[|1.0; 2.0; 3.0|]; [|4.0; 5.0; 6.0|]|] in
  let m' = [|[|1.0; 4.0|]; [|2.0; 5.0|]; [|3.0; 6.0|]|] in
  assert_equal m' (transpose m)

(* let test_ *)
(* Test Suite *)

let test_suite = "Test Suite for Matrix" >::: [
  "test_create" >:: test_create;
  "test_create_negative" >:: test_create_negative;
  "test_zeroes" >:: test_zeroes;
  "test_identity" >:: test_identity;
  "test_shape" >:: test_shape;
  "test_add" >:: test_add;
  "test_sub" >:: test_sub;
  "test_scale" >:: test_scale;
  "test_transpose" >:: test_transpose
  (* 
  "test_matrixmult" >:: test_matrixmult;
  "test_vector_mult" >:: test_vector_mult 
   test_det;
  "test_decomposition"
  "test_solver";
  "test_inverse" >:: test_inverse;
  
  
  *)
]

let _ = run_test_tt_main test_suite