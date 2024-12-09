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

let test_zeroes _ = 
  let m = zeroes 2 2 in
  assert_equal 2 (Array.length m);
  assert_equal 2 (Array.length m.(0));
  assert_equal 0.0 m.(0).(0);
  assert_equal 0.0 m.(0).(1);
  assert_equal 0.0 m.(1).(0);
  assert_equal 0.0 m.(1).(1)

let test_shape _ = 
  let m = create 4 5 1.0 in
  assert_equal (4, 5) (shape m)

(* Test Suite *)

let test_suite = "Test Suite for Matrix" >::: [
  "test_create" >:: test_create;
  "test_zeroes" >:: test_zeroes;
  "test_shape" >:: test_shape
]

let _ = run_test_tt_main test_suite