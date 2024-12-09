open OUnit2
open Core.Vector


(* Test Cases *)

let test_create _ =
  let v = create 3 1.0 in
  assert_equal 3 (Array.length v);
  assert_equal 1.0 v.(0);
  assert_equal 1.0 v.(1);
  assert_equal 1.0 v.(2)
  
let test_zeroes _ =
  let v = zeroes 3 in
  assert_equal 3 (Array.length v);
  assert_equal 0.0 v.(0);
  assert_equal 0.0 v.(1);
  assert_equal 0.0 v.(2)

let test_size _ = 
  let v = create 3 1.0 in
  assert_equal 3 (size v)


(* Test Suite *)

let test_suite = "Test Suite for Vector" >::: [
  "test_create" >:: test_create;
  "test_zeroes" >:: test_zeroes;
  "test_size" >:: test_size
]

let _ = run_test_tt_main test_suite