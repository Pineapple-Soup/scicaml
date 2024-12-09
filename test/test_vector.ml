open OUnit2
open Core.Vector


(* Test Cases *)

let test_create _ =
  let v = create 3 1.0 in
  assert_equal 3 (Array.length v);
  assert_equal 1.0 v.(0);
  assert_equal 1.0 v.(1);
  assert_equal 1.0 v.(2)

let test_create_negative _ =
  assert_raises (Invalid_argument "Vector.create: size must be non-negative") (fun () -> create (-1) 1.0)

let test_zeroes _ =
  let v = zeroes 3 in
  assert_equal 3 (Array.length v);
  assert_equal 0.0 v.(0);
  assert_equal 0.0 v.(1);
  assert_equal 0.0 v.(2)

let test_size _ = 
  let v = create 3 1.0 in
  assert_equal 3 (size v)

let test_add _ = 
  let v1 = create 3 1.0 in 
  let v2 = create 3 2.0 in
  let v = add v1 v2 in
  assert_equal 3 (Array.length v);
  assert_equal 3.0 v.(0);
  assert_equal 3.0 v.(1);
  assert_equal 3.0 v.(2)

let test_sub _ =
  let v1 = create 3 1.0 in 
  let v2 = create 3 2.0 in
  let v = sub v1 v2 in
  assert_equal 3 (Array.length v);
  assert_equal (-1.0) v.(0);
  assert_equal (-1.0) v.(1);
  assert_equal (-1.0) v.(2) 

let test_sum _ =
  let v = create 3 1.0 in
  assert_equal 3.0 (sum v)

let test_prod _ =
  let v = create 3 2.0 in
  assert_equal 8.0 (prod v)

let test_scale _ = 
  let v = create 3 2.0 in
  let new_v = scale v 2.0 in
  assert_equal 3 (Array.length new_v);
  assert_equal 4.0 new_v.(0);
  assert_equal 4.0 new_v.(1);
  assert_equal 4.0 new_v.(2)

let test_map _ =
  let v = create 3 1.0 in
  let new_v = map (fun x -> x +. 1.0) v in
  assert_equal 3 (Array.length new_v);
  assert_equal 2.0 new_v.(0);
  assert_equal 2.0 new_v.(1);
  assert_equal 2.0 new_v.(2)

let test_dot _ =
  let v1 = [| 1.0; 2.0; 3.0 |] in
  let v2 = [| 4.0; 5.0; 6.0 |] in
  assert_equal 32.0 (dot v1 v2)

let test_norm1 _ =
  let v = [| -1.0; 2.0; -3.0 |] in
  assert_equal 6.0 (norm1 v)

let test_norm2 _ =
  let v = [| 3.0; 4.0 |] in
  assert_equal 5.0 (norm2 v)

let test_norm_inf _ =
  let v = [| -1.0; 2.0; -3.0 |] in
  assert_equal 3.0 (norm_inf v)

let test_max _ =
  let v = [| -1.0; 2.0; -3.0 |] in
  assert_equal 2.0 (max v)

let test_min _ =
  let v = [| -1.0; 2.0; -3.0 |] in
  assert_equal (-3.0) (min v)

let test_argmax _ =
  let v = [| -1.0; 2.0; -3.0 |] in
  assert_equal 1 (argmax v)

let test_argmin _ =
  let v = [| -1.0; 2.0; -3.0 |] in
  assert_equal 2 (argmin v)

let test_mean _ =
  let v = [| -1.0; 2.0; -3.0 |] in
  assert_equal (-2.0 /. 3.0) (mean v)

let test_variance _ =
  let v = [| -3.0; -2.0; -1.0; 0.0; 1.0; 2.0; 3.0 |] in
  assert_equal 4.0 (variance v) ~printer:string_of_float

let test_std _ =
  let v = [| -3.0; -2.0; -1.0; 0.0; 1.0; 2.0; 3.0 |] in
  assert_equal (2.0) (std v) ~printer:string_of_float

(* Test Suite *)

let test_suite = "Test Suite for Vector" >::: [
  "test_create" >:: test_create;
  "test_create_negative" >:: test_create_negative;
  "test_zeroes" >:: test_zeroes;
  "test_size" >:: test_size;
  "test_add" >:: test_add;
  "test_sub" >:: test_sub;
  "test_sum" >:: test_sum;
  "test_prod" >:: test_prod;
  "test_scale" >:: test_scale;
  "test_map" >:: test_map;
  "test_dot" >:: test_dot;
  "test_norm1" >:: test_norm1;
  "test_norm2" >:: test_norm2;
  "test_norm_inf" >:: test_norm_inf;
  "test_max" >:: test_max;
  "test_min" >:: test_min;
  "test_argmax" >:: test_argmax;
  "test_argmin" >:: test_argmin;
  "test_mean" >:: test_mean;
  "test_variance" >:: test_variance;
  "test_std" >:: test_std
]

let _ = run_test_tt_main test_suite