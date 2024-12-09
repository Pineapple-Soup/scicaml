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

let test_vstack _ = 
  let m1 = create 2 3 1.0 in
  let m2 = create 4 3 1.0 in
  let m = vstack m1 m2 in
  assert_equal (create 6 3 1.0) m

let test_hstack _ =
  let m1 = create 2 3 1.0 in
  let m2 = create 2 4 1.0 in
  let m = hstack m1 m2 in
  assert_equal (create 2 7 1.0) m

let test_append _ =
  let m1 = create 2 3 1.0 in
  let m2 = create 4 3 1.0 in
  let m = append m1 m2 0 in
  assert_equal (create 6 3 1.0) m;
  let m1 = create 2 3 1.0 in
  let m2 = create 2 4 1.0 in
  let m = append m1 m2 1 in
  assert_equal (create 2 7 1.0) m

let test_append_vector _ =
  let m = create 2 3 1.0 in
  let v = [|1.0; 1.0; 1.0|] in
  let m' = append_vector m v 0 in
  assert_equal (create 3 3 1.0) m';
  let m = create 2 3 1.0 in
  let v = [|1.0; 1.0|] in
  let m' = append_vector m v 1 in
  assert_equal (create 2 4 1.0) m'


let test_matrixmult _ =
  let m1 = [|[|1.0;2.0;3.0;4.0|]; [|5.0;6.0;8.0;9.0|]; [|13.0;2.0;1.0;4.0|]|] in
  let m2 = [|[|1.0;2.0;3.0|]; [|4.0;5.0;6.0|]; [|7.0;8.0;9.0|]; [|10.0;11.0;12.0|]|] in
  let m3 = [|[|70.0;80.0;90.0|]; [|175.0;203.0;231.0;|]; [|68.0;88.0;108.0;|]|] in
  assert_equal m3 (matrixmult m1 m2)
(* Test Suite *)
let test_vector_mult _ = 
  let m = [|[|1.0; 2.0; 3.0|]; [|4.0; 5.0; 6.0|]; [|7.0; 8.0; 9.0|]|] in
  let v = [|1.0; 2.0; 3.0|] in
  let v' = [|14.0; 32.0; 50.0|] in
  assert_equal v' (vector_mult m v)

let test_det _ =
  let m = [|[|1.0; 3.0; 5.0|]; [|2.0; 4.0; 6.0|]; [|2.0; 3.0; 8.0|]|] in
  let ans = -4.0 in 
  assert_equal ans (det m)

let test_decomposition _ =
  let m = [|[|2.0;7.0;1.0|];[|3.0;-2.0;0.0|];[|1.0;5.0;3.0|]|] in
  let (lu, _perm, _toggle) = decomposition m in
  let res = [|[|2.0;7.0;1.0|];[|0.0;-12.5;-1.5|];[|0.0;0.0;2.32|]|]in
  assert_equal res lu

let test_solver _ = 
  let m = [|[|7.0;0.7;0.43|];[|2.0;-0.4;-12.0|];[|1.0;1.3;18.0|]|] in 
  let b = [|1.0; 2.0; 3.0|] in
  let ans = [|-1.0;(4.0/.3.0);(4.0/.9.0)|] in
  assert_equal ans (solver m b)

let test_inverse _ = 
  let m = [|[|1.0;3.0;4.0|]; [|2.0;6.0;3.0|]; [|3.0;5.0;1.0|]|]in
  let res = [|[|9.0/.20.0; -17.0/.20.0;3.0/.4.0|];[|-7.0/.20.0;11.0/.20.0;-1.0/.4.0;|];[|2.0/.5.0;-1.0/.5.0;0.0|]|] in
  assert_equal res (inverse m)




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

  "test_vector_mult" >:: test_vector_mult;
  "test_det" >:: test_det;
  "test_decomposition" >:: test_decomposition;
  "test_solver" >:: test_solver;
  "test_inverse" >:: test_inverse;

]

let _ = run_test_tt_main test_suite