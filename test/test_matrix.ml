open OUnit2
open Core.Matrix
open Core

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

let test_copy _ =
  let m = create 2 2 1.0 in
  let m' = copy m in
  m'.(0).(0) <- 2.0;  (* Change a value in the copy *)
  assert_equal ~cmp:(<>) m m'
  (* assert_equal m m' *)

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

(* Test Suite *)
let test_vector_mult _ = 
  let m = [|[|1.0; 2.0; 3.0|]; [|4.0; 5.0; 6.0|]; [|7.0; 8.0; 9.0|]|] in
  let v = [|1.0; 2.0; 3.0|] in
  let v' = [|14.0; 32.0; 50.0|] in
  assert_equal v' (vector_mult m v)

let test_det _ =
  let m = [|[|1.0; 3.0; 5.0|]; [|2.0; 4.0; 6.0|]; [|2.0; 3.0; 8.0|]|] in
  let ans = -8.0 in 
  assert_equal ans (det m)

let assert_float_equal_matrix m1 m2 =
  let epsilon = 1e-4 in
  let rows1 = Array.length m1 in
  let rows2 = Array.length m2 in
  assert_equal rows1 rows2;
  for i = 0 to rows1 - 1 do
    let cols1 = Array.length m1.(i) in
    let cols2 = Array.length m2.(i) in
    assert_equal cols1 cols2;
    for j = 0 to cols1 - 1 do
      assert_bool (Printf.sprintf "m1.(%d).(%d) = %f, m2.(%d).(%d) = %f" i j m1.(i).(j) i j m2.(i).(j)) (abs_float (m1.(i).(j) -. m2.(i).(j)) < epsilon)
    done
  done

let assert_float_equal_array a1 a2 =
  let epsilon = 1e-4 in
  let len1 = Array.length a1 in
  let len2 = Array.length a2 in
  assert_equal len1 len2;
  for i = 0 to len1 - 1 do
    assert_bool (Printf.sprintf "a1.(%d) = %f, a2.(%d) = %f" i a1.(i) i a2.(i)) (abs_float (a1.(i) -. a2.(i)) < epsilon)
  done
  
let test_decomposition _ =
  let m = [|[|2.0;7.0;1.0|];[|3.0;-2.0;0.0|];[|1.0;5.0;3.0|]|] in
  let (lu, _perm, _toggle) = decomposition m in
  print (lu);
  let res = [|[|3.0;-2.0;0.0|];[|2.0/.3.0;25.0/.3.0;1.0|];[|1.0/.3.0;17.0/.25.0;2.32|]|]in
  print (res);
  (* assert_equal res lu *)
  assert_float_equal_matrix res lu

let test_solver _ = 
  let m = [|[|7.0;0.7;0.43|];[|2.0;-0.4;-12.0|];[|1.0;1.3;18.0|]|] in 
  let b = [|1.0; 2.0; 3.0|] in
  let ans = [|0.4694;-10.0/.3.0;1.0/.9.0|] in
  (* Vector.print (solver m b);
  Vector.print ans; *)
  assert_float_equal_array ans (solver m b);
  assert_float_equal_array ans (solver m b)

let test_inverse _ = 
  let m = [|[|1.0;3.0;4.0|]; [|2.0;6.0;3.0|]; [|3.0;5.0;1.0|]|]in
  let res = [|[|9.0/.20.0; -17.0/.20.0;3.0/.4.0|];[|-7.0/.20.0;11.0/.20.0;-1.0/.4.0;|];[|2.0/.5.0;-1.0/.5.0;0.0|]|] in
  (* print (inverse m);
  print (res); *)
  assert_float_equal_matrix res (inverse m);
  assert_float_equal_matrix res (inverse m)

(* Test Suite *)
let test_suite = "Test Suite for Matrix" >::: [
  "test_create" >:: test_create;
  "test_create_negative" >:: test_create_negative;
  "test_zeroes" >:: test_zeroes;
  "test_identity" >:: test_identity;
  "test_shape" >:: test_shape;
  "test_copy" >:: test_copy;
  "test_add" >:: test_add;
  "test_sub" >:: test_sub;
  "test_scale" >:: test_scale;
  "test_transpose" >:: test_transpose;
  "test_vstack" >:: test_vstack;
  "test_hstack" >:: test_hstack;
  "test_append" >:: test_append;
  "test_append_vector" >:: test_append_vector;
  (* "test_dot" >:: test_dot;
  "test_transpose" >:: test_transpose;
  "test_matrixmult" >:: test_matrixmult;

  "test_vector_mult" >:: test_vector_mult;
  "test_decomposition" >:: test_decomposition;
  "test_solver" >:: test_solver;
  "test_inverse" >:: test_inverse *)
]

let _ = run_test_tt_main test_suite