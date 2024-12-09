type t = float array array

let create n m value = 
  if n < 0 || m < 0 then
    raise (Invalid_argument "Matrix.create: size must be non-negative");
  Array.make_matrix n m value

let zeroes n m = create n m 0.0

let ones n m = create n m 1.0

let identity m = 
  let id = zeroes m m in
  for i = 0 to m - 1 do
    id.(i).(i) <- 1.0
  done;
  id

let shape m = (Array.length m, Array.length m.(0))


(* determinant *)
(* inverse, transpose *)
(* matrixmult, vector mult, dot and cross product *)

(* eigenvalues, eigenvectors  (might be optional )*)