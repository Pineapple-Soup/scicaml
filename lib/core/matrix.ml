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

let add n m =
  if shape n <> shape m then
    raise (Invalid_argument "Matrix.add: matrices must have the same shape");
  let (nrows, _ncols) = shape n in
  let add' i =
    Array.map2 ( +. ) n.(i) m.(i)
  in
  Array.init nrows (fun i -> add' i) (* initalizes new array with each row being sum*)

let sub n m =
  if shape n <> shape m then
    raise (Invalid_argument "Matrix.sub: matrices must have the same shape");
  let (nrows, _ncols) = shape n in
  let sub' i =
    Array.map2 ( -. ) n.(i) m.(i)
  in
  Array.init nrows (fun i -> sub' i)

let scale m s = Array.map (fun x -> Array.map (fun y -> y *. s) x) m

let transpose m = 
  let (nrows, ncols) = shape m in
  let transpose' j =
    Array.init nrows (fun i -> m.(i).(j))
  in
  Array.init ncols (fun j -> transpose' j)


(* determinant *)
(* inverse, transpose *)
(* matrixmult, vector mult, dot and cross product *)

(* eigenvalues, eigenvectors  (might be optional )*)