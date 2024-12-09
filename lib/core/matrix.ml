type t = float array array

let create n m value = 
  if n < 0 || m < 0 then
    raise (Invalid_argument "Matrix.create: size must be non-negative");
  Array.make_matrix n m value

let zeroes n m = create n m 0.0

let copy m = Array.map Array.copy m

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


let matrixmult m1 m2 =
  let (nrows1, ncols1) = shape m1 in
  let (nrows2, ncols2) = shape m2 in
  if ncols1 <> nrows2 then
    raise (Invalid_argument "Matrix.matrix_mult: matrices must have compatible shapes");
  Array.init nrows1 (fun i ->
    Array.init ncols2 (fun j ->
      Array.fold_left ( +. ) 0.0 (Array.init ncols1 (fun k -> m1.(i).(k) *. m2.(k).(j)))
    )
  )
let vector_mult m v  =
  let (nrows, ncols) = shape m in
  if ncols <> Array.length v then
    raise (Invalid_argument "Matrix.vector_mult: matrix and vector must have compatible shapes");
  Array.init nrows (fun i -> Array.fold_left ( +. ) 0.0 (Array.map2 ( *. ) m.(i) v))


let det m = 
  let (nrows, ncols) = shape m in
  if nrows <> ncols then
    raise (Invalid_argument "Matrix.det: matrix must be square");
  let rec det' m = 
    match shape m with
    | (1, 1) -> m.(0).(0)
    | _ -> 
      let det'' i = 
        let m' = Array.init (nrows - 1) (fun j -> 
          Array.init (ncols - 1) (fun k -> 
            if k < i then m.(j + 1).(k) else m.(j + 1).(k + 1)
            )
          ) in
        m.(0).(i) *. det' m'
      in
      Array.fold_left ( +. ) 0.0 (Array.init ncols (fun i -> if i mod 2 = 0 then det'' i else -. det'' i))
  in
  det' m

  let decomposition m =
    let (nrows, ncols) = shape m in
    if nrows <> ncols then
      raise (Invalid_argument "Matrix.crout_lu_decomposition: matrix must be square");
    let lu = zeroes nrows ncols in
    let perm = Array.init nrows (fun i -> float_of_int i) in
    let toggle = ref 1 in
    for i = 0 to nrows - 1 do
      for j = 0 to i do
        let sum = ref m.(j).(i) in
        for k = 0 to j - 1 do
          sum := !sum -. lu.(j).(k) *. lu.(k).(i)
        done;
        lu.(j).(i) <- !sum
      done;
      let pivot = ref i in
      for j = i + 1 to nrows - 1 do
        let sum = ref m.(j).(i) in
        for k = 0 to i - 1 do
          sum := !sum -. lu.(j).(k) *. lu.(k).(i)
        done;
        lu.(j).(i) <- !sum;
        if abs_float lu.(j).(i) > abs_float lu.(!pivot).(i) then
          pivot := j
      done;
      if !pivot <> i then begin
        let temp = lu.(i) in
        lu.(i) <- lu.(!pivot);
        lu.(!pivot) <- temp;
        let temp = perm.(i) in
        perm.(i) <- perm.(!pivot);
        perm.(!pivot) <- temp;
        toggle := - !toggle
      end;
      for j = i + 1 to nrows - 1 do
        lu.(j).(i) <- lu.(j).(i) /. lu.(i).(i)
      done
    done;
    (lu, perm, !toggle)

let solver lu b = 
  let (nrows, ncols) = shape lu in 
  if nrows <> ncols then
    raise (Invalid_argument "Matrix.solver: matrix must be square");
  let x = Array.copy b in
    (* forward sub  Ly=b*)
  for i = 0 to nrows - 1 do
    let sum = ref x.(i) in
    for j = 0 to i do
      sum := !sum -. lu.(i).(j) *. x.(j)
    done;
  done;

  x.(nrows-1) <- x.(nrows-1) /. lu.(nrows-1).(nrows-1);
  (* Backward sub Ux=y *)
  for i = nrows - 2 downto 0 do
    let sum = ref x.(i) in
    for j = i + 1 to nrows - 1 do
      sum := !sum -. lu.(i).(j) *. x.(j)
    done;
    x.(i) <- !sum /. lu.(i).(i)
  done;
  x 

let inverse m = 
  let (nrows, ncols) = shape m in
  if nrows <> ncols then
    raise (Invalid_argument "Matrix.inverse: matrix must be square");
  let lu, perm, _toggle = decomposition m in
  let res = Array.copy m in
  Array.iteri (fun i _row -> 
    let b = Array.init nrows (fun j -> if float_of_int (i) = perm.(j) then 1.0 else 0.0) in
    let x = solver lu b in
    Array.iteri (fun j v -> res.(j).(i) <- v) x
  ) (res);
  res;
     

(* determinant *)
(* inverse, transpose *)
(* matrixmult, vector mult, dot and cross product *)

(* eigenvalues, eigenvectors  (might be optional )*)