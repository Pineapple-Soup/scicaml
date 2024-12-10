type t = float array array

let create n m value = 
  if n < 0 || m < 0 then
    raise (Invalid_argument "Matrix.create: size must be non-negative");
  Array.make_matrix n m value

let zeroes n m = create n m 0.0

let print m =
  print_newline ();
  Array.iter (fun row ->
    Array.iter (fun elem ->
      Printf.printf "%f " elem  (* Print each element in the row *)
    ) row;
    print_newline ()  (* Move to the next line after each row *)
  ) m

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

let vstack m1 m2 =
  let (nrows1, ncols1) = shape m1 in
  let (nrows2, ncols2) = shape m2 in
  if ncols1 <> ncols2 then
    raise (Invalid_argument "Matrix.vstack: matrices must have the same number of columns");
  Array.init (nrows1 + nrows2) (fun i -> if i < nrows1 then m1.(i) else m2.(i - nrows1))

let hstack m1 m2 =
  let nrows1 = shape m1 |> fst in
  let nrows2 = shape m2 |> fst in
  if nrows1 <> nrows2 then
    raise (Invalid_argument "Matrix.hstack: matrices must have the same number of rows");
  Array.init nrows1 (fun i -> Array.append m1.(i) m2.(i))

let append m1 m2 axis = 
  match axis with
  | 0 -> vstack m1 m2
  | 1 -> hstack m1 m2
  | _ -> raise (Invalid_argument "Matrix.append: axis must be 0 or 1")
    
let append_vector m v axis = 
  match axis with
  | 0 -> vstack m [|v|]
  | 1 -> hstack m (transpose [|v|])
  | _ -> raise (Invalid_argument "Matrix.append_vector: axis must be 0 or 1")


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





let decomposition (m: t) =
  let (nrows, ncols) = shape m in
  if nrows <> ncols then
    raise (Invalid_argument "Matrix.crout_lu_decomposition: matrix must be square");
  let lu = copy m in
  let perm = Array.init nrows (fun i -> float_of_int i) in
  let toggle = ref 1 in
  for j = 0 to nrows -2 do
    let max = ref (abs_float(lu.(j).(j))) in 
    let piv = ref j in 
    for i = j + 1 to nrows-1 do
      let temp = abs_float(lu.(i).(j)) in 
      if temp > !max then (
        max := temp;
        piv := i
      )
    done;
    if !piv <> j then (
      for k = 0 to nrows - 1 do
        let xij = lu.(!piv).(k) in 
        lu.(!piv).(k) <- lu.(j).(k);
        lu.(j).(k) <- xij
      done;
      let xij = perm.(!piv) in 
      perm.(!piv) <- perm.(j);
      perm.(j) <- xij;
      toggle := - !toggle
    );
    let xjj = lu.(j).(j) in
    if abs_float xjj > 1.0e-10 then
      for i = j + 1 to nrows-1 do
        let xij = lu.(i).(j) /. xjj in
        lu.(i).(j) <- xij;
        for k = j + 1 to nrows-1 do
          lu.(i).(k) <- lu.(i).(k) -. xij *. lu.(j).(k)
        done
      done
  done;
  (lu, perm, !toggle)


let det m = 
  let (nrows, ncols) = shape m in
  if nrows <> ncols then
    raise (Invalid_argument "Matrix.det: matrix must be square");
  let lu, _perm, toggle = decomposition m in
  let det = ref (float_of_int toggle) in
  for i = 0 to nrows - 1 do
    det := !det *. lu.(i).(i)
  done;
  !det 




let solver lu b = 
  let (nrows, ncols) = shape lu in 
  if nrows <> ncols then
    raise (Invalid_argument "Matrix.solver: matrix must be square");
  let x = Array.copy b in
    (* forward sub  Ly=b*)
  for i = 1 to nrows - 1 do
    let sum = ref x.(i) in
    for j = 0 to i-1 do
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