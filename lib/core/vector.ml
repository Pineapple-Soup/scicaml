type t = float array


let arr = [|1.0; 2.0; 3.0|]



let create n value = 
  if n < 0 then
    raise (Invalid_argument "Vector.create: size must be non-negative");
  Array.make n value

let zeroes n = create n 0.0

let ones  n = create n 1.0

let size v = Array.length v

let add v1 v2 = 
  if Array.length v1 <> Array.length v2 then
    raise (Invalid_argument "Vector.add: vectors must have the same size");
  Array.map2 ( +. ) v1 v2

let sub v1 v2 = 
  if Array.length v1 <> Array.length v2 then
    raise (Invalid_argument "Vector.sub: vectors must have the same size");
  Array.map2 ( -. ) v1 v2

let scale v s = Array.map(fun x -> x *. s) v

let map func v1 = 
  Array.map func v1

let dot v1 v2 = 
  if Array.length v1 <> Array.length v2 then
    raise (Invalid_argument "Vector.dot: vectors must have the same size");
  Array.fold_left ( +. ) 0.0 (Array.map2 ( *. ) v1 v2)


let norm1 v = 
  Array.fold_left ( +. ) 0.0 (Array.map abs_float v)

let norm2 v =
  sqrt (Array.fold_left ( +. ) 0.0 (Array.map (fun x -> x *. x) v))

let norm_inf v =
  Array.fold_left max 0.0 (Array.map abs_float v)

let max v = 
  Array.fold_left max v.(0) v

let min v = 
  Array.fold_left min v.(0) v

let argmax v = 
  let rec argmax' i max_i max_v = 
    if i = Array.length v then
      max_i
    else if v.(i) > max_v then
      argmax' (i + 1) i v.(i)
    else
      argmax' (i + 1) max_i max_v
  in
  argmax' 0 0 v.(0)

let argmin v = 
  let rec argmin' i min_i min_v = 
    if i = Array.length v then
      min_i
    else if v.(i) > min_v then
      argmin' (i + 1) i v.(i)
    else
      argmin' (i + 1) min_i min_v
  in
  argmin' 0 0 v.(0)

let mean v = 
  (Array.fold_left ( +. ) 0.0 v) /. (float_of_int (Array.length v))

let variance v = 
  let m = mean v in
  let n = float_of_int (Array.length v) in
  (Array.fold_left (fun acc x -> acc +. (x -. m) ** 2.0) 0.0 v) /. n

let std v =
  sqrt (variance v)


(* element operators *)
(* scalars, norms *)
(* max, min, argmax, argmin *)
