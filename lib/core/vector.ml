type t = float array

let create n value = Array.make n value

let zeroes n = create n 0.0

let size v = Array.length v

let add v1 v2 = 
  if Array.length v1 <> Array.length v2 then
    raise (Invalid_argument "Vector.add: vectors must have the same size");
  Array.map2 ( +. ) v1 v2