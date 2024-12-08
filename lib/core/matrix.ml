type t = float array array

let create n m ?(value=0.0) = Array.make_matrix n m 0.0

let shape m = (Array.length m, Array.length m.(0))