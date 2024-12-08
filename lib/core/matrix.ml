type t = float array array

let create n m value = Array.make_matrix n m value

let zeroes n m = create n m 0.0

let shape m = (Array.length m, Array.length m.(0))