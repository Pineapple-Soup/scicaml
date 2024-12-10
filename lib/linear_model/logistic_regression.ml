open Core

module LogisticRegression = struct
  type t = {
    mutable weights : Vector.t;
    mutable bias : float;
  }

  let init n_features = 
    { weights = Vector.zeroes n_features; bias = 0.0 }

  let sigmoid z = 1.0 /. (1.0 +. Float.exp (-.z))

  let predict model x =
    let bias_vector = Vector.create (Array.length x) model.bias in
    Vector.map sigmoid (Vector.add (Matrix.vector_mult x model.weights) bias_vector) in

  let logistic_loss model x y =
    let y_pred = predict model x in
    let loss = Array.map2 (fun y y_pred -> ((y *. Float.log y_pred) +. ((1.0 -. y) *. Float.log (1.0 -. y_pred)))) y y_pred in
    Vector.scale loss (-1.0) |> Vector.sum

  let gradient model x y = 
    let y_pred = predict model x in
    let diff = Vector.sub y y_pred in
    let grad_bias = Vector.sum diff in
    let grad_weights = Matrix.vector_mult (Matrix.transpose x) diff in
    (grad_weights, grad_bias)
  
  let step model x y learning_rate =
    let (grad_weights, grad_bias) = gradient model x y in
    model.weights <- Vector.add model.weights (Vector.scale grad_weights learning_rate);
    model.bias <- model.bias +. (grad_bias *. learning_rate)
  
  let gradient_descent model x y learning_rate n_iter =
    for _ = 1 to n_iter do
      step model x y learning_rate
    done

end