open Core

module LinearRegression = struct
  type t = {
    mutable weights : Vector.t;
    mutable bias : float;
  }

  let init n_features = 
    { weights = Vector.zeroes n_features; bias = 0.0 }
  
  let predict model x =
    let bias_vector = Vector.create (Array.length x) model.bias in
    Vector.add (Matrix.vector_mult x model.weights) bias_vector
  
  let fit model x y =
    let x' = Matrix.append_vector x (Vector.ones (Vector.size y)) 1 in
    let x_t = Matrix.transpose x' in
    let x_t_x = Matrix.dot x_t x' in
    let x_t_y = Matrix.vector_mult x_t y in
    let x_t_x_inv = Matrix.inverse x_t_x in
    let weights = Matrix.vector_mult x_t_x_inv x_t_y in
    (* Extract bias from weights *)
    model.bias <- weights.(Vector.size weights - 1);
    model.weights <- Array.sub weights 0 (Vector.size weights - 1) 
  
  let quadratic_loss model x y =
    let y_pred = predict model x in
    let diff = Vector.sub y y_pred in
    Vector.map (fun x -> x *. x) diff |> Vector.sum
  
  let mse model x y =
    let y_pred = predict model x in
    let diff = Vector.sub y y_pred in
    Vector.map (fun x -> x *. x) diff |> Vector.mean
  
  let gradient model x y =
    let y_pred = predict model x in
    let diff = Vector.sub y y_pred in
    let x' = Matrix.append_vector x (Vector.ones (Vector.size y)) 1 in
    let grad = Matrix.vector_mult (Matrix.transpose x') diff in
    let grad_weights = Array.sub grad 0 (Vector.size grad - 1) in
    let grad_bias = grad.(Vector.size grad - 1) in
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