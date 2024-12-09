type t = {
  features: Matrix.t;
  labels: Vector.t;
}

let create features labels = 
  let (rows, _) = Matrix.shape features in 
  if rows <> Vector.size labels then
    raise (Invalid_argument "Dataset.create: features and labels must have the same number of rows");
  { features; labels }