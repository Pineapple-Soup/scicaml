type t = {
  features: Matrix.t;
  labels: Vector.t;
}

let create features labels = 
  let (rows, _) = Matrix.shape features in 
  if rows <> Vector.size labels then
    raise (Invalid_argument "Dataset.create: features and labels must have the same number of rows");
  { features; labels }

let shape dataset =
  (Matrix.shape dataset.features, Vector.size dataset.labels)

let describe dataset =
  Printf.sprintf "Dataset with %d rows and %d columns" (fst (Matrix.shape dataset.features)) (snd (Matrix.shape dataset.features))

let standardize dataset =
  let transposed_features = Matrix.transpose dataset.features in
  let new_cols = Array.map (fun row -> Vector.scale (Vector.sub row (Vector.create (Vector.size row) (Vector.mean row))) (1. /. Vector.std row)) transposed_features in
  let new_features = Matrix.transpose new_cols in
  create new_features dataset.labels