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

let from_csv file label_col =
  let data = Csv.load file in
  let rows = List.length data in
  let cols = List.length (List.hd data) in
  let features = Matrix.create rows cols 0.0 in
  let labels = Vector.create rows 0.0 in
  List.iteri (fun i row ->
    List.iteri (fun j elem ->
      if j = label_col then
        labels.(i) <- float_of_string elem
      else
        features.(i).(j) <- float_of_string elem
    ) row) data;
  create (Matrix.drop features label_col 1) labels