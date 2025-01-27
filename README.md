# SciCaml
A machine learning library written in OCaml
## Features
- Supervised learning algorithms
<!-- - Unsupervised learning algorithms -->
- Data preprocessing utilities
- Matrix/Linear algebra tools
- Model evaluation metrics

## Installation
To install SciCaml, clone the repository and build the project using `dune`:
```sh
git clone https://github.com/pineapple-soup/scicaml.git
cd scicaml
dune build
```

## Usage
Here is a simple example of how to use SciCaml:
```ocaml
open Scicaml

let () =
  let data = Dataset.load_csv "data.csv" in
  let model = LinearRegression.train data in
  let predictions = LinearRegression.predict model data in
  Printf.printf "Predictions: %s\n" (string_of_predictions predictions)
```

## License
This project is licensed under the MIT License.