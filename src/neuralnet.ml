(* ========== Definitions and constants ========== *)

type neural_network =
  { nb_input : int;
    nb_output : int;
    nb_layers : int;
    layers : float array array; (* array of vectors *)
    weights : float array array array; (* array of matrices *)
  }

exception Break of int

let max_weight = 50.

(* ========== Useful functions ========== *)

let random_weight () = (Random.float (2. *. max_weight)) -. max_weight

let random_matrix ~lines ~columns =
  let f _ = random_weight() in
  let g _ = Array.init columns f in
  Array.init lines g

let round f =
  let ef = truncate f in
  let delta = f -. (float_of_int ef) in
  if (delta >= 0.5) then ef + 1 else ef

let dot_product u v =
  let r = ref 0. in
  for i = 0 to Array.length u - 1 do
    r := !r +. u.(i) *. v.(i)
  done;
  !r

(* ========== Neuronal network creation ========== *)

let create ~nb_input ~nb_output ~nb_layers =
  let a = (float_of_int (nb_output - nb_input)) /. (float_of_int (nb_layers + 1)) in
  let b = float_of_int nb_input in
  let layer_length i = round (a *. (float_of_int i) +. b) in
  let layer_lengths = Array.init (nb_layers + 2) layer_length in
  let make_layer i = Array.make layer_lengths.(i) 0. in
  let l = Array.init (nb_layers + 2) make_layer in
  let make_weight_matrix i = random_matrix layer_lengths.(i+1) layer_lengths.(i) in
  let w = Array.init (nb_layers + 1) make_weight_matrix in
  { nb_input = nb_input;
    nb_output = nb_output;
    nb_layers = nb_layers;
    layers = l;
    weights = w }

let randomise network =
  for i_layer = 0 to network.nb_layers do
    let matrix = network.weights.(i_layer) in
    let height = Array.length matrix in
    let width = Array.length matrix.(0) in
    for i = 0 to height - 1 do
      for j = 0 to width - 1 do
        matrix.(i).(j) <- random_weight()
      done
    done
  done

(* ========== Neuronal network functions ========== *)

let activation_function x =
  (*if x >= 0. then x else 0. (* ReLU *) *)
  1. /. (1. +. exp (-. x)) (* sigmoid *)

let activated_product ~matrix ~vector ~result =
  for i = 0 to Array.length matrix - 1 do
    result.(i) <- activation_function (dot_product matrix.(i) vector)
  done

let forward network ~inputs =
  network.layers.(0) <- inputs;
  for i = 0 to network.nb_layers do
    activated_product network.weights.(i) network.layers.(i) network.layers.(i+1)
  done

let choose network =
  let exp_values = Array.map exp network.layers.(network.nb_layers + 1) in
  let total_exp = Array.fold_left (+.) 0. exp_values in
  let proba = Array.make (network.nb_output + 1) 0. in
  for i = 1 to network.nb_output do
    proba.(i) <- exp_values.(i-1) /. total_exp +. proba.(i-1)
  done;
  let r = Random.float 1. in
  try
    for i = 0 to network.nb_output - 1 do
      if r < proba.(i+1) then raise (Break i)
    done;
    failwith "Network choice error"
  with Break i -> i