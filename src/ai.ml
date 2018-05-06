open Population
open Neuralnet
open Battle

(* ========== Definition and constants ========== *)

type ai = neural_network array

let size = 160

let nb_group1 = 40
let group1_size = 4

let nb_group2 = 10
let group2_size = 4

let nb_best = 10
let nb_random = 40

let nb_input = 150
let nb_output = 9
let nb_layers = 4

let nb_iterations = 50

let mutation_probability = 0.05

let popsize = 20
let mapsize = 150
let p1 = Population.create 100 5 25 20 20 1 (5,5) (10,10) 0x0000FF
let p2 = Population.create 100 5 25 20 20 1 (145,145) (140,140) 0xFF0000

(* ========== AI creation and randomisation ========== *)

let create () =
  let f _ = Neuralnet.create nb_input nb_output nb_layers in
  Array.init size f

(* Durstenfeld shuffle O(n) *)
let shuffle ai =
  let n = Array.length ai in
  for i = 0 to n - 2 do
    let j = (Random.int (n - i)) + i in
    let tmp = ai.(i) in
    ai.(i) <- ai.(j);
    ai.(j) <- tmp
  done

(* ========== IO functions ========== *)

let read filename =
  if Sys.file_exists filename then
  begin
    let ic = Scanf.Scanning.open_in filename in
    let ai = create() in
    let fread_value line i =
      line.(i) <- Scanf.bscanf ic " %f" (fun x -> x)
    in
    let fread_line len line =
      for i = 0 to len - 1 do
        fread_value line i
      done
    in
    let fread_matrix matrix = Array.map (fread_line (Array.length matrix.(0))) matrix in
    let fread_network network = Array.map fread_matrix network.weights in
    let fread_ai ai =
      for i = 0 to nb_best - 1 do
        ignore (fread_network ai.(i))
      done
    in
    fread_ai ai;
    Scanf.Scanning.close_in ic;
    Some ai
  end
  else None

let write ai filename =
  let oc = open_out filename in
  let fwrite_line line = Array.map (Printf.printf "%f ") line in
  let fwrite_matrix matrix = Array.map fwrite_line matrix in
  let fwrite_network network = Array.map fwrite_matrix network.weights in
  let fwrite_ai ai =
    for i = 0 to nb_best - 1 do
      ignore (fwrite_network ai.(i))
    done
  in
  let _ = fwrite_ai ai in
  close_out oc

(* ========== Genetic algorithm functions ========== *)

let crossover_mutate a b result =
  for i_layer = 0 to nb_layers do
    let a_mat = a.weights.(i_layer) in
    let b_mat = b.weights.(i_layer) in
    let result_mat = result.weights.(i_layer) in
    let height = Array.length a.weights.(i_layer) in
    let width = Array.length a.weights.(i_layer).(0) in
    for i = 0 to height - 1 do
      for j = 0 to width - 1 do
        result_mat.(i).(j) <- if Random.bool() then a_mat.(i).(j) else b_mat.(i).(j); (* 50% a 50% b *)
        if Random.float 1. < mutation_probability then
          let t = (Random.float 0.4) +. 0.8 in (* [-20%;+20%] mutation rate *)
          let v = min (t *. result_mat.(i).(j)) Neuralnet.max_weight in
          let v = max v (-. Neuralnet.max_weight) in
          result_mat.(i).(j) <- v
      done
    done
  done

let generate ai best_individuals =
  for i = 0 to nb_best - 1 do
    ai.(i) <- best_individuals.(i)
  done;
  for i = nb_best to nb_best + nb_random - 1 do
    randomise ai.(i)
  done;
  for i = nb_best + nb_random to size - 1 do
    let r = Random.int nb_best in
    let a = best_individuals.(r) in
    let r = Random.int nb_best in
    let b = best_individuals.(r) in
    crossover_mutate a b ai.(i)
  done

let learn () =
  let brain =
    match read "ai.dat" with
    | None -> create()
    | Some ai -> ai
  in
  while true do
    for iteration = 0 to nb_iterations - 1 do
      print_int iteration;
      print_newline();
      shuffle brain;
      let k = ref 0 in
      let groups = ref [] in
      for i = 0 to nb_group1 - 1 do
        let group = ref [] in
        for j = 0 to group1_size - 1 do
          group := brain.(!k) :: !group;
          incr k
        done;
        groups := (Array.of_list !group) :: !groups
      done;
      let best_ones = Battle.make_play !groups group1_size p1 popsize mapsize in
      let k = ref 0 in
      let groups2 = ref [] in
      for i = 0 to nb_group2 - 1 do
        let group = ref [] in
        for j = 0 to group2_size - 1 do
          group := best_ones.(!k) :: !group;
          incr k
        done;
        groups2 := (Array.of_list !group) :: !groups2
      done;
      let best_of_the_best = Battle.make_play !groups2 group2_size p1 popsize mapsize in
      generate brain best_of_the_best
    done;
    print_endline "Writing to file...";
    write brain "ai.dat"
  done

(* ========== Function to play a game with a map ========== *)

let play () =
  let brain =
    match read "ai.dat" with
    | None -> create()
    | Some ai -> ai
  in
  let nn1 = brain.(0) in
  let nn2 = brain.(1) in
  let map = Map.create mapsize in
  let b = Battle.create p1 p2 popsize popsize nn1 nn2 map true in
  Battle.run b