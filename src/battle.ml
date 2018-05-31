open Neuralnet
open Population
open Individual
open Map
open View

(* This file manages battles between populations *)

(* ========== Definitions and constants ========== *)

(* Heuristics ! *)
(* total number of individuals created / total lifespan / sum of kills *)
type stats = int * float * int

(* Every battle contains the individuals from each population, and stats *)
type battle =
  { p1 : population;
    mutable l1 : individual array;
    nn1 : neural_network;
    mutable stats1 : stats;
    p2 : population;
    mutable l2 : individual array;
    nn2 : neural_network;
    mutable stats2 : stats;
    map : map;
    with_view : bool;
  }

(* Number of inputs in the neural networks *)
let nb_input = 85

(* Duration of battles when learning *)
let train_ticks = 2000

(* Time to wait before next tick when playing a game graphically (we need to have time to see) *)
let delay = 0.1

(* Different possible choices for an individual *)
type choice = Up | Down | Right | Left | Attack | Eat | Copulate | Store | Collect

(* Translates a number into a choice *)
(* val make_choice : int -> choice *)
let make_choice i =
  match i with
  | 0 -> Up
  | 1 -> Down
  | 2 -> Right
  | 3 -> Left
  | 4 -> Attack
  | 5 -> Eat
  | 6 -> Copulate
  | 7 -> Store
  | 8 -> Collect
  | _ -> failwith "Nonexistent choice"

(* debug *)
let print_choice c =
  let s =
    match c with
    | Up -> "Up"
    | Down -> "Down"
    | Right -> "Right"
    | Left -> "Left"
    | Attack -> "Attack"
    | Eat -> "Eat"
    | Copulate -> "Copulate"
    | Store -> "Store"
    | Collect -> "Collect"
  in print_string s

(* Removes an individual from an array, knowing its ID *)
(* val without : individual array -> id:int -> individual array *)
let without a ~id =
  let r = ref [] in
  for i = 0 to Array.length a - 1 do
    if a.(i).id <> id then
      r := a.(i) :: !r
  done;
  Array.of_list !r

(* The few next functions need an integer parameter i, to know which population the individual is from *)

(* Function to deal damage to an enemy, and possibly kill it *)
(* val damage : battle -> me:individual -> enemy:individual -> int -> unit *)
let damage battle ~me ~enemy i =
  let dmg = me.attack_damage in
  let hp = max 0 (enemy.hp - dmg) in
  if hp > 0 then
  begin
    (* The enemy does not die *)
    let enemies = if i = 1 then battle.l2 else battle.l1 in
    for k = 0 to Array.length enemies - 1 do
      if enemies.(k).id = enemy.id then
        enemies.(k).hp <- hp
    done
  end
  else
    (* The enemy dies, the average lifespan of its population is modified and the kill counter on the other one are modified too *)
    if i = 1 then
    begin
      let x, y, z = battle.stats2 in
      battle.stats2 <- (x, y +. (float_of_int enemy.lifespan), z);
      let x, y, z = battle.stats1 in
      battle.stats1 <- (x, y, z + 1);
      battle.l2 <- without battle.l2 enemy.id
    end
    else
    begin
      let x, y, z = battle.stats1 in
      battle.stats1 <- (x, y +. (float_of_int enemy.lifespan), z);
      let x, y, z = battle.stats2 in
      battle.stats2 <- (x, y, z + 1);
      battle.l1 <- without battle.l1 enemy.id
    end


(* Makes an individual attack its closest enemy *)
(* val attack : individual -> battle -> int -> unit *)
let attack individual battle i =
  let f (u,v) ind =
    let pos = ind.position in
    (pos = (u,v)) || (pos = (u+1,v)) || (pos = (u-1,v)) || (pos = (u,v+1)) || (pos = (u,v-1))
  in
  let filter_individuals p a =
    let r = ref [] in
    for i = 0 to Array.length a - 1 do
      if p a.(i) then
        r := a.(i) :: !r
    done;
    Array.of_list !r
  in
  let enemies = if i = 1 then battle.l2 else battle.l1 in
  let close_enemies = filter_individuals (f individual.position) enemies in
  let n = Array.length close_enemies in
  if n > 0 then
  begin
    let r = Random.int n in
    let enemy = close_enemies.(r) in
    damage battle individual enemy i
  end

(* The individual regenerates its health every tick, or loses health points if it does not have enough food *)
(* val regen : individual -> battle -> int -> unit *)
let regen individual battle i =
  let newfoodlevel = individual.food_level - individual.food_decay in
  if (newfoodlevel >= 0) then
  begin
    (* enough food level *)
    individual.food_level <- newfoodlevel;
    individual.hp <- min (individual.hp + individual.regen) individual.max_hp
  end
  else
  begin
    (* not enough food level *)
    individual.food_level <- 0;
    let newstocklevel = individual.food_stock + newfoodlevel in
    if (newstocklevel >= 0) then
    begin
      (* enough food stock, still ok *)
      individual.food_stock <- newstocklevel;
      individual.hp <- min (individual.hp + individual.regen) individual.max_hp
    end
    else
    begin
      (* nothing left in the food stock, the individual will lose hp *)
      let newhp = individual.hp - individual.regen in
      if (newhp > 0) then
        (* the individual will not die now, but it loses hp *)
        individual.hp <- newhp
      else
      begin
        (* the individual dies, the average lifespan of its population is modified *)
        if i = 1 then
        begin
          let x, y, z = battle.stats1 in
          battle.stats1 <- (x, y +. (float_of_int individual.lifespan), z);
          battle.l1 <- without battle.l1 individual.id;
        end
        else
        begin
          let x, y, z = battle.stats2 in
          battle.stats2 <- (x, y +. (float_of_int individual.lifespan), z);
          battle.l2 <- without battle.l2 individual.id;
        end
      end
    end
  end

(* ========== Battle creation ========== *)

(* Creates a battle with 2 populations, 2 sizes, 2 neural networks, a map *)
(* and a boolean to know whether a view is to be displayed *)
(* val create : p1:population -> p2:population -> n1:int -> n2:int -> nn1:neural_network -> nn2:neural_network -> map:map -> view_needed:bool *)
let create ~p1 ~p2 ~n1 ~n2 ~nn1 ~nn2 ~map ~view_needed =
  let f1 _ = Individual.create p1 in
  let f2 _ = Individual.create p2 in
  let b =
  { p1 = p1;
    l1 = Array.init n1 f1;
    nn1 = nn1;
    stats1 = (0, 0., 0);
    p2 = p2;
    l2 = Array.init n2 f2;
    nn2 = nn2;
    stats2 = (0, 0., 0);
    map = map;
    with_view = view_needed } in
  b.stats1 <- (Array.length b.l1, 0., 0);
  b.stats2 <- (Array.length b.l2, 0., 0);
  b

(* ========== Battle functions ========== *)

(* Generates inputs for the neural network given data about the game *)
(* val generate_inputs : individual -> individual array -> population -> individual array -> population -> map -> float array *)
let generate_inputs individual allies p_allies enemies p_enemies map =
  (* First inputs (6) *)
  let inputs = Array.make nb_input 0 in
  inputs.(0) <- individual.hp;
  inputs.(1) <- individual.food_level;
  inputs.(2) <- individual.food_stock;
  let x, y = individual.position in
  let bx, by = p_allies.food_position in
  inputs.(3) <- map.cells.(bx).(by);
  inputs.(4) <- x - bx;
  inputs.(5) <- y - by;

  (* Food map cells inputs (49) *)
  let k = ref 6 in
  for i = x - 3 to x + 3 do
    for j = y - 3 to y + 3 do
      let v =
        if (i < 0) || (i > map.size - 1) || (j < 0) || (j > map.size - 1) then 0
        else map.cells.(i).(j)
      in
      inputs.(!k) <- v;
      incr k
    done
  done;
  
  let get_distances ind =
    let xi, yi = ind.position in
    let dx = x - xi in
    let dy = y - yi in
    let d = abs dx + abs dy in
    (ind, dx, dy, d)
  in

  (* Comparison function : order by manhattan distance to individual *)
  let cmp (i1, dx1, dy1, d1) (i2, dx2, dy2, d2) = compare d1 d2 in

  (* Close allies inputs (15) *)
  let allies_positions =
    let t = Array.map get_distances (without allies individual.id) in
    Array.sort cmp t;
    Array.map (fun (i, dx, dy, d) -> (dx, dy)) t
  in
  let k = ref 55 in
  for i = 0 to 4 do
    try
      let dx, dy = Array.get allies_positions i in
      inputs.(!k) <- dx; incr k;
      inputs.(!k) <- dy; incr k;
      inputs.(!k) <- 1; incr k
    with
    | Invalid_argument _ -> begin
      inputs.(!k) <- 0; incr k;
      inputs.(!k) <- 0; incr k;
      inputs.(!k) <- 0; incr k
    end
  done;

  (* Close enemies inputs (15) *)
  let enemies_positions =
    let t = Array.map get_distances enemies in
    Array.sort cmp t;
    Array.map (fun (i, dx, dy, d) -> (dx, dy)) t
  in
  let k = ref 70 in
  for i = 0 to 4 do
    try
      let dx, dy = Array.get enemies_positions i in
      inputs.(!k) <- dx; incr k;
      inputs.(!k) <- dy; incr k;
      inputs.(!k) <- 1; incr k
    with
    | Invalid_argument _ -> begin
      inputs.(!k) <- 0; incr k;
      inputs.(!k) <- 0; incr k;
      inputs.(!k) <- 0; incr k
    end
  done;

  Array.map float_of_int inputs

(* This function lets us know which population has won, computing the scores from the heuristics *)
(* val compute_scores : battle -> float * float *)
let compute_scores battle =
  let x, y, kills = battle.stats1 in
  let average_lifespan = y /. (float_of_int x) in
  let alive_at_the_end = Array.length battle.l1 in
  let score1 = (float_of_int alive_at_the_end) +. average_lifespan +. (float_of_int kills) in
  let x, y, kills = battle.stats2 in
  let average_lifespan = y /. (float_of_int x) in
  let alive_at_the_end = Array.length battle.l2 in
  let score2 = (float_of_int alive_at_the_end) +. average_lifespan +. (float_of_int kills) in
  (score1, score2)

(* Increments every alive individual's lifespan (actually applies a tick) *)
(* val tick : battle -> unit *)
let tick battle =
  for k = 0 to Array.length battle.l1 - 1 do
    battle.l1.(k).lifespan <- battle.l1.(k).lifespan + 1
  done;
  for k = 0 to Array.length battle.l2 - 1 do
    battle.l2.(k).lifespan <- battle.l2.(k).lifespan + 1
  done

(* Makes a whole population play (move, eat, attack...) and updates the stats accordingly *)
(* val make_individuals_play : battle -> int -> unit *)
let make_individuals_play battle n =
  let allies = if n = 1 then battle.l1 else battle.l2 in
  let enemies = if n = 1 then battle.l2 else battle.l1 in
  let network = if n = 1 then battle.nn1 else battle.nn2 in
  let p_allies = if n = 1 then battle.p1 else battle.p2 in
  let p_enemies = if n = 1 then battle.p2 else battle.p1 in
  let map = battle.map in
  (* debug
  print_string "POPULATION ";
  print_int n;
  print_newline(); *)
  for i = 0 to Array.length allies - 1 do
    let inputs = generate_inputs allies.(i) allies p_allies enemies p_enemies map in
    Neuralnet.forward network inputs;
    let c = make_choice (Neuralnet.choose network) in
    (* debug *)
    (* print_string ">> ";
    print_int i;
    print_string " - hp:";
    print_int allies.(i).hp;
    print_string " - fl:";
    print_int allies.(i).food_level;
    print_string " - fs:";
    print_int allies.(i).food_stock;
    print_string " - ";
    print_choice c;
    print_newline(); *)
    let () =
      match c with
      | Up -> Individual.move_up allies.(i) map
      | Down -> Individual.move_down allies.(i)
      | Right -> Individual.move_right allies.(i) map
      | Left -> Individual.move_left allies.(i)
      | Attack -> attack allies.(i) battle n
      | Eat -> Individual.eat allies.(i) p_allies map
      | Copulate ->
        let new_allies, modified = Individual.copulate allies p_allies map in
        if modified then
        begin
          if n = 1 then
          begin
            let x, y, z = battle.stats1 in
            battle.stats1 <- (x + 1, y, z);
            battle.l1 <- new_allies
          end
          else
          begin
            let x, y, z = battle.stats2 in
            battle.stats2 <- (x + 1, y, z);
            battle.l2 <- new_allies
          end
        end
      | Store -> Individual.store allies.(i) battle.map
      | Collect -> Individual.collect allies.(i) battle.map
    in
    regen allies.(i) battle n
  done

(* Runs a battle from the beginning to the end. It stops after train_ticks ticks or if a population is dead *)
(* This version runs with a view, the one below does not start the view (runs faster to learn faster) *)
(* Returns a boolean (has the first player won ?) *)
(* val run_with_view : battle -> bool *)
let run_with_view battle =
  View.init();
  View.create battle.map.size;
  let () =
    try
      for ticks = 0 to train_ticks - 1 do
        let before = Unix.gettimeofday() in
        make_individuals_play battle 1;
        make_individuals_play battle 2;
        tick battle;
        View.reset();
        View.draw_map battle.map;
        View.draw battle.p1 battle.l1;
        View.draw battle.p2 battle.l2;
        View.update();
        View.check_event();
        let after = Unix.gettimeofday() in
        let t = after -. before in
        let towait = max (delay -. t) 0. in
        Unix.sleepf towait;
        if (Array.length battle.l1 = 0) || (Array.length battle.l2 = 0) then raise Exit
      done
    with Exit -> ()
  in
  let score1, score2 = compute_scores battle in
  (score1 > score2)

(* val run_without_view : battle -> bool *)
let run_without_view battle =
  let () =
    try
      for ticks = 0 to train_ticks - 1 do
        make_individuals_play battle 1;
        make_individuals_play battle 2;
        tick battle;
        if (Array.length battle.l1 = 0) || (Array.length battle.l2 = 0) then raise Exit
      done
    with Exit -> ()
  in
  let score1, score2 = compute_scores battle in
  (score1 > score2)

(* This one is the general "run" function, it decides to start a view or not from the with_view attribute of the battle *)
(* val run : battle -> bool *)
let run battle =
  if battle.with_view then
    run_with_view battle
  else
    run_without_view battle

(* Makes a battle between two neural networks *)
(* val make_battle : neural_network -> neural_network -> population -> popsize:int -> mapsize:int -> bool *)
let make_battle a b population ~popsize ~mapsize =
  let m = Map.create mapsize in
  let battle = create population population popsize popsize a b m false in
  (* run battle *)
  (* total number of individuals created / total lifespan / sum of kills *)
  (* debug *)
  let res = run battle in
  let (a, b, c), (d, e, f) = battle.stats1, battle.stats2 in
  Printf.printf "(%d, %f, %d) (%d, %f, %d)\n" a b c d e f;
  res

(* Finds out who is the best individual of a group (group battles as explained in ai.ml) *)
(* val get_best_one : group:neural_network array -> size:int -> population:population -> popsize:int -> mapsize:int -> neural_network *)
let get_best_one ~group ~size ~population ~popsize ~mapsize =
  let scores = Array.make size 0 in
  for i = 0 to size - 2 do
    for j = i + 1 to size - 1 do
      (* debug *)
      print_string "battle: ";
      print_int i;
      print_string " - ";
      print_int j;
      print_newline();
      if make_battle group.(i) group.(j) population popsize mapsize then
        scores.(i) <- scores.(i) + 1
      else
        scores.(j) <- scores.(j) + 1
    done
  done;
  let max_index = ref (-1) in
  let max_score = ref (-1) in
  for i = 0 to size - 1 do
    if scores.(i) > !max_score then
    begin
      max_score := scores.(i);
      max_index := i
    end
  done;
  (* debug *)
  print_endline "group done";
  group.(!max_index)

(* Just a map function, working with functions taking a "group" named parameter *)
(* val group_map : (f:'a -> 'b) -> 'a list -> 'b list *)
let rec group_map f groups =
  match groups with
  | [] -> []
  | group :: r -> (f ~group:group) :: (group_map f r)

(* NB : this map function is the key if we want to make the computations parallel *)

(* This is called by the AI, it is the main function to get the best individuals from each group *)
(* val make_play : groups:neural_network array list -> size:int -> population -> popsize:int -> mapsize:int -> neural_network array *)
let make_play ~groups ~size population ~popsize ~mapsize =
  let f = get_best_one ~size:size ~population:population ~popsize:popsize ~mapsize:mapsize in
  Array.of_list (group_map f groups)