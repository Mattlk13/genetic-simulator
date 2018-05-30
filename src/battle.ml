open Neuralnet
open Population
open Individual
open Map
open View

(* ========== Definitions and constants ========== *)

(* total number of individuals created / total lifespan / sum of kills *)
type stats = int * float * int

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

let nb_input = 85
let train_ticks = 2000
let delay = 0.1

type choice = Up | Down | Right | Left | Attack | Eat | Copulate | Store | Collect

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

let without a ~id =
  let r = ref [] in
  for i = 0 to Array.length a - 1 do
    if a.(i).id <> id then
      r := a.(i) :: !r
  done;
  Array.of_list !r

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
      battle.l2 <- without battle.l2 enemy.id;
    end
    else
    begin
      let x, y, z = battle.stats1 in
      battle.stats1 <- (x, y +. (float_of_int enemy.lifespan), z);
      let x, y, z = battle.stats2 in
      battle.stats2 <- (x, y, z + 1);
      battle.l1 <- without battle.l1 enemy.id;
    end

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

let tick battle =
  for k = 0 to Array.length battle.l1 - 1 do
    battle.l1.(k).lifespan <- battle.l1.(k).lifespan + 1
  done;
  for k = 0 to Array.length battle.l2 - 1 do
    battle.l2.(k).lifespan <- battle.l2.(k).lifespan + 1
  done

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

let run battle =
  if battle.with_view then
    run_with_view battle
  else
    run_without_view battle

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

let rec group_map f groups =
  match groups with
  | [] -> []
  | group :: r -> (f ~group:group) :: (group_map f r)

let make_play ~groups ~size population ~popsize ~mapsize =
  let f = get_best_one ~size:size ~population:population ~popsize:popsize ~mapsize:mapsize in
  Array.of_list (group_map f groups)