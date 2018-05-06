open Population
open Position
open Map

(* ========== Definition ========== *)

type individual =
  { id : int;
    mutable lifespan : int;
    mutable hp : int;
    max_hp : int;
    regen : int;
    attack_damage : int;
    mutable food_level : int;
    max_food_level : int;
    mutable food_stock : int;
    max_food_stock : int;
    food_decay : int;
    mutable position : position;
  }

(* ========== Individual counter (unique ID) ========== *)

let counter = ref 0

let reset_counter () =
  counter := 0

(* ========== Individual creation ========== *)

let create (population : population) =
  incr counter;
  { id = !counter;
    lifespan = 0;
    hp = population.max_hp;
    max_hp = population.max_hp;
    regen = population.regen;
    attack_damage = population.attack_damage;
    food_level = population.max_food_level;
    max_food_level = population.max_food_level;
    food_stock = population.max_food_stock;
    max_food_stock = population.max_food_stock;
    food_decay = population.food_decay;
    position = population.spawn_position }

let move_up individual map =
  let x, y = individual.position in
  let new_y = min (y + 1) (map.size - 1) in
  individual.position <- (x, new_y)

let move_down individual =
  let x, y = individual.position in
  let new_y = max (y - 1) 0 in
  individual.position <- (x, new_y)

let move_left individual =
  let x, y = individual.position in
  let new_x = max (x - 1) 0 in
  individual.position <- (new_x, y)

let move_right individual map =
  let x, y = individual.position in
  let new_x = min (x + 1) (map.size - 1) in
  individual.position <- (new_x, y)

let eat individual population map =
  let x, y = individual.position in
  let fx, fy = population.food_position in
  if (x = fx) && (y = fy) then
  begin
    let base_food_quantity = map.cells.(fx).(fy) in
    let current_food_level = individual.food_level in
    let quantity_to_eat = individual.max_food_level - current_food_level in
    let quantity_taken = min quantity_to_eat base_food_quantity in
    map.cells.(fx).(fy) <- base_food_quantity - quantity_taken;
    individual.food_level <- current_food_level + quantity_taken
  end

let copulate allies population map =
  let fx, fy = population.food_position in
  let popfood = map.cells.(fx).(fy) in
  let indcost = population.max_food_level in
  if popfood > indcost then
  begin
    map.cells.(fx).(fy) <- popfood - indcost;
    let newind = create population in
    (Array.append allies [|newind|], true)
  end
  else (allies, false)

let store individual map =
  let x, y = individual.position in
  map.cells.(x).(y) <- map.cells.(x).(y) + individual.food_stock;
  individual.food_stock <- 0

let collect individual map =
  let x, y = individual.position in
  let current_ind_stock = individual.food_stock in
  let current_map_stock = map.cells.(x).(y) in
  let space_available = individual.max_food_stock - current_ind_stock in
  let quantity_taken = min current_map_stock space_available in
  map.cells.(x).(y) <- current_map_stock - quantity_taken;
  individual.food_stock <- current_ind_stock + quantity_taken

(*
1v1
doit ramener sur la base pour manger
bouffe sert à manger et se reproduire
1 food rend 1 food level
chaque tour où food level != 0 remonte hp de reg rate sinon descend hp de reg rate

copulate crée un individu au spawn et consomme dans la base max food level de nourriture
attack ennemi à 1 case

manger remonte à max food level et consomme food stock

on peut poser bouffe partout

*)