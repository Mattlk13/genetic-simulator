open Population
open Position
open Map

(* This file defines an individual in the game *)

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

(* ========== Individual counter (unique ID for each individual) ========== *)

let counter = ref 0

let reset_counter () =
  counter := 0

(* ========== Individual creation ========== *)

(* Creates an individual according to its population's stats *)
(* val create : population -> individual *)
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

(* Maximum speed of the individuals *)
let max_speed = 5.0

(* The next 4 functions define displacements for an individual on a map, according to its speed *)

(* val move_up : individual -> map -> unit *)
let move_up individual map =
  let x, y = individual.position in
  let d = float_of_int individual.food_level /. float_of_int individual.max_food_level *. max_speed in
  let new_y = min (y + truncate d) (map.size - 1) in
  individual.position <- (x, new_y)

(* val move_down : individual -> map -> unit *)
let move_down individual =
  let x, y = individual.position in
  let d = float_of_int individual.food_level /. float_of_int individual.max_food_level *. max_speed in
  let new_y = max (y - truncate d) 0 in
  individual.position <- (x, new_y)

(* val move_left : individual -> map -> unit *)
let move_left individual =
  let x, y = individual.position in
  let d = float_of_int individual.food_level /. float_of_int individual.max_food_level *. max_speed in
  let new_x = max (x - truncate d) 0 in
  individual.position <- (new_x, y)

(* val move_right : individual -> map -> unit *)
let move_right individual map =
  let x, y = individual.position in
  let d = float_of_int individual.food_level /. float_of_int individual.max_food_level *. max_speed in
  let new_x = min (x + truncate d) (map.size - 1) in
  individual.position <- (new_x, y)

(* Makes an individual eat food : it takes food from the base and regenerates the individual's food level *)
(* val eat : individual -> population -> map -> unit *)
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

(* Creates a new individual if there is enough food in the base (population.max_food_level needed) *)
(* Returns the new individual array (same if not enough food) *)
(* It also returns a boolean (true if a new individual has been created, false otherwise) *)
(* val copulate : individual array -> population -> map -> individual array * bool *)
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

(* The individual stores all of its food at its location *)
(* val store : individual -> map -> unit *)
let store individual map =
  let x, y = individual.position in
  map.cells.(x).(y) <- map.cells.(x).(y) + individual.food_stock;
  individual.food_stock <- 0

(* The individual collects food on the map, according to the place it has (food_stock) *)
(* val collect : individual -> map -> unit *)
let collect individual map =
  let x, y = individual.position in
  let current_ind_stock = individual.food_stock in
  let current_map_stock = map.cells.(x).(y) in
  let space_available = individual.max_food_stock - current_ind_stock in
  let quantity_taken = min current_map_stock space_available in
  map.cells.(x).(y) <- current_map_stock - quantity_taken;
  individual.food_stock <- current_ind_stock + quantity_taken