open Position

(* ========== Definition ========== *)

let counter = ref 0

let reset_counter () =
  counter := 0

type population =
  { id : int;
    max_hp : int;
    regen : int;
    attack_damage : int;
    max_food_level : int;
    max_food_stock : int;
    food_decay : int;
    spawn_position : position;
    food_position : position;
    colour : int;
  }

(* ========== Population creation ========== *)

let create ~max_hp ~regen ~attack_damage ~max_food_level ~max_food_stock
  ~food_decay ~spawn_position ~food_position ~colour =
  incr counter;
  { id = !counter; max_hp; regen; attack_damage; max_food_level; max_food_stock;
    food_decay; spawn_position; food_position; colour }