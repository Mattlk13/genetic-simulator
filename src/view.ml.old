open Population
open Individual
open Map

(* ========== View creation and deletion ========== *)

let create size =
  let s = size * 3 + 2 in
  Graphics.open_graph (Printf.sprintf " %dx%d" s s);
  Graphics.set_window_title "OCaml is love, OCaml is life"

let reset () =
  Graphics.set_color Graphics.black;
  let size = Graphics.size_x() in
  Graphics.fill_rect 0 0 size size;
  Graphics.set_color Graphics.white;
  Graphics.draw_rect 1 1 (size - 1) (size - 1)

let close () =
  Graphics.close_graph()

(* ========== ========== *)

let draw_map map =
  Graphics.set_color 0x336600;
  for i = 0 to map.size - 1 do
    for j = 0 to map.size - 1 do
      let q = map.cells.(i).(j) in
      let s =
        if (q > 90) then 9
        else if (q > 80) then 8
        else if (q > 70) then 7
        else if (q > 60) then 6
        else if (q > 50) then 5
        else if (q > 40) then 4
        else if (q > 30) then 3
        else if (q > 20) then 2
        else if (q > 10) then 1
        else 0
      in
      let rx = i * 3 + 1 in
      let ry = j * 3 + 1 in
      Graphics.fill_rect rx ry s s
    done
  done

let draw population individuals =
  Graphics.set_color population.colour;
  let f i =
    let x, y = i.position in
    let rx = x * 3 + 1 in
    let ry = y * 3 + 1 in
    Graphics.fill_rect rx ry 3 3
  in
  ignore (Array.map f individuals)