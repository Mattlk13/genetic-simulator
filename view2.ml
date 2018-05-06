open Population
open Individual
open Map

let image_filename = "testsdl_2.jpg"
let font_filename  = "testsdl_2.ttf"
let music_filename = "testsdl_2.mp3"

let run () =
    let screen = Sdlvideo.set_video_mode 400 400 [`DOUBLEBUF] in
    let image = Sdlloader.load_image image_filename in
    let font = Sdlttf.open_font font_filename 24 in
    let text = Sdlttf.render_text_blended font "Enjoy!" ~fg:Sdlvideo.white in
    let music = Sdlmixer.load_music music_filename in
    let position_of_image = Sdlvideo.rect 0 0 300 300 in
    let position_of_text = Sdlvideo.rect 300 0 300 300 in
    Sdlvideo.blit_surface ~dst_rect:position_of_image ~src:image ~dst:screen ();
    Sdlvideo.blit_surface ~dst_rect:position_of_text ~src:text ~dst:screen ();
    Sdlvideo.flip screen;
    Sdlmixer.fadein_music music 1.0;
    Sdltimer.delay 1000; (* fade in *)
    Sdltimer.delay 6000; (* play *)
    Sdlmixer.fadeout_music 2.0;
    Sdltimer.delay 2000; (* fade out *)
    Sdlmixer.halt_music ();
    Sdlmixer.free_music music

let main () =
    Sdl.init [`VIDEO; `AUDIO];
    at_exit Sdl.quit;
    Sdlttf.init ();
    at_exit Sdlttf.quit;
    Sdlmixer.open_audio ();
    at_exit Sdlmixer.close_audio;
    run ()

let _ = main ()

(* ========== View creation and deletion ========== *)



let create size =
  let s = size * 3 + 2 in
  Graphics.open_graph (Printf.sprintf " %dx%d" s s);
  let window = new window (OcsfmlGraphics.VideoMode.create ~w:800 ~h:600 ()) "SFML window" in
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