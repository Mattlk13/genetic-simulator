open Population
open Individual
open Map

let screen_size = ref 0
let screen = ref None
let music_filename = "toto-africa.mp3"

(* ========== Drawing functions ========== *)

let draw_rect ~x ~y ~height ~width ~color ~surface =
  for i = x to x + width - 1 do
    Sdlvideo.put_pixel_color surface i y color;
    Sdlvideo.put_pixel_color surface i (y + height - 1) color
  done;
  for j = y to y + height - 1 do
    Sdlvideo.put_pixel_color surface x j color;
    Sdlvideo.put_pixel_color surface (x + width - 1) j color
  done

let draw_map map =
  match !screen with
  | None -> ()
  | Some surface ->
    let color = Sdlvideo.map_RGB surface (0x33, 0x66, 0x00) in
    for i = 0 to map.size - 1 do
      for j = 0 to map.size - 1 do
        let q = map.cells.(i).(j) in
        let s =
          if (q > 90) then 5
          else if (q > 70) then 4
          else if (q > 50) then 3
          else if (q > 30) then 2
          else if (q > 10) then 1
          else 0
        in
        let rx = i * 5 + 1 in
        let ry = j * 5 + 1 in
        let r = Sdlvideo.rect rx ry s s in
        Sdlvideo.fill_rect ~rect:r surface color
      done
    done

let draw population individuals =
  match !screen with
  | None -> ()
  | Some surface ->
    let popcolor = population.colour in
    let r = (popcolor land 0xFF0000) lsr 16 in
    let g = (popcolor land 0x00FF00) lsr 8 in
    let b = popcolor land 0x0000FF in
    let color = Sdlvideo.map_RGB surface (r, g, b) in
    let f i =
      let x, y = i.position in
      let rx = x * 5 + 1 in
      let ry = y * 5 + 1 in
      let r = Sdlvideo.rect rx ry 5 5 in
      Sdlvideo.fill_rect ~rect:r surface color
    in
    ignore (Array.map f individuals)

(* ========== View general functions ========== *)

let init () =
  Sdl.init [`VIDEO; `AUDIO];
  at_exit Sdl.quit;
  Sdlmixer.open_audio();
  at_exit Sdlmixer.close_audio;
  let music = Sdlmixer.load_music music_filename in
  at_exit (fun () -> Sdlmixer.free_music music);
  Sdlmixer.play_music music;
  at_exit Sdlmixer.halt_music

let create size =
  let s = size * 5 + 2 in
  screen := Some (Sdlvideo.set_video_mode ~w:s ~h:s ~bpp:16 [`HWSURFACE; `DOUBLEBUF]);
  screen_size := s

let reset () =
  match !screen with
  | None -> ()
  | Some surface ->
    Sdlvideo.fill_rect surface (Sdlvideo.map_RGB surface Sdlvideo.black);
    draw_rect 0 0 !screen_size !screen_size Sdlvideo.white surface

let update () =
  match !screen with
  | None -> ()
  | Some surface -> Sdlvideo.flip surface

let check_event () =
  match Sdlevent.poll() with
  | None -> ()
  | Some e ->
    begin
      match e with
      | Sdlevent.QUIT -> exit 1
      | Sdlevent.KEYDOWN ke -> if ke.Sdlevent.keysym = Sdlkey.KEY_q then exit 1
      | _ -> ()
    end