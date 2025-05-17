open Tsdl_mixer

let init_audio () =
  match Mixer.init Mixer.Init.mp3 with
  | Ok _ ->
    begin match Mixer.open_audio 44100 Mixer.default_format 2 2048 with
    | Ok () -> ()
    | Error (`Msg e) -> failwith ("Failed to open audio: " ^ e)
    end
  | Error (`Msg e) -> failwith ("Failed to initialize SDL2_mixer: " ^ e)

let play_music file =
  match Mixer.load_mus file with
  | Ok music ->
      begin match Mixer.play_music music (-1) with
      | Ok _ -> ()
      | Error (`Msg e) -> failwith ("Failed to play music: " ^ e)
      end
  | Error (`Msg e) -> failwith ("Failed to load music: " ^ e)

let play_sound file =
  match Mixer.load_wav file with
  | Ok chunk ->
      begin match Mixer.play_channel (-1) chunk 0 with
      | Ok _ -> ()
      | Error (`Msg e) -> failwith ("Failed to play sound: " ^ e)
      end
  | Error (`Msg e) -> failwith ("Failed to load sound: " ^ e)

let stop_music () =
  match Mixer.halt_music () with
  | Ok () -> ()
  | Error (`Msg e) -> failwith ("Failed to stop music: " ^ e)