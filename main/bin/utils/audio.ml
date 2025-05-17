open Tsdl_mixer

(** 
  [init_audio ()] initializes the SDL2_mixer library and opens the audio device.
  @raise Failure if initialization fails.
*)
let init_audio () : unit =
  match Mixer.init Mixer.Init.mp3 with
  | Ok _ ->
    begin match Mixer.open_audio 44100 Mixer.default_format 2 2048 with
    | Ok () ->
        ignore (Mixer.allocate_channels 32);  (* Increase to 32 channels *)
    | Error (`Msg e) -> failwith ("Failed to open audio: " ^ e)
    end
  | Error (`Msg e) -> failwith ("Failed to initialize SDL2_mixer: " ^ e)

(**
  [play_music file] plays the music file.
  @param file The path to the music file.
  @raise Failure if loading or playing the music fails.
*)
let play_music (file : string) : unit =
  match Mixer.load_mus file with
  | Ok music ->
      begin match Mixer.play_music music (-1) with
      | Ok _ -> ()
      | Error (`Msg e) -> failwith ("Failed to play music: " ^ e)
      end
  | Error (`Msg e) -> failwith ("Failed to load music: " ^ e)

(**
  [play_sound file] plays the sound file.
  @param file The path to the sound file.
  @raise Failure if loading or playing the sound fails.
*)
let play_sound (file : string) : unit =
  match Mixer.load_wav file with
  | Ok chunk ->
      begin match Mixer.play_channel (-1) chunk 0 with
      | Ok _ -> ()
      | Error (`Msg e) -> failwith ("Failed to play sound: " ^ e)
      end
  | Error (`Msg e) -> failwith ("Failed to load sound: " ^ e)

(**
  [stop_music ()] stops the currently playing music.
  @raise Failure if stopping the music fails.
*)
let stop_music () : unit =
  match Mixer.halt_music () with
  | Ok () -> ()
  | Error (`Msg e) -> failwith ("Failed to stop music: " ^ e)