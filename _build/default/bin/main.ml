open Yojson.Basic.Util
open Game
open Adventure
open Command
open State

(* Helper method copied from src/main.ml*)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

(* Helper method that processes commands after the game had begun *)
let rec process_cmd command adv st =
  match parse command with
  | exception Empty ->
      let _ =
        print_string "You've entered an empty command, please try again\n\ns> "
      in
      let command = read_line () in
      process_cmd command adv st
  | Quit ->
      let _ = print_endline "Nice playing with you!" in
      exit 0
  | exception Malformed ->
      let _ =
        print_string
          "You've entered a malformed command, please try again\n\n> "
      in
      let command = read_line () in
      process_cmd command adv st
  | Go obj_phrase -> (
      let obj = List.fold_left (fun acc x -> acc ^ " " ^ x) "" obj_phrase in
      match go (String.sub obj 1 (String.length obj - 1)) adv st with
      | Legal t ->
          let _ =
            print_endline
              ("You've succeeded in moving to " ^ current_room_id t ^ "!")
          in
          let _ = print_endline (description adv (current_room_id t)) in
          let _ =
            if
              cmp_set_like_lists (room_ids adv) (visited t @ [ "Death" ]) = true
              && current_room_id t = start_room adv
            then
              print_endline
                "\n\
                 Congratulations! You have discovered all the places and have \
                 returned home successfully! Now you can graduate from Cornell \
                 with a diploma in Magic Major! You can either quit or go to \
                 live on the planet that you like best!"
          in
          let _ = print_string "\n> " in
          let command = read_line () in
          process_cmd command adv t
      | Illegal ->
          let _ =
            print_endline
              ("Either this place does not exist or you cannot access it from \
                your current location.\n\
                You are still at " ^ current_room_id st)
          in
          let _ = print_string "Please try again.\n\n> " in
          let command = read_line () in
          process_cmd command adv st)

(** [play_game f] starts the adventure in file [f]. *)
let rec play_game f =
  if f = "data" ^ Filename.dir_sep ^ "quit" ^ ".json" then
    let _ = print_endline "Nice playing with you!" in
    exit 0
  else if not (Sys.file_exists f) then
    let _ =
      print_string "You've entered an invalid file name, please try again\n\n> "
    in
    match read_line () with
    | exception End_of_file -> ()
    | file_name -> play_game ("data" ^ Filename.dir_sep ^ file_name ^ ".json")
  else
    let adv = from_json (Yojson.Basic.from_file f) in
    let _ = print_endline (description adv (start_room adv)) in
    let _ = print_string "\n> " in
    let command = read_line () in
    process_cmd command adv (init_state adv)

let data_dir_prefix = "data" ^ Filename.dir_sep

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the 3110 Text Adventure Game engine.\n";
  print_endline "Please enter the name of the game file you want to load.\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game (data_dir_prefix ^ file_name ^ ".json")

(* Execute the game engine. *)
let () = main ()
