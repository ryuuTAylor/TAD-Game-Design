(* Note: You may introduce new code anywhere in this file. *)

type object_phrase = string list

type command =
  | Go of object_phrase
  | Quit

exception Empty
exception Malformed

(* Helper method that helps parse a string roughtly *)
let parse_helper str = String.split_on_char ' ' str

(* Helper method that helps proon empty strings from a string list *)
let proon str_lst = List.filter (fun x -> not (x = "")) str_lst

let parse str =
  let prooned_str_lst = str |> parse_helper |> proon in
  match prooned_str_lst with
  | [] -> raise Empty
  | [ "quit" ] -> Quit
  | [ "go" ] -> raise Malformed
  | "go" :: obj_phrase -> Go obj_phrase
  | _ -> raise Malformed
