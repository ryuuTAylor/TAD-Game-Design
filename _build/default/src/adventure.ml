open Yojson.Basic.Util

exception UnknownRoom of string
exception UnknownExit of string

type exit = {
  name : string;
  room_id : string;
}

type rooms = {
  id : string;
  description : string;
  exits : exit list;
}

type t = {
  rooms : rooms list;
  start_room : string;
}

(* Helper method that helps deal with a single exit *)
let exit_of_json j =
  {
    name = j |> member "name" |> to_string;
    room_id = j |> member "room id" |> to_string;
  }

(* Helper method that helps deal with a single room *)
let room_of_json j =
  {
    id = j |> member "id" |> to_string;
    description = j |> member "description" |> to_string;
    exits = j |> member "exits" |> to_list |> List.map exit_of_json;
  }

(* Helper method that turns a jason format adventure into an OCaml format *)
let adventure_of_json j =
  {
    rooms = j |> member "rooms" |> to_list |> List.map room_of_json;
    start_room = j |> member "start room" |> to_string;
  }

(* Helper method that helps catch parsing error for from_json *)
let parse j =
  try adventure_of_json j
  with Type_error (s, _) -> failwith ("Parsing error: " ^ s)

let from_json json = parse json
let start_room adv = adv.start_room
let room_ids adv = List.fold_left (fun acc x -> acc @ [ x.id ]) [] adv.rooms

(* Helper method that helps find a room if it is known in the adventure,
   otherwise raise UnknownRoom exception *)
let exist_room adv room =
  try List.find (fun x -> x.id = room) adv.rooms
  with Not_found -> raise (UnknownRoom room)

let description adv room = (exist_room adv room).description

let exits adv room =
  let rm = exist_room adv room in
  List.fold_left (fun acc x -> acc @ [ x.name ]) [] rm.exits

let next_room adv room ex =
  let rm = exist_room adv room in
  try (List.find (fun x -> x.name = ex) rm.exits).room_id
  with Not_found -> raise (UnknownExit ex)

let next_rooms adv room =
  let _ = exist_room adv room in
  List.sort_uniq compare
    (List.fold_left
       (fun acc x -> acc @ [ next_room adv room x ])
       [] (exits adv room))
