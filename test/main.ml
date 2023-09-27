open OUnit2
open Game
open Adventure
open Command
open State

(********************************************************************
   Here are some helper functions for your testing of set-like lists.
 ********************************************************************)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether they are
    equivalent set-like lists. That means checking two things. First, they must
    both be "set-like", meaning that they do not contain any duplicates. Second,
    they must contain the same elements, though not necessarily in the same
    order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

(* These tests demonstrate how to use [cmp_set_like_lists] and [pp_list] to get
   helpful output from OUnit. *)
let cmp_demo =
  [
    ( "order is irrelevant" >:: fun _ ->
      assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
        [ "foo"; "bar" ] [ "bar"; "foo" ] )
    (* Uncomment this test to see what happens when a test case fails.
       "duplicates not allowed" >:: (fun _ -> assert_equal
       ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string) ["foo"; "foo"]
       ["foo"]); *);
  ]

(********************************************************************
   End helper functions.
 ********************************************************************)

(* You are welcome to add strings containing JSON here, and use them as the
   basis for unit tests. You can also use the JSON files in the data directory
   as tests. And you can add JSON files in this directory and use them, too. *)

(* Here is an example of how to load files from the data directory: *)
let data_dir_prefix = "data" ^ Filename.dir_sep
let lonely = Yojson.Basic.from_file (data_dir_prefix ^ "lonely_room.json")
let ho = Yojson.Basic.from_file (data_dir_prefix ^ "ho_plaza.json")
let lonely_t = lonely |> from_json
let ho_t = ho |> from_json

(* You should not be testing any helper functions here. Test only the functions
   exposed in the [.mli] files. Do not expose your helper functions. See the
   handout for an explanation. *)

(* TODO: add unit tests for modules below. You are free to reorganize the
   definitions below. Just keep it clear which tests are for which modules. *)

let start_room_test (name : string) (adventure : Adventure.t)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal ~printer:pp_string expected_output (adventure |> start_room)

let start_room_tests =
  [
    start_room_test "lonely's start room" lonely_t "the room";
    start_room_test "ho's start room" ho_t "ho plaza";
  ]

let room_ids_test (name : string) (adventure : Adventure.t)
    (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
    expected_output (adventure |> room_ids)

let room_ids_tests =
  [
    room_ids_test "ho's room ids" ho_t
      [ "ho plaza"; "tower"; "health"; "nirvana" ];
    room_ids_test "ho's room ids" ho_t
      [ "tower"; "ho plaza"; "nirvana"; "health" ];
    room_ids_test "lonely's room ids" lonely_t [ "the room" ];
  ]

let description_test (name : string) (adventure : Adventure.t)
    (identifier : string) (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal ~printer:pp_string expected_output
    (description adventure identifier)

let description_test' (name : string) (adventure : Adventure.t)
    (identifier : string) (expected_output : exn) : test =
  name >:: fun _ ->
  assert_raises expected_output (fun () -> description adventure identifier)

let description_tests =
  [
    description_test "lonely's description of the room" lonely_t "the room"
      "A very lonely room.";
    description_test "ho's description of tower" ho_t "tower"
      "You climbed up all 161 steps to the top of McGraw Tower. A Chimesmaster \
       is playing the Jennie McGraw Rag. You feel inspired to ascend higher.";
    description_test' "lonely raises UnknownRoom kitchen" lonely_t "kitchen"
      (UnknownRoom "kitchen");
    description_test' "ho raises UnknownRoom statler" ho_t "statler"
      (UnknownRoom "statler");
  ]

let exits_test (name : string) (adventure : Adventure.t) (identifier : string)
    (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
    expected_output
    (exits adventure identifier)

let exits_test' (name : string) (adventure : Adventure.t) (identifier : string)
    (expected_output : exn) : test =
  name >:: fun _ ->
  assert_raises expected_output (fun () -> exits adventure identifier)

let exits_tests =
  [
    exits_test "lonely's the room has no exit" lonely_t "the room" [];
    exits_test "ho's health has exits northeast, north east, and Ho Plaza" ho_t
      "health"
      [ "northeast"; "north east"; "Ho Plaza" ];
    exits_test' "lonely raises UnknownRoom kitchen" lonely_t "kitchen"
      (UnknownRoom "kitchen");
  ]

let next_room_test (name : string) (adventure : Adventure.t)
    (identifier : string) (exit : string) (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal ~printer:pp_string expected_output
    (next_room adventure identifier exit)

let next_room_test' (name : string) (adventure : Adventure.t)
    (identifier : string) (exit : string) (expected_output : exn) : test =
  name >:: fun _ ->
  assert_raises expected_output (fun () -> next_room adventure identifier exit)

let next_room_tests =
  [
    next_room_test "ho's health northeast has next room ho plaza" ho_t "health"
      "northeast" "ho plaza";
    next_room_test' "ho's health raises UnknownExit northwest" ho_t "health"
      "northwest" (UnknownExit "northwest");
    next_room_test' "ho raises UnknownRoom statler" ho_t "statler" "northeast"
      (UnknownRoom "statler");
  ]

let next_rooms_test (name : string) (adventure : Adventure.t)
    (identifier : string) (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
    expected_output
    (next_rooms adventure identifier)

let next_rooms_tests =
  [
    next_rooms_test "ho's ho plaza has next rooms health and tower" ho_t
      "ho plaza" [ "health"; "tower" ];
  ]

let adventure_tests =
  List.flatten
    [
      start_room_tests;
      room_ids_tests;
      description_tests;
      exits_tests;
      next_room_tests;
      next_rooms_tests;
    ]

let parse_test (name : string) (command : string) (expected_output : command) :
    test =
  name >:: fun _ -> assert_equal expected_output (parse command)

let parse_test' (name : string) (command : string) (expected_output : exn) :
    test =
  name >:: fun _ -> assert_raises expected_output (fun () -> parse command)

let parse_tests =
  [
    parse_test "command.mli example 1" "    go   clock   tower   "
      (Go [ "clock"; "tower" ]);
    parse_test "command.mli example 1 prime" "go clock tower"
      (Go [ "clock"; "tower" ]);
    parse_test "command.mli example 2" "quit" Quit;
    parse_test' "empty command raises empty" " " Empty;
    parse_test' "misspelled verb command raises malformed" "Go" Malformed;
    parse_test' "wrong verb command raises malformed" "kill" Malformed;
    parse_test' "quit with non-empty object phrase command raises malformed"
      "quit coding" Malformed;
    parse_test' "go with empty object phrase command raises malformed" "go"
      Malformed;
  ]

let command_tests = List.flatten [ parse_tests ]

let go_test_illegal (name : string) (exit : string) (adventure : Adventure.t)
    (state : State.t) (expected_result : result) : test =
  name >:: fun _ -> assert_equal expected_result (go exit adventure state)

let go_current_room_id_test (name : string) (exit : string)
    (adventure : Adventure.t) (state : State.t) (expected_result : string) :
    test =
  name >:: fun _ ->
  match go exit adventure state with
  | Legal t ->
      assert_equal ~printer:pp_string expected_result (current_room_id t)
  | Illegal ->
      assert_equal ~printer:pp_string expected_result (current_room_id state)

let go_visited_test (name : string) (exit : string) (adventure : Adventure.t)
    (state : State.t) (expected_result : string list) : test =
  name >:: fun _ ->
  match go exit adventure state with
  | Legal t ->
      assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
        expected_result (visited t)
  | Illegal ->
      assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
        expected_result (visited state)

let go_tests =
  [
    go_test_illegal "inital state of ho exits from east is illegal" "east" ho_t
      (init_state ho_t) Illegal;
    go_current_room_id_test "initial state of ho exits from southwest is legal"
      "southwest" ho_t (init_state ho_t) "health";
    go_visited_test "initial state of ho exits from southwest is legal"
      "southwest" ho_t (init_state ho_t) [ "ho plaza"; "health" ];
    go_current_room_id_test "initial state of ho exits from east is illegal"
      "east" ho_t (init_state ho_t) "ho plaza";
    go_visited_test "initial state of ho exits from east is illegal" "east" ho_t
      (init_state ho_t) [ "ho plaza" ];
  ]

let state_tests = List.flatten [ go_tests ]

let suite =
  "test suite for A2"
  >::: List.flatten [ cmp_demo; adventure_tests; command_tests; state_tests ]

let _ = run_test_tt_main suite
