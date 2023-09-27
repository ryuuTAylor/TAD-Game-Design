open Adventure

type t = {
  init_room : string;
  current_room_id : string;
  visited : string list;
}

let init_state adv =
  {
    init_room = start_room adv;
    current_room_id = start_room adv;
    visited = [ start_room adv ];
  }

let current_room_id st = st.current_room_id
let visited st = List.sort_uniq compare st.visited

type result =
  | Legal of t
  | Illegal

let go ex adv st =
  try
    let next_rm = next_room adv st.current_room_id ex in
    Legal
      {
        init_room = st.init_room;
        current_room_id = next_rm;
        visited = List.sort_uniq compare (visited st @ [ next_rm ]);
      }
  with UnknownExit ex -> Illegal
