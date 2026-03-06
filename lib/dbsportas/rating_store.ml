open Core

let runner_hash ~id ~course = sprintf "%d__%s" id course

module History = struct
  type t = {
    position : int list;
    rating : float list;
    rating_diff : float list;
    rd : float list;
    rd_diff : float list;
    vol : float list;
    vol_diff : float list;
  }
  [@@deriving show, fields]

  let empty () =
    {
      position = [];
      rating = [];
      rating_diff = [];
      rd = [];
      rd_diff = [];
      vol = [];
      vol_diff = [];
    }

  let add t ~position ~rating ~rd ~vol =
    let rating_diff =
      match List.last t.rating with
      | None -> 0.
      | Some prev_rating -> rating -. prev_rating
    in

    let rd_diff =
      match List.last t.rd with None -> 0. | Some prev_rd -> rd -. prev_rd
    in

    let vol_diff =
      match List.last t.vol with None -> 0. | Some prev_vol -> vol -. prev_vol
    in

    {
      position = t.position @ [ position ];
      rating = t.rating @ [ rating ];
      rating_diff = t.rating_diff @ [ rating_diff ];
      rd = t.rd @ [ rd ];
      rd_diff = t.rd_diff @ [ rd_diff ];
      vol = t.vol @ [ vol ];
      vol_diff = t.vol_diff @ [ vol_diff ];
    }
end

module Info = struct
  type t = {
    id : int;
    course : string;
    rating : float;
    rd : float;
    vol : float;
    history : History.t;
  }
  [@@deriving show, fields]

  let update t ~position ~rating ~rd ~vol =
    let history = History.add t.history ~position ~rating ~rd ~vol in
    { t with history; rating; rd; vol }
end

type t = { map : Info.t String.Map.t; settings : Glicko2.Settings.t }

let create ~(settings : Glicko2.Settings.t) =
  { map = String.Map.empty; settings }

let add_if_not_exist t ~id ~course =
  let hash = runner_hash ~id ~course in
  let map =
    match Map.find t.map hash with
    | None ->
        let runner_info =
          Info.Fields.create ~id ~course ~rating:t.settings.initial_rating
            ~rd:t.settings.rd ~vol:t.settings.vol ~history:(History.empty ())
        in
        Map.add_exn t.map ~key:hash ~data:runner_info
    | Some _ -> t.map
  in

  { t with map }

let info t ~id ~course =
  let hash = runner_hash ~id ~course in
  Map.find t.map hash

let update_info t ~id ~course ~position ~rating ~rd ~vol =
  let hash = runner_hash ~id ~course in
  let map =
    Map.update t.map hash ~f:(fun info ->
        match info with
        | None -> assert false
        | Some info -> Info.update info ~position ~rating ~rd ~vol)
  in
  { t with map }

(* TODO:Steps to do 
  1. Create Rating_store.t with Glicko2 settings 
  2. For every league event:
    2.1 For each course of the vent:
      2.1.1 Add (add_if_not_exist) each participant from the event course to the Rating_store.t (for each course)
      2.1.2 Create a new Glicko2.Ranking.t with the settings from Rating_store.t 
      2.1.3 Add each participant from the event course to the Glicko2.Ranking.t
      2.1.4 Add event course results to the Glicko2.Ranking.t 
      2.1.5 Calculate new ratings for results 
        2.1.5.1 Limit how much rating a DSQ looses
      2.1.6 Get all players from the Glicko2.Ranking.t and call Rating_store.update_info for each player 
*)
