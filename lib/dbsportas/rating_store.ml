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

  let show_diff t =
    List.fold t.rating_diff ~init:"" ~f:(fun acc diff ->
        acc ^ sprintf "%.2f, " diff)
end

module Info = struct
  type t = {
    id : int;
    name : string;
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

module Store = struct
  type t = { map : Info.t String.Map.t; settings : Glicko2.Settings.t }

  let create ~(settings : Glicko2.Settings.t) =
    { map = String.Map.empty; settings }

  let show t ?(course = "1") () =
    let runners =
      Map.data t.map
      (* |> List.map ~f:(fun info -> *)
      (*        printf "Info: %s\n" (Info.show info); *)
      (*        info) *)
      |> List.filter ~f:(fun info -> String.(info.course = course))
      |> List.sort ~compare:(fun info1 info2 ->
             Float.compare info1.rating info2.rating)
    in

    let out = sprintf "Ratings for '%s' course\n" course in
    let out =
      out ^ sprintf "Settings: %s\n" (Glicko2.Settings.show t.settings)
    in
    List.foldi runners ~init:out ~f:(fun i acc info ->
        acc
        ^ sprintf "%d %.2f %s (%d) (rd=%.2f, vol=%.2f) - %s\n" i info.rating
            info.name info.id info.rd info.vol
            (History.show_diff info.history))

  let add_if_not_exist t ~id ~name ~course =
    let hash = runner_hash ~id ~course in
    let map =
      match Map.find t.map hash with
      | None ->
          let runner_info =
            Info.Fields.create ~id ~name ~course
              ~rating:t.settings.initial_rating ~rd:t.settings.rd
              ~vol:t.settings.vol ~history:(History.empty ())
          in
          Map.add_exn t.map ~key:hash ~data:runner_info
      | Some _ -> t.map
    in

    { t with map }

  let info t ~id ~course =
    let hash = runner_hash ~id ~course in
    Map.find t.map hash

  let participant t ~id ~course =
    let hash = runner_hash ~id ~course in
    match Map.find t.map hash with
    | None -> None
    | Some info ->
        Some
          (Glicko2.Race.Participant.Fields.create ~id
             ~stats:
               (Some
                  (Glicko2.Race.Stats.Fields.create ~rating:info.rating
                     ~rd:info.rd ~vol:info.vol)))

  let update_info t ~id ~course ~position ~rating ~rd ~vol =
    let hash = runner_hash ~id ~course in
    let map =
      Map.update t.map hash ~f:(fun info ->
          match info with
          | None -> assert false
          | Some info -> Info.update info ~position ~rating ~rd ~vol)
    in
    { t with map }
end

let calculate_ratings ~(settings : Glicko2.Settings.t)
    ~(league : League.League.t) : Store.t =
  let store = Store.create ~settings in

  let store =
    List.fold league.events ~init:store ~f:(fun store event ->
        match event.results with
        | None -> store
        | Some results ->
            List.fold results.courses ~init:store ~f:(fun store course ->
                let all_runners = course.results.finished in
                let store =
                  List.fold all_runners ~init:store ~f:(fun store runner ->
                      (* make sure runner exists in the store *)
                      Store.add_if_not_exist store ~id:runner.runner_nr
                        ~name:runner.name ~course:course.id)
                in

                let race =
                  List.map all_runners ~f:(fun runner ->
                      let participant =
                        Store.participant store ~id:runner.runner_nr
                          ~course:course.id
                        |> Option.value_exn
                      in
                      [ participant ])
                in
                (* printf "Participants in event: %d %s %d %s\n" (List.length race) *)
                (*   course.id event.nr event.location; *)
                let glicko2 =
                  Glicko2.Ranking.of_race ~settings ~race
                  |> Glicko2.Ranking.update_ratings
                in
                let updated_ratings = Glicko2.Ranking.players glicko2 in

                List.fold updated_ratings ~init:store ~f:(fun store ratings ->
                    let position, _ =
                      List.findi_exn all_runners ~f:(fun _ runner ->
                          runner.runner_nr = ratings.id)
                    in
                    (* convert from index to position *)
                    let position = position + 1 in
                    Store.update_info store ~id:ratings.id ~position
                      ~course:course.id ~rating:ratings.rating ~rd:ratings.rd
                      ~vol:ratings.vol)))
  in
  store

let%expect_test "calculate_ratings" =
  let league =
    Yojson.Safe.from_file
      "/home/angel/Documents/ocaml/vkd/league244_full_object.json"
    |> League.League.t_of_yojson
  in

  let settings =
    (* NOTE: these are the default settings *)
    Glicko2.Settings.create ~initial_rating:1500. ~rd:350. ~vol:0.06 ~tau:0.5 ()
  in

  let store = calculate_ratings ~settings ~league in
  (* TODO: 1. remove vol from printout
           2. Add position & location (or event_nr) to each diff number
           3. Double check if results make sense
           4. Handle disqualified runners
           *)

  printf "%s" (Store.show store ~course:"1" ());

  [%expect {||}]

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
