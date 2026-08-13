open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module UserState = struct
  type t = {
    runner_id : int;
    mutable cutoff_date : string;
    mutable rating_history : Glicko2.Rating.Info.t list; [@default []]
    mutable simple_results : Db.Types.SimpleResult.t list; [@default []]
    mutable result_stats : Db.Types.ResultStats.t list list; [@default []]
    mutable info : Db.Types.RunnerInfo.t option; [@default None]
    mutable medals : Db.Types.Medals.t option; [@default None]
  }
  [@@deriving yojson]

  let make ~runner_id =
    {
      runner_id;
      cutoff_date = "";
      rating_history = [];
      simple_results = [];
      result_stats = [];
      info = None;
      medals = None;
    }

  let ratings (t : t) (db : Db.t) ~(since : string) =
    if String.(since = t.cutoff_date) && List.length t.rating_history > 0 then
      t.rating_history
    else
      let rating_history =
        Db.ratings_for_runner db ~runner_id:t.runner_id ~cutoff_date:since
      in
      t.rating_history <- rating_history;
      t.cutoff_date <- since;
      rating_history

  let simple_results (t : t) (db : Db.t) =
    if List.length t.simple_results > 0 then t.simple_results
    else
      let simple_results =
        Db.simple_results_for_runner db ~runner_id:t.runner_id
      in
      t.simple_results <- simple_results;
      simple_results

  let result_stats (t : t) (db : Db.t) ~(page_num : int) ~(page_size : int) =
    (* NOTE: this should only be called with consecutive page numbers, for
       example when we have the first 2 pages we should NEVER be requesting
       page 4 *)
    assert (
      List.length t.result_stats = 0 || List.length t.result_stats >= page_num);

    match List.nth t.result_stats (page_num - 1) with
    | None ->
        let result_stats =
          Db.result_stats_for_runner db ~runner_id:t.runner_id ~page_num
            ~page_size
        in
        t.result_stats <- t.result_stats @ [ result_stats ];
        result_stats
    | Some result_stats -> result_stats

  let runner_info (t : t) (db : Db.t) =
    if Option.is_some t.info then Option.value_exn t.info
    else
      let info = Db.runner_by_id db ~runner_id:t.runner_id in
      t.info <- Some info;
      info

  let medals (t : t) (db : Db.t) =
    if Option.is_some t.medals then Option.value_exn t.medals
    else
      let medals = Db.medals_for_runner db ~runner_id:t.runner_id in
      t.medals <- Some medals;
      medals
end

module State = struct
  type t = {
    filename : string;
    mutable latest_league_events : Db.Types.EventInfoExtra.t list; [@default []]
    mutable all_latest_ratings : Glicko2.Rating.Info.t list; [@default []]
    mutable all_league_events : Db.Types.LeagueEvent.t list; [@default []]
    (* this includes all leagues *)
    mutable all_events : Db.Types.LeagueEvent.t list; [@default []]
    mutable user_state : (int * UserState.t) list; [@default []]
  }
  [@@deriving yojson]

  let save (t : t) = Yojson.Safe.to_file t.filename (yojson_of_t t)

  let load (filename : string) =
    match Yojson.Safe.from_file filename with
    | t -> t_of_yojson t
    | exception exc ->
        printf
          "Failed to load cache from file (%s): %s . Initializing an empty \
           cache..."
          filename (Exn.to_string exc);
        {
          filename;
          latest_league_events = [];
          all_latest_ratings = [];
          all_league_events = [];
          all_events = [];
          user_state = [];
        }

  let _user_state (t : t) ~(runner_id : int) =
    match List.Assoc.find t.user_state ~equal:Int.equal runner_id with
    | None ->
        let state = UserState.make ~runner_id in
        t.user_state <- (runner_id, state) :: t.user_state;
        state
    | Some user_state -> user_state

  let latest_league_events (t : t) (db : Db.t) =
    if List.length t.latest_league_events > 0 then t.latest_league_events
    else (
      Dream.log "Fetching event data from DB";
      let today = Utils.today_string () in
      let events = Db.latest_league_events db today in
      t.latest_league_events <- events;
      save t;
      events)

  let all_league_events (t : t) (db : Db.t) =
    if List.length t.all_league_events > 0 then t.all_league_events
    else (
      Dream.log "Fetching all league events data from DB";
      let events = Db.all_league_events db in
      t.all_league_events <- events;
      save t;
      events)

  (* TODO: this is currently not used - DELETE *)
  let all_events (t : t) (db : Db.t) =
    if List.length t.all_events > 0 then t.all_events
    else (
      Dream.log "Fetching all events data from DB";
      let events = Db.all_events db in
      t.all_events <- events;
      save t;
      events)

  let _update_ratings_rd (t : t) (db : Db.t)
      (ratings : Glicko2.Rating.Info.t list) =
    let all_event_dates =
      all_league_events t db |> List.map ~f:(fun event -> event.event_date)
    in
    let latest_event_date =
      List.fold ratings ~init:"" ~f:(fun latest_date r ->
          if String.(r.event_date > latest_date) then r.event_date
          else latest_date)
    in
    let latest_event_idx =
      List.findi all_event_dates ~f:(fun _ date ->
          String.equal latest_event_date date)
    in

    List.map ratings ~f:(fun r ->
        match latest_event_idx with
        | None -> r
        | Some (latest_i, _) ->
            let rating_date_idx, _ =
              List.findi_exn all_event_dates ~f:(fun _ date ->
                  String.equal r.event_date date)
            in
            let diff = latest_i - rating_date_idx in
            let rd_increase =
              if diff > 0 then Float.of_int diff *. Db.rd_increase_per_event
              else 0.
            in
            { r with rd = r.rd +. rd_increase })

  let all_latest_ratings (t : t) (db : Db.t) =
    if List.length t.all_latest_ratings > 0 then t.all_latest_ratings
    else (
      Dream.log "Fetching all latest ratings data from DB";
      let ratings =
        Db.all_latest_ratings_for_last_year db |> _update_ratings_rd t db
      in

      t.all_latest_ratings <- ratings;
      save t;
      ratings)

  let ratings_for_runner (t : t) (db : Db.t) ?(since : string = "2020-01-01")
      (runner_id : int) =
    let user_state = _user_state t ~runner_id in
    (* TODO: for deployment, disable all this saving cause it will be very slow *)
    let ratings = UserState.ratings user_state db ~since in
    save t;
    ratings

  let simple_results_for_runner (t : t) (db : Db.t) (runner_id : int) =
    let user_state = _user_state t ~runner_id in
    (* TODO: for deployment, disable all this saving cause it will be very slow *)
    let results = UserState.simple_results user_state db in
    save t;
    results

  let result_stats_for_runner (t : t) (db : Db.t) (runner_id : int) ~page_num
      ~page_size =
    let user_state = _user_state t ~runner_id in
    (* TODO: for deployment, disable all this saving cause it will be very slow *)
    let results = UserState.result_stats user_state db ~page_num ~page_size in
    save t;
    results

  let runner_info (t : t) (db : Db.t) (runner_id : int) =
    let user_state = _user_state t ~runner_id in
    (* TODO: for deployment, disable all this saving cause it will be very slow *)
    let info = UserState.runner_info user_state db in
    save t;
    info

  let medals (t : t) (db : Db.t) (runner_id : int) =
    let user_state = _user_state t ~runner_id in
    (* TODO: for deployment, disable all this saving cause it will be very slow *)
    let medals = UserState.medals user_state db in
    save t;
    medals
end
