open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module UserState = struct
  type t = {
    runner_id : int;
    mutable cutoff_date : string;
    mutable rating_history : Glicko2.Rating.Info.t list; [@default []]
  }
  [@@deriving yojson]

  let make ~runner_id = { runner_id; cutoff_date = ""; rating_history = [] }

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
end

module State = struct
  type t = {
    filename : string;
    mutable latest_league_events : Db.EventInfoExtra.t list; [@default []]
    mutable all_latest_ratings : Glicko2.Rating.Info.t list; [@default []]
    mutable all_league_events : Db.LeagueEvent.t list; [@default []]
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
          user_state = [];
        }

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
    let user_state =
      match List.Assoc.find t.user_state ~equal:Int.equal runner_id with
      | None ->
          let state = UserState.make ~runner_id in
          t.user_state <- (runner_id, state) :: t.user_state;
          state
      | Some user_state -> user_state
    in
    (* TODO: for deployment, disable all this saving cause it will be very slow *)
    let ratings = UserState.ratings user_state db ~since in
    save t;
    ratings
end
