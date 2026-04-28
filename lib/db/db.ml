open Core
open Db_ops
module DB = DbOps (Turso)

type t = Turso.conn [@@deriving show { with_path = false }]

let make ?(debug = false) ~hostname ~token () : Turso.conn =
  (* TODO: check if db exists and if not, create it *)
  (* TODO: if you want to make use of the baton you have to first execute the
     following req "sql": "BEGIN"  *)
  {
    hostname;
    token;
    log_name = "db_logs.txt";
    debug;
    baton = None;
    statements = [];
  }

let log_db_conn (t : t) =
  if t.debug then
    Turso.log_conn t (sprintf "\n\n\t>>>NEW CONN (%s) <<<\n\n" t.hostname)
  else ()

let to_int64_option (i : int option) =
  match i with None -> None | Some value -> Some (Int64.of_int value)

let to_int_option (i : Int64.t option) =
  match i with None -> None | Some value -> Some (Int64.to_int_exn value)

let create_tables (handle : Turso.conn) =
  (* NOTE: DO NOT FORGET TO BASE64 ENCODE EVERY LINK *)
  let _ = DB.create_leagues handle in
  let _ = DB.create_league_events handle in
  let _ = DB.create_event_details handle in
  let _ = DB.create_event_map_links handle in
  let _ = DB.create_event_maps handle in
  let _ = DB.create_event_stats handle in
  let _ = DB.create_courses handle in
  let _ = DB.create_course_stats handle in
  let _ = DB.create_age_groups handle in
  let _ = DB.create_results handle in
  let _ = DB.create_result_stats handle in
  let _ = DB.create_splits handle in
  let _ = DB.create_runners handle in
  let _ = DB.create_ratings handle in
  let _ = DB.create_medals handle in
  ignore (Turso.send_buffered handle)

(* NOTE: this is not needed for turso connection *)
let close _ = Ok ()

(** NOTE: the EventInfo objects returned from here will be missing some info
    because we don't store results url & other data into the db *)
let event_details_for_year (handle : Turso.conn) (year : string) :
    Vilpage.Events.EventInfo.t list =
  let event_details = ref [] in
  ignore
    (DB.event_details_for_year handle ~year
       (fun ~id ~event_link ~event_date ~location ~thumbnail ~map_info ~links ->
         let _ = id in
         let map_links =
           links |> String.split ~on:',' |> List.map ~f:Utils.of_base64
         in
         let event =
           Vilpage.Events.EventInfo.Fields.create
             ~date:(Time_ns_unix.of_string event_date)
             ~thumbnail ~thumbnail_src:"" ~event_link ~location ~map_info
             ~map_links ~map_settings:None ~result_link:None
         in
         event_details := event :: !event_details));
  !event_details

(** NOTE: does not commit *)
let _add_event_links (handle : Turso.conn) (event_date : Time_ns.t)
    (links : string list) =
  let links = links |> List.map ~f:Utils.to_base64 |> String.concat ~sep:"," in
  ignore
    (DB.add_event_map_link handle ~id:None
       ~event_date:(Utils.format_time_as_date event_date)
       ~links)

(** NOTE: does not commit *)
let _add_event_details (handle : Turso.conn)
    (details : Vilpage.Events.EventInfo.t) =
  ignore
    (DB.add_event_details handle ~id:None
       ~event_link:(Utils.to_base64 details.event_link)
       ~event_date:(Utils.format_time_as_date details.date)
       ~location:details.location ~thumbnail:details.thumbnail
       ~map_info:details.map_info)

(** If year is provided it will just unpack it. If None is provided it will
    return the current year. *)
let year_from_opt (year : string option) =
  match year with
  | Some y -> y
  | None ->
      let now = Time_ns_unix.now () in
      Time_ns_unix.format ~zone:Timezone.utc now "%Y"

(** Update missing event details and map links for a particular year.

    @param year: Format: '2013'. If not provided, uses the current year*)
let action_refresh_event_details ?(year : string option = None)
    (handle : Turso.conn) =
  let year = year_from_opt year in
  let existing = event_details_for_year handle year in
  let new_events = Vilpage.Events.download_events ~year in
  (* TODO: should we insert the map images to the db here ? *)
  List.iter new_events ~f:(fun ev ->
      match List.find existing ~f:(fun v -> Time_ns.(ev.date = v.date)) with
      | None -> _add_event_details handle ev
      | Some v ->
          if List.length v.map_links = 0 && List.length ev.map_links > 0 then
            _add_event_links handle ev.date ev.map_links
          else ());
  ignore (Turso.send_buffered handle)

let leagues_aux ~f (handle : Turso.conn) =
  let leagues = ref [] in
  ignore
    (f handle (fun ~id ~league_year ~name ->
         let league =
           Dbsportas.League.LeagueInfo.Fields.create ~id:(Int64.to_int_exn id)
             ~year:(Int64.to_int_exn league_year)
             ~name
         in
         leagues := league :: !leagues));
  !leagues

(** NOTE: does not commit *)
let _add_league (handle : Turso.conn) (league : Dbsportas.League.LeagueInfo.t) =
  ignore
    (DB.add_league handle ~id:(Int64.of_int league.id)
       ~league_year:(Int64.of_int league.year) ~name:league.name)

let all_leagues (handle : Turso.conn) : Dbsportas.League.LeagueInfo.t list =
  leagues_aux handle ~f:DB.all_leagues

let leagues_for_year (handle : Turso.conn) (year : string option) =
  let year = year_from_opt year in
  leagues_aux handle ~f:(DB.leagues_for_year ~year:(Int64.of_string year))

(** NOTE: does not commit *)
let _add_league_event (handle : Turso.conn) (league_id : int)
    (event : Dbsportas.League.LeagueEvent.t) =
  DB.add_league_event handle ~id:None
    ~league_id:(Int64.of_int_exn league_id)
    ~event_nr:(Int64.of_int_exn event.nr)
    ~event_date:(Utils.format_time_as_date event.date)
    ~location:event.location

(* NOTE: use this when a new league is available *)
let add_leagues_if_not_exists (handle : Turso.conn)
    (leagues : Dbsportas.League.LeagueInfo.t list) =
  let existing = all_leagues handle in
  List.iter leagues ~f:(fun new_league ->
      match List.find existing ~f:(fun v -> Int.(v.id = new_league.id)) with
      | None ->
          _add_league handle new_league;
          let league_data =
            Dbsportas.League.download_league_info ~with_results:false
              ~league_id:(Int.to_string new_league.id)
              ()
          in
          ignore
            (List.map league_data.events
               ~f:(_add_league_event handle new_league.id))
      | Some _ -> ());
  ignore (Turso.send_buffered handle)

let ratings_aux ~f (handle : Turso.conn) =
  let ratings = ref [] in
  ignore
    (f handle
       (fun
         ~id
         ~league_id
         ~event_nr
         ~event_date
         ~course_id
         ~runner_id
         ~rating
         ~rating_diff
         ~rd
         ~vol
       ->
         let _ = id in
         let rating =
           Glicko2.Rating.Info.Fields.create
             ~league_id:(Int64.to_int_exn league_id)
             ~event_nr:(Int64.to_int_exn event_nr)
             ~event_date ~course_id
             ~runner_id:(Int64.to_int_exn runner_id)
             ~rating ~rating_diff ~rd ~vol
         in
         ratings := rating :: !ratings));
  !ratings

let ratings_for_course (handle : Turso.conn) (course_id : string) =
  ratings_aux ~f:(DB.ratings_for_course ~course_id) handle

type gender = Men | Women [@@deriving show { with_path = false }]

let gender_prefix = function Men -> "V" | Women -> "M"

let ratings_for_course_by_gender (handle : Turso.conn) (course_id : string)
    (gender : gender) =
  ratings_aux
    ~f:
      (DB.ratings_for_course_by_gender ~gender:(gender_prefix gender) ~course_id)
    handle

let rating_history_for_league_and_course (handle : Turso.conn)
    (course_id : string) (runner_id : int) (league_id : int) =
  ratings_aux
    ~f:
      (DB.rating_history_for_league_and_course
         ~runner_id:(Int64.of_int_exn runner_id)
         ~league_id:(Int64.of_int_exn league_id)
         ~course_id)
    handle

let rating_history_for_course (handle : Turso.conn) (course_id : string)
    (runner_id : int) =
  ratings_aux
    ~f:
      (DB.rating_history_for_course
         ~runner_id:(Int64.of_int_exn runner_id)
         ~course_id)
    handle

(** NOTE: does not commit *)
let _add_rating (handle : Turso.conn) (rating : Glicko2.Rating.Info.t) =
  ignore
    (DB.add_rating handle ~id:None
       ~league_id:(Int64.of_int_exn rating.league_id)
       ~event_nr:(Int64.of_int_exn rating.event_nr)
       ~event_date:rating.event_date ~course_id:rating.course_id
       ~runner_id:(Int64.of_int_exn rating.runner_id)
       ~rating:rating.rating ~rating_diff:rating.rating_diff ~rd:rating.rd
       ~vol:rating.vol)

(** NOTE: does not commit *)
let _add_event_stats ~(league_id : int) (handle : Turso.conn)
    (event : Dbsportas.League.LeagueEvent.t) =
  let results = Option.value_exn event.results in
  let num_men =
    List.map results.courses ~f:(fun c -> c.stats.num_men)
    |> List.fold ~init:0 ~f:( + )
  in
  let num_women =
    List.map results.courses ~f:(fun c -> c.stats.num_women)
    |> List.fold ~init:0 ~f:( + )
  in
  ignore
    (DB.add_event_stats handle ~id:None
       ~league_id:(Int64.of_int_exn league_id)
       ~event_nr:(Int64.of_int_exn event.nr)
       ~event_date:(Utils.format_time_as_date event.date)
       ~num_men:(Int64.of_int_exn num_men)
       ~num_women:(Int64.of_int_exn num_women))

(** NOTE: does not commit *)
let _add_course ~(league_id : int) ~(event_nr : int) ~(event_date : string)
    (handle : Turso.conn) (course : Dbsportas.League.Course.t) =
  let controls =
    match course.controls with None -> "" | Some c -> String.concat ~sep:"," c
  in
  ignore
    (DB.add_course handle ~id:None
       ~league_id:(Int64.of_int_exn league_id)
       ~event_nr:(Int64.of_int_exn event_nr)
       ~event_date ~course_id:course.id ~distance:course.distance
       ~num_controls:(Int64.of_int_exn course.controls_num)
       ~controls)

(** Fetch all events which don't have results associated with them in the db.
    @param year:
      Format: '2013'. If not provided, it will try to refresh any unprocessed
      events

    @return
      List of tuples where first tuple element is the league id and the second
      is the league event (the league event doesn't have any results) *)
let unprocessed_league_events ?(year : string option = None)
    (handle : Turso.conn) : (int * Dbsportas.League.LeagueEvent.t) list =
  let fetch_fn =
    match year with
    | Some y ->
        DB.events_to_be_processed_for_year ~league_year:(Int64.of_string y)
    | None -> DB.events_to_be_processed
  in

  let events = ref [] in
  ignore
    (fetch_fn handle (fun ~id ~league_id ~event_nr ~event_date ~location ->
         let _ = id in
         let league_id = Int64.to_int_exn league_id in
         let event =
           Dbsportas.League.LeagueEvent.Fields.create
             ~nr:(Int64.to_int_exn event_nr)
             ~date:(Time_ns_unix.of_string event_date)
             ~location ~results:None
         in
         events := (league_id, event) :: !events));
  !events

(** Add full event info (including results, stats, ratings, medals, etc.)

    NOTE: does not commit *)
let _add_full_event ~(league_id : int) (handle : Turso.conn)
    (event : Dbsportas.League.LeagueEvent.t) =
  (* TODO: implement this *)
  let _ = (league_id, handle, event) in
  ()

(** Update missing events & results for a particular year.

    @param year:
      Format: '2013'. If not provided, it will try to refresh any unprocessed
      events *)
let action_refresh_events_and_results ?(year : string option = None)
    (handle : Turso.conn) =
  assert (List.length handle.statements = 0);
  let unprocessed = unprocessed_league_events ~year handle in

  let now = Time_ns_unix.now () in
  let today = Time_ns_unix.to_date ~zone:Timezone.utc now in
  let tomorrow = Date.add_days today 1 in

  (* NOTE: exclude any unprocessed events that happen in the future - there won't be results 
     for them anyways so no need to try to process them *)
  let past_events =
    List.filter unprocessed ~f:(fun (_, event) ->
        let event_date = Time_ns_unix.to_date ~zone:Timezone.utc event.date in
        Date.(event_date < tomorrow))
  in

  if List.length past_events = 0 then () (* nothing to do -> return *)
  else
    let events_to_add =
      (* group by league id *)
      List.sort_and_group past_events ~compare:(fun (id1, _) (id2, _) ->
          Int.compare id1 id2)
      (* for each league download results for 'past' events *)
      |> List.map ~f:(fun group ->
             let league_id, _ = List.hd_exn group in
             let event_nrs_to_include =
               List.map group ~f:(fun (_, ev) -> ev.nr)
             in
             Dbsportas.League.download_league_info
               ~include_events:(Some event_nrs_to_include)
               ~league_id:(Int.to_string league_id) ())
    in
    (* prepare db statements for adding event data *)
    List.iter events_to_add ~f:(fun league ->
        List.iter league.events
          ~f:(_add_full_event handle ~league_id:(Int.of_string league.id)));

    (* NOTE: we need to get ratings info before or after all data that needs to
       be committed but not inbetween *)

    (* send all statements to turso *)
    ignore (Turso.send_buffered handle)

(* TODO: add methods to get all ratings and then based on results calculate the ratings and insert new ratings to db *)

let test (handle : Turso.conn) =
  let _ = handle in
  printf "hello\n";
  ()

let%expect_test "make" =
  printf "hello";
  [%expect {| hello |}]
