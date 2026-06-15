open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Core
open Db_ops
open Custom_db_ops
module DB = DbOps (Turso)
module CustomDb = CustomDbOps (Turso)

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

let to_int64 (i : int) = Int64.of_int i
let of_int64 (i : Int64.t) = Int64.to_int_exn i

let to_int64_option (i : int option) =
  match i with None -> None | Some value -> Some (to_int64 value)

let to_int_option (i : Int64.t option) =
  match i with None -> None | Some value -> Some (of_int64 value)

module EventParams = struct
  type t = { league_id : int64; event_nr : int64; event_date : string }

  let create ~(league_id : string) ~(event_nr : int) ~(event_date : Time_ns.t) =
    let league_id = Int64.of_string league_id in
    let event_nr = to_int64 event_nr in
    let event_date = Utils.format_time_as_date event_date in
    { league_id; event_nr; event_date }
end

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

  (* indexes *)
  let _ = DB.create_league_events_date_idx handle in
  let _ = DB.create_event_details_date_idx handle in
  let _ = DB.create_event_map_links_date_idx handle in
  let _ = DB.create_event_maps_date_idx handle in
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
           match links with
           | None -> []
           | Some links ->
               links |> String.split ~on:',' |> List.map ~f:Utils.of_base64
         in
         (* TODO: modify query to return the map settings and result links etc *)
         let event =
           Vilpage.Events.EventInfo.Fields.create
             ~date:(Utils.time_of_date event_date)
             ~thumbnail ~thumbnail_src:"" ~event_link ~location ~map_info
             ~map_links ~map_settings:None ~result_link:None
         in
         event_details := event :: !event_details));
  List.rev !event_details

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

(** NOTE: does not commit *)
let _add_event_map (handle : Turso.conn)
    (map_info : Vilpage.Events.MapSettings.t option) ~(event_date : string) =
  match map_info with
  | None -> ()
  | Some map_info ->
      ignore
        (DB.add_event_map handle ~id:None ~event_date ~map_key:map_info.key
           ~title:map_info.title ~lat:map_info.location_lat
           ~lon:map_info.location_lon ~default_map_img:map_info.default_map
           ~bike_map_img:map_info.bike_map)

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

  List.iter new_events ~f:(fun ev ->
      match List.find existing ~f:(fun v -> Time_ns.(ev.date = v.date)) with
      | None -> _add_event_details handle ev
      | Some v ->
          if List.length v.map_links = 0 && List.length ev.map_links > 0 then (
            _add_event_links handle ev.date ev.map_links;
            _add_event_map handle ev.map_settings
              ~event_date:(Utils.format_time_as_date ev.date))
          else ());
  ignore (Turso.send_buffered handle)

let leagues_aux ~f (handle : Turso.conn) =
  let leagues = ref [] in
  ignore
    (f handle (fun ~id ~league_year ~name ->
         let league =
           Dbsportas.League.LeagueInfo.Fields.create ~id:(of_int64 id)
             ~year:(of_int64 league_year) ~name
         in
         leagues := league :: !leagues));
  List.rev !leagues

(** NOTE: does not commit *)
let _add_league (handle : Turso.conn) (league : Dbsportas.League.LeagueInfo.t) =
  ignore
    (DB.add_league handle ~id:(to_int64 league.id)
       ~league_year:(to_int64 league.year) ~name:league.name)

let all_leagues (handle : Turso.conn) : Dbsportas.League.LeagueInfo.t list =
  leagues_aux handle ~f:DB.all_leagues

let leagues_for_year (handle : Turso.conn) (year : string option) =
  let year = year_from_opt year in
  leagues_aux handle ~f:(DB.leagues_for_year ~year:(Int64.of_string year))

let leagues_for_name (handle : Turso.conn) (league_name : string) =
  leagues_aux handle ~f:(DB.leagues_for_name ~league_name)

(** NOTE: does not commit *)
let _add_league_event (handle : Turso.conn) (league_id : int)
    (event : Dbsportas.League.LeagueEvent.t) =
  DB.add_league_event handle ~id:None ~league_id:(to_int64 league_id)
    ~event_nr:(to_int64 event.nr)
    ~event_date:(Utils.format_time_as_date event.date)
    ~location:event.location

module EventInfoExtra = struct
  type t = {
    event_id : int;
    league_id : int;
    league_name : string;
    event_nr : int;
    event_date : string;
    (* This is the preiliminary location name *)
    location : string;
    event_link : string option;
    thumbnail : string option; [@opaque]
    map_info : string option;
    (* This is the real location displayed in the results *)
    official_location : string option;
    links : string list;
  }
  [@@deriving show, fields, yojson]

  let t_of_db_row ~id ~league_id ~event_nr ~event_date ~location ~league_name
      ~event_link ~thumbnail ~map_info ~official_location ~links : t =
    let links =
      match links with
      | None -> []
      | Some links ->
          links |> String.split ~on:',' |> List.map ~f:Utils.of_base64
    in
    let event_link = Option.map event_link ~f:Utils.of_base64 in
    {
      event_id = Int64.to_int_exn id;
      league_id = Int64.to_int_exn league_id;
      league_name;
      event_nr = Int64.to_int_exn event_nr;
      event_date;
      location;
      event_link;
      thumbnail;
      map_info;
      official_location;
      links;
    }
end

let latest_league_events (handle : Turso.conn) (date : string) =
  let results = ref [] in
  DB.league_event_neighbors handle ~input_date:date
    (fun
      ~id
      ~league_id
      ~event_nr
      ~event_date
      ~location
      ~league_name
      ~event_link
      ~thumbnail
      ~map_info
      ~official_location
      ~links
    ->
      results :=
        EventInfoExtra.t_of_db_row ~id ~league_id ~event_nr ~event_date
          ~location ~league_name ~event_link ~thumbnail ~map_info
          ~official_location ~links
        :: !results);
  List.sort (* sort from latest to oldest *)
    ~compare:(fun e1 e2 -> String.compare e2.event_date e1.event_date)
    !results

(* TODO: make sure to add leagues first and then start processing events *)
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
          Utils.sleep ~s:1;
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
         ~runner_name
         ~runner_club
         ~runner_gender
       ->
         let _ = id in
         let rating =
           Glicko2.Rating.Info.Fields.create ~league_id:(of_int64 league_id)
             ~event_nr:(of_int64 event_nr) ~event_date ~course_id
             ~runner_id:(of_int64 runner_id) ~rating ~rating_diff ~rd ~vol
             ~runner_name ~runner_club ~runner_gender
         in

         ratings := rating :: !ratings));
  (* NOTE: this preserves the order from the query *)
  List.rev !ratings

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
      (DB.rating_history_for_league_and_course ~runner_id:(to_int64 runner_id)
         ~league_id:(to_int64 league_id) ~course_id)
    handle

let rating_history_for_course (handle : Turso.conn) (course_id : string)
    (runner_id : int) =
  ratings_aux
    ~f:(DB.rating_history_for_course ~runner_id:(to_int64 runner_id) ~course_id)
    handle

(** NOTE: does not commit *)
let _add_rating (handle : Turso.conn) (rating : Glicko2.Rating.Info.t) =
  ignore
    (DB.add_rating handle ~id:None
       ~league_id:(to_int64 rating.league_id)
       ~event_nr:(to_int64 rating.event_nr) ~event_date:rating.event_date
       ~course_id:rating.course_id
       ~runner_id:(to_int64 rating.runner_id)
       ~rating:rating.rating ~rating_diff:rating.rating_diff ~rd:rating.rd
       ~vol:rating.vol)

(** NOTE: does not commit *)
let _add_event_stats ~(event_params : EventParams.t) (handle : Turso.conn)
    (results : Dbsportas.League.EventResults.t) =
  let num_men =
    List.map results.courses ~f:(fun c -> c.stats.num_men)
    |> List.fold ~init:0 ~f:( + )
  in
  let num_women =
    List.map results.courses ~f:(fun c -> c.stats.num_women)
    |> List.fold ~init:0 ~f:( + )
  in
  ignore
    (DB.add_event_stats handle ~id:None ~league_id:event_params.league_id
       ~event_nr:event_params.event_nr ~event_date:event_params.event_date
       ~num_men:(to_int64 num_men) ~num_women:(to_int64 num_women))

(** NOTE: does not commit *)
let _add_course ~(event_params : EventParams.t) (handle : Turso.conn)
    (course : Dbsportas.League.Course.t) =
  let controls =
    match course.controls with None -> "" | Some c -> String.concat ~sep:"," c
  in
  ignore
    (DB.add_course handle ~id:None ~league_id:event_params.league_id
       ~event_nr:event_params.event_nr ~event_date:event_params.event_date
       ~course_id:course.id ~distance:course.distance
       ~num_controls:(to_int64 course.controls_num)
       ~controls)

(** NOTE: does not commit *)
let _add_course_stats ~(event_params : EventParams.t) (handle : Turso.conn)
    (course : Dbsportas.League.Course.t) =
  let s = course.stats in
  let course_id = course.id in
  ignore
    (DB.add_course_stats handle ~id:None ~league_id:event_params.league_id
       ~event_nr:event_params.event_nr ~event_date:event_params.event_date
       ~course_id ~num_men:(to_int64 s.num_men)
       ~num_women:(to_int64 s.num_women) ~tilt_overall:(to_int64 s.tilt_overall)
       ~tilt_men:(to_int64 s.tilt_men) ~tilt_women:(to_int64 s.tilt_women)
       ~mistake_time_overall:(to_int64 s.mistake_time_overall)
       ~mistake_time_men:(to_int64 s.mistake_time_men)
       ~mistake_time_women:(to_int64 s.mistake_time_women)
       ~blunder_perc_overall:(to_int64 s.blunder_perc_overall)
       ~blunder_perc_men:(to_int64 s.blunder_perc_men)
       ~blunder_perc_women:(to_int64 s.blunder_perc_women)
       ~big_mistake_perc_overall:(to_int64 s.big_mistake_perc_overall)
       ~big_mistake_perc_men:(to_int64 s.big_mistake_perc_men)
       ~big_mistake_perc_women:(to_int64 s.big_mistake_perc_women)
       ~small_mistake_perc_overall:(to_int64 s.small_mistake_perc_overall)
       ~small_mistake_perc_men:(to_int64 s.small_mistake_perc_men)
       ~small_mistake_perc_women:(to_int64 s.small_mistake_perc_women)
       ~most_tricky_overall:(to_int64_option s.most_tricky_overall)
       ~most_tricky_men:(to_int64_option s.most_tricky_men)
       ~most_tricky_women:(to_int64_option s.most_tricky_women)
       ~avg_time_for_mistake_overall:(to_int64 s.avg_time_for_mistake_overall)
       ~avg_time_for_mistake_men:(to_int64 s.avg_time_for_mistake_men)
       ~avg_time_for_mistake_women:(to_int64 s.avg_time_for_mistake_women)
       ~avg_mistake_num_overall:(to_int64 s.avg_mistake_num_overall)
       ~avg_mistake_num_men:(to_int64 s.avg_mistake_num_men)
       ~avg_mistake_num_women:(to_int64 s.avg_mistake_num_women))

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
         let league_id = of_int64 league_id in
         let event =
           Dbsportas.League.LeagueEvent.Fields.create ~nr:(of_int64 event_nr)
             ~date:(Utils.time_of_date event_date)
             ~location ~results:None
         in
         events := (league_id, event) :: !events));
  List.rev !events

(** NOTE: does not commit *)
let _add_result_stats (handle : Turso.conn) ~(event_params : EventParams.t)
    ~(course_id : string) ~(runner_id : int64)
    (stats : Dbsportas.Runner_stats.t option) =
  match stats with
  | None -> ()
  | Some s ->
      ignore
        (DB.add_result_stats handle ~id:None ~league_id:event_params.league_id
           ~event_nr:event_params.event_nr ~event_date:event_params.event_date
           ~course_id ~runner_id ~mistake_time:(to_int64 s.mistake_time)
           ~mistake_num:(to_int64 s.mistake_num)
           ~small_mistake_time:(to_int64 s.small_mistakes.time)
           ~small_mistake_num:(to_int64 s.small_mistakes.num)
           ~small_mistake_time_ratio:(to_int64 s.small_mistakes.time_ratio)
           ~small_mistake_num_ratio:(to_int64 s.small_mistakes.num_ratio)
           ~big_mistake_time:(to_int64 s.big_mistakes.time)
           ~big_mistake_num:(to_int64 s.big_mistakes.num)
           ~big_mistake_time_ratio:(to_int64 s.big_mistakes.time_ratio)
           ~big_mistake_num_ratio:(to_int64 s.big_mistakes.num_ratio)
           ~blunder_mistake_time:(to_int64 s.blunder_mistakes.time)
           ~blunder_mistake_num:(to_int64 s.blunder_mistakes.num)
           ~blunder_mistake_time_ratio:(to_int64 s.blunder_mistakes.time_ratio)
           ~blunder_mistake_num_ratio:(to_int64 s.blunder_mistakes.num_ratio)
           ~consecutive_mistakes:(to_int64 s.consecutive_mistakes)
           ~tilt_rate:(to_int64 s.tilt_rate)
           ~mistake_cluster:
             (Option.bind s.mistake_cluster ~f:(fun v ->
                  Some (Dbsportas.Runner_stats.show_mistakeCluster v)))
           ~mistakes_impact:
             (Option.bind s.mistakes_impact ~f:(fun v ->
                  Some (Dbsportas.Runner_stats.show_mistakesImpact v)))
           ~race_execution:
             (Option.bind s.race_execution ~f:(fun v ->
                  Some (Dbsportas.Runner_stats.show_raceExecution v)))
           ~best_splits:(to_int64 s.best_splits)
           ~top5_splits:(to_int64 s.top5_splits)
           ~top10_splits:(to_int64 s.top10_splits)
           ~performance:(to_int64 s.performance)
           ~overall_position:(to_int64_option s.overall_position)
           ~position_gender:(to_int64_option s.position_gender)
           ~position_group:(to_int64_option s.position_group)
           ~potential_time:(to_int64_option s.potential_time)
           ~potential_position:(to_int64_option s.potential_position))

(** NOTE: does not commit *)
let _add_split (handle : Turso.conn) ~(event_params : EventParams.t)
    ~(course_id : string) ~(runner_id : int64) (split : Dbsportas.Split.t)
    (split_idx : int64) =
  ignore
    (DB.add_splits handle ~id:None ~league_id:event_params.league_id
       ~event_nr:event_params.event_nr ~event_date:event_params.event_date
       ~course_id ~runner_id ~split_idx
       ~time_sec:(to_int64_option split.time)
       ~position:(to_int64_option split.position)
       ~overall_time:(to_int64_option split.overall_time)
       ~overall_position:(to_int64_option split.overall_position)
       ~split_timestamp:(to_int64_option split.timestamp)
       ~mistake_time:(to_int64_option split.mistake_time))

(** NOTE: does not commit *)
let _add_splits (handle : Turso.conn) ~(event_params : EventParams.t)
    ~(course_id : string) ~(runner_id : int64)
    (splits : Dbsportas.Splits.t option) =
  match splits with
  | None -> ()
  | Some splits ->
      List.iteri splits ~f:(fun idx split ->
          _add_split handle ~event_params ~course_id ~runner_id split
            (to_int64 idx))

(** NOTE: does not commit *)
let _add_result (handle : Turso.conn) ~(event_params : EventParams.t)
    ~(course_id : string) (result : Dbsportas.League.OverallResult.t) =
  let dsq = (match result.status with Dsq -> 1 | _ -> 0) |> to_int64 in
  ignore
    (DB.add_result handle ~id:None ~league_id:event_params.league_id
       ~event_nr:event_params.event_nr ~event_date:event_params.event_date
       ~course_id
       ~runner_id:(to_int64 result.runner_nr)
       ~time_sec:(to_int64_option result.time)
       ~start_time:(to_int64_option result.start)
       ~points:(to_int64 result.points) ~pace:result.pace ~dsq)

type medal_type = Gold | Silver | Bronze

let medal_type_to_string = function
  | Gold -> "gold"
  | Silver -> "silver"
  | Bronze -> "bronze"

let medal_type_of_string = function
  | "gold" -> Gold
  | "silver" -> Silver
  | "bronze" -> Bronze
  | _ -> assert false

(** NOTE: does not commit *)
let _add_medal (handle : Turso.conn) ~(event_params : EventParams.t)
    ~(course_id : string) ~(runner_id : int64) ~(medal : medal_type) =
  ignore
    (DB.add_medal handle ~id:None ~league_id:event_params.league_id
       ~event_nr:event_params.league_id ~event_date:event_params.event_date
       ~course_id ~runner_id
       ~medal_type:(medal_type_to_string medal))

(** Compute & add medals to db. Medals are determined based on overall time and
    not based on the field from stats (because sometimes stats are not
    available). If two people have the same time for second place (for example)
    they will both get a silver medal but there won't be a bronze medal since
    the next position afterwards will be 4th.

    NOTE: does not commit *)
let _add_medals (handle : Turso.conn) ~(event_params : EventParams.t)
    ~(course_id : string) (finished : Dbsportas.League.OverallResult.t list) =
  let sorted_groups =
    List.sort_and_group finished ~compare:(fun r1 r2 ->
        match (r1.time, r2.time) with
        | Some t1, Some t2 -> Int.compare t1 t2
        (* here we indicate that any value is smaller than NO_VALUE to push
           Nones to the end of the list *)
        | Some _, None -> -1
        | None, Some _ -> 1
        | None, None -> 0)
  in

  ignore
    (List.fold ~init:1
       ~f:(fun position group ->
         if position > 3 then position
         else
           let medal =
             match position with
             | 1 -> Gold
             | 2 -> Silver
             | 3 -> Bronze
             | _ -> assert false
           in
           List.iter group ~f:(fun r ->
               _add_medal ~event_params ~course_id
                 ~runner_id:(to_int64 r.runner_nr) ~medal handle);
           position + List.length group)
       sorted_groups)

(** Get map of runner_id -> age_group. *)
let all_groups (handle : Turso.conn) : string Int64.Map.t =
  let groups = ref Int64.Map.empty in
  DB.all_groups handle (fun ~id ~league_id ~runner_id ~age_group_id ->
      let _ = (id, league_id) in
      groups := Map.set !groups ~key:runner_id ~data:age_group_id);
  !groups

(** Add runner age group information to DB if DB doesn't already have it.

    NOTE: does not commit *)
let _add_age_group_for_runner (handle : Turso.conn)
    ~(event_params : EventParams.t) ~(runner_id : int64)
    ~(age_group_id : string) (groups : string Int64.Map.t) =
  match Map.find groups runner_id with
  | None ->
      ignore
        (DB.add_age_group_for_runner handle ~id:None
           ~league_id:event_params.league_id ~runner_id ~age_group_id)
  | Some _ -> ()

module RunnerInfo = struct
  type t = {
    id : int64;
    join_date : string;
    name : string;
    club : string;
    gender : string;
  }
  [@@deriving fields]
end

(** Get all runners from DB

    @return Map with runner id as key and {!RunnerInfo.t} as values *)
let all_runners (handle : Turso.conn) : RunnerInfo.t Int64.Map.t =
  let runners = ref Int64.Map.empty in
  DB.all_runners handle (fun ~id ~join_date ~name ~club ~gender ->
      let info = RunnerInfo.Fields.create ~id ~join_date ~name ~club ~gender in
      runners := Map.set !runners ~key:id ~data:info);
  !runners

(** Add runner information to DB if DB doesn't already have it.

    NOTE: does not commit *)
let _add_runner (handle : Turso.conn) ~(event_params : EventParams.t)
    ~(runner : Dbsportas.League.OverallResult.t)
    (runners : RunnerInfo.t Int64.Map.t) : unit =
  let runner_id = to_int64 runner.runner_nr in
  match Map.find runners runner_id with
  | None ->
      let group_prefix =
        if String.length runner.group.group < 2 then ""
        else String.sub ~pos:0 ~len:2 runner.group.group
      in
      let gender =
        match group_prefix with
        | "V-" -> Dbsportas.League.gender_men
        | "M-" -> Dbsportas.League.gender_women
        | _ ->
            printf "WARNING: unexpected gender prefix for runner: %d %s - %s\n"
              runner.runner_nr runner.name runner.group.group;
            ""
      in
      ignore
        (DB.add_runner handle ~id:runner_id ~join_date:event_params.event_date
           ~name:runner.name ~club:runner.club ~gender)
  | Some _ -> ()

(** NOTE: does not commit *)
let _add_results (handle : Turso.conn) ~(event_params : EventParams.t)
    ~(course_id : string) (results : Dbsportas.League.OverallResults.t) =
  let runners_in_results = results.finished @ results.dsq in
  let runner_age_groups = all_groups handle in
  let runners_in_db = all_runners handle in

  List.iter runners_in_results ~f:(fun result ->
      let runner_id = to_int64 result.runner_nr in
      let age_group_id = result.group.group in
      _add_result ~event_params ~course_id handle result;
      _add_result_stats ~event_params ~course_id ~runner_id handle result.stats;
      _add_splits ~event_params ~course_id ~runner_id handle result.splits;
      _add_age_group_for_runner ~event_params ~runner_id ~age_group_id handle
        runner_age_groups;
      _add_runner ~event_params ~runner:result handle runners_in_db);
  _add_medals ~event_params ~course_id handle results.finished

let all_latest_ratings (handle : Turso.conn) =
  ratings_aux ~f:DB.all_latest_ratings handle

let all_latest_relevant_ratings (handle : Turso.conn) ~participants =
  ratings_aux ~f:(CustomDb.all_latest_relevant_ratings ~participants) handle

(** NOTE: does not commit *)
let _update_ratings (handle : Turso.conn) ~(event_params : EventParams.t)
    (event : Dbsportas.League.LeagueEvent.t) =
  let settings =
    Glicko2.Settings.create ~tau:0.5 ~initial_rating:1500.0 ~rd:350.0 ~vol:0.06
      ()
  in
  let store = Dbsportas.Rating_store.Store.create ~settings in

  let current_ratings = all_latest_ratings handle in
  (* TODO: test this separately to make sure it works *)
  (* let current_ratings = *)
  (*   all_latest_relevant_ratings *)
  (*     ~participants:(Dbsportas.League.LeagueEvent.participants event) *)
  (*     handle *)
  (* in *)
  let store =
    List.fold current_ratings ~init:store ~f:(fun s rating ->
        Dbsportas.Rating_store.Store.add s ~name:rating.runner_name
          ~id:rating.runner_id ~course:rating.course_id ~rating:rating.rating
          ~rd:rating.rd ~vol:rating.vol)
  in

  let store =
    Dbsportas.Rating_store.calculate_ratings_for_event ~settings ~store
      ~league_id:(Int64.to_string event_params.league_id)
      event
  in

  let new_ratings =
    Dbsportas.Rating_store.Store.all_ratings store
      ~league_id:(of_int64 event_params.league_id)
      ~event_nr:(of_int64 event_params.event_nr)
      ~event_date:event_params.event_date
  in
  List.iter new_ratings ~f:(_add_rating handle)

(** Add full event info (including results, stats, ratings, medals, etc.)

    NOTE: does not commit *)
let _add_full_event ~(league_id : string) ~(is_part_of_main_league : bool)
    (handle : Turso.conn) (event : Dbsportas.League.LeagueEvent.t) =
  let event_params =
    EventParams.create ~league_id ~event_nr:event.nr ~event_date:event.date
  in
  let results = Option.value_exn event.results in

  _add_event_stats ~event_params handle results;

  List.iter results.courses ~f:(fun course ->
      let course_id = course.id in
      _add_course ~event_params handle course;
      _add_course_stats ~event_params handle course;
      _add_results ~event_params ~course_id handle course.results);

  if is_part_of_main_league then _update_ratings ~event_params handle event
  else ()

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
    let main_league_name = Dbsportas.League.LeagueInfo.main_league_name in
    let main_leagues = leagues_for_name handle main_league_name in

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
        let is_part_of_main_league =
          Option.is_some
            (List.find main_leagues ~f:(fun l -> l.id = Int.of_string league.id))
        in
        List.iter league.events
          ~f:
            (_add_full_event handle ~league_id:league.id ~is_part_of_main_league));

    (* ignore (assert false); *)

    (* send all statements to turso *)
    ignore (Turso.send_buffered handle)

let test_rating_fn (handle : Turso.conn) =
  let league_id = 141 in
  let event_nrs_to_include = [ 1 ] in
  let league =
    Dbsportas.League.download_league_info
      ~include_events:(Some event_nrs_to_include)
      ~league_id:(Int.to_string league_id) ()
  in
  let event = List.hd_exn league.events in
  let participants = Dbsportas.League.LeagueEvent.participants event in
  let all_ratings = all_latest_ratings handle in
  (* NOTE: this currently saves 80 rows on 1210 rows - so currently negligible
     but maybe in the future it would be more .
     The main problem is that the RD grows pretty slowly
     *)
  let relevant_ratings = all_latest_relevant_ratings handle ~participants in
  let sexp = [%sexp (all_ratings : Glicko2.Rating.Info.t list)] in
  printf "All ratings: \n";
  print_s sexp;
  printf "\n\nRelevant ratings: \n";
  let sexp = [%sexp (relevant_ratings : Glicko2.Rating.Info.t list)] in
  print_s sexp;
  printf "\n"

let test (handle : Turso.conn) =
  (* add_leagues_if_not_exists handle Dbsportas.League.leagues; *)
  (* action_refresh_events_and_results handle; *)
  (* action_refresh_event_details ~year:(Some "2026") handle; *)
  (* test_rating_fn handle; *)
  let _ = handle in
  ()

let%expect_test "make" =
  printf "hello";
  [%expect {| hello |}]
