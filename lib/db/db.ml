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
    immediate = true;
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
  handle.immediate <- false;
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
  handle.immediate <- true;
  let _ = DB.create_medals handle in
  ()

(* NOTE: this is not needed for turso connection *)
let close _ = Ok ()

let action_download_event_details (handle : Turso.conn) =
  let _ = handle in
  (* TODO: implement this *)
  ()

let event_details_for_year (handle : Turso.conn) (year : string) :
    Vilpage.Events.EventInfo.t list =
  let event_details = ref [] in
  ignore
    (DB.event_details_for_year handle ~year
       (fun ~id ~event_link ~event_date ~location ~thumbnail ~map_info ~links ->
         let _ = id in
         let map_links =
           links |> String.split ~on:',' |> List.map ~f:Base64.decode_exn
         in
         let event =
           (* NOTE: the EventInfo object returned from here will be missing some info
              because we don't store results url & other data into the db *)
           Vilpage.Events.EventInfo.Fields.create
             ~date:(Time_ns_unix.of_string event_date)
             ~thumbnail ~thumbnail_src:"" ~event_link ~location ~map_info
             ~map_links ~map_settings:None ~result_link:None
         in
         event_details := event :: !event_details));
  !event_details

let add_event_details (handle : Turso.conn)
    (details : Vilpage.Events.EventInfo.t) =
  let _ = (handle, details) in
  (* TODO: continue from here *)
  DB.add_event_details handle ~id:None

let add_league (handle : Turso.conn) (league : Dbsportas.League.LeagueInfo.t) =
  DB.add_league handle ~id:(Int64.of_int league.id)
    ~league_year:(Int64.of_int league.year) ~name:league.name

let test_add_leagues (handle : Turso.conn)
    (leagues : Dbsportas.League.LeagueInfo.t list) =
  ignore (List.map leagues ~f:(add_league handle))

let test (handle : Turso.conn) =
  let _ = handle in
  printf "hello\n";
  (* test_add_leagues handle *)
  ()

let%expect_test "make" =
  printf "hello";
  [%expect {| hello |}]
