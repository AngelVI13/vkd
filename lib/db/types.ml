open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

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

module RunnerInfo = struct
  type t = {
    id : int64;
    join_date : string;
    name : string;
    club : string;
    gender : string;
  }
  [@@deriving fields, yojson]
end

module EventParams = struct
  type t = { league_id : int64; event_nr : int64; event_date : string }

  let create ~(league_id : string) ~(event_nr : int) ~(event_date : Time_ns.t) =
    let league_id = Int64.of_string league_id in
    let event_nr = Helpers.to_int64 event_nr in
    let event_date = Utils.format_time_as_date event_date in
    { league_id; event_nr; event_date }
end

module LeagueEvent = struct
  type t = {
    league_id : int;
    event_nr : int;
    event_date : string;
    location : string;
  }
  [@@deriving fields, yojson]
end

module SimpleResult = struct
  type t = {
    league_id : int;
    event_nr : int;
    event_date : string;
    (* --- *)
    course_id : string;
    runner_id : int;
    time_sec : int option;
    start_time : int option;
    points : int;
    pace : string option;
    dsq : int;
    (* --- *)
    location : string;
    (* --- *)
    league_name : string;
  }
  [@@deriving fields, yojson]
end

module Medals = struct
  type t = { gold : int option; silver : int option; bronze : int option }
  [@@deriving fields, yojson]
end

module ResultStats = struct
  type t = {
    league_id : int;
    event_nr : int;
    event_date : string;
    (* --- *)
    course_id : string;
    runner_id : int;
    (* --- *)
    mistake_time : int;
    mistake_num : int;
    small_mistake_time : int;
    small_mistake_num : int;
    small_mistake_time_ratio : int;
    small_mistake_num_ratio : int;
    big_mistake_time : int;
    big_mistake_num : int;
    big_mistake_time_ratio : int;
    big_mistake_num_ratio : int;
    blunder_mistake_time : int;
    blunder_mistake_num : int;
    blunder_mistake_time_ratio : int;
    blunder_mistake_num_ratio : int;
    consecutive_mistakes : int;
    tilt_rate : int;
    mistake_cluster : Dbsportas.Runner_stats.mistakeCluster option;
    mistakes_impact : Dbsportas.Runner_stats.mistakesImpact option;
    race_execution : Dbsportas.Runner_stats.raceExecution option;
    best_splits : int;
    top5_splits : int;
    top10_splits : int;
    performance : int;
    overall_position : int option;
    position_gender : int option;
    position_group : int option;
    potential_time : int option;
    potential_position : int option;
  }
  [@@deriving fields, yojson]
end
