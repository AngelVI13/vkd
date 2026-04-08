open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

let base_url = "https://dbsportas.lt"
let league_url = "https://dbsportas.lt/lt/mvarz"
let league_num_placeholder = "LEAGUE_NUM"
let event_num_placeholder = "EVENT_NUM"

let data_url =
  sprintf "%s%s" base_url "/msplitdata.php?varz=LEAGUE_NUM&turas=EVENT_NUM"
(* "https://dbsportas.lt/msplitdata.php?varz=244&turas=6" *)

(* https://dbsportas.lt/lt/mvarz/244 *)
let fetch_page__ ~(name : string) url =
  let res = Ezcurl.get ~url () in
  let out = match res with Ok c -> c.body | Error (_, s) -> failwith s in
  Out_channel.write_all
    (sprintf "/home/angel/Documents/ocaml/vkd/%s.html" name)
    ~data:out

let fetch_page url =
  (* if String.(url = "https://dbsportas.lt/msplitdata.php?varz=244&turas=6") then *)
  (*   In_channel.read_all "/home/angel/Documents/ocaml/vkd/2025_kaminai_data.json" *)
  (* else *)
  let res = Ezcurl.get ~url () in
  let out = match res with Ok c -> c.body | Error (_, s) -> failwith s in
  out

(* NOTE: this is the final result details for each runner, it merges data from the main results 
   and data from the split results *)
module OverallResult = struct
  type t = {
    name : string; (* stored in the format: LASTNAME FIRSTNAME *)
    club : string;
    runner_nr : int;
    group : Simple_result.AgeGroup.t;
    time : int option;
    start : int option;
    points : int;
    pace : string option; (* in min/km i.e.: 6:40 OR `dsq`*)
    status : Runner_result.resultStatus;
    splits : Splits.t option;
    stats : Runner_stats.t option;
  }
  [@@deriving yojson, fields]
end

(* NOTE: this is the final result list for the whole course *)
module OverallResults = struct
  type t = { finished : OverallResult.t list; dsq : OverallResult.t list }
  [@@deriving yojson]

  let of_simple_and_detailed_results
      ~(simple : Simple_result.CourseResult.t list)
      ~(detailed : Results.CourseResult.t option) =
    let of_results ~(status : Runner_result.resultStatus)
        (results : Simple_result.CourseResult.t list) =
      List.map results ~f:(fun r ->
          let splits, stats, start, time =
            match detailed with
            | None -> (None, None, None, Utils.time_of_string r.time)
            | Some detailed ->
                let detailed_stats =
                  match status with
                  | Finished -> detailed.finished
                  | Dsq -> detailed.dsq
                in

                let detailed_r =
                  List.find detailed_stats ~f:(fun detailed_r ->
                      r.number = detailed_r.number)
                in
                let splits =
                  Option.bind detailed_r ~f:(fun r -> Some r.splits)
                in
                let stats = Option.bind detailed_r ~f:(fun r -> Some r.stats) in
                let start = Option.bind detailed_r ~f:(fun r -> Some r.start) in
                let time =
                  match detailed_r with
                  | Some runnner -> Some runnner.time
                  | None -> Utils.time_of_string r.time
                in

                (splits, stats, start, time)
          in

          OverallResult.Fields.create ~name:r.name ~club:r.club
            ~runner_nr:r.number ~group:r.group ~time ~start ~points:r.points
            ~pace:r.pace ~status ~splits ~stats)
    in

    let finished =
      of_results ~status:Finished
        (List.filter simple ~f:(fun r -> Option.is_some r.pace))
    in
    let dsq =
      of_results ~status:Dsq
        (List.filter simple ~f:(fun r -> Option.is_none r.pace))
    in

    { finished; dsq }
end

module CourseStats = struct
  type t = {
    num_men : int;
    num_women : int;
    tilt_overall : int;
    tilt_men : int;
    tilt_women : int;
    mistake_time_overall : int;
    mistake_time_men : int;
    mistake_time_women : int;
    blunder_perc_overall : int;
    blunder_perc_men : int;
    blunder_perc_women : int;
    big_mistake_perc_overall : int;
    big_mistake_perc_men : int;
    big_mistake_perc_women : int;
    small_mistake_perc_overall : int;
    small_mistake_perc_men : int;
    small_mistake_perc_women : int;
    most_tricky_overall : int option;
    most_tricky_men : int option;
    most_tricky_women : int option;
    avg_time_for_mistake_overall : int;
    avg_time_for_mistake_men : int;
    avg_time_for_mistake_women : int;
    avg_mistake_num_overall : int;
    avg_mistake_num_men : int;
    avg_mistake_num_women : int;
  }
  [@@deriving yojson]

  let runners_by_gender ~(gender_prefix : string)
      (runners : OverallResult.t list) =
    List.filter runners ~f:(fun r ->
        String.is_prefix ~prefix:gender_prefix r.group.group)

  let most_occurance_from_map ~compare map =
    let alist =
      Map.to_alist map
      |> List.sort ~compare:(fun (_, ocur1) (_, ocur2) -> compare ocur1 ocur2)
    in
    match List.last alist with None -> None | Some (value, _) -> Some value

  let calculate_percent total divide_by =
    if divide_by = 0 then 0
    else Float.(to_int (round_nearest (of_int total /. of_int divide_by)))

  let avg_stat ~field (runners : OverallResult.t list) =
    let total, found =
      List.fold runners ~init:(0, 0) ~f:(fun (total, found) r ->
          match r.stats with
          | None -> (total, found)
          | Some stats ->
              let value = Field.get field stats in
              (total + value, found + 1))
    in
    calculate_percent total found

  let mode ~map data =
    let map =
      List.fold data ~init:map ~f:(fun m el ->
          Map.change m el ~f:(fun occurances ->
              match occurances with None -> Some 1 | Some oc -> Some (oc + 1)))
    in
    most_occurance_from_map ~compare:Int.compare map

  let avg_blunder_perc (runners : OverallResult.t list) =
    let total, found =
      List.fold runners ~init:(0, 0) ~f:(fun (total, found) r ->
          match r.stats with
          | None -> (total, found)
          | Some stats -> (total + stats.blunder_mistakes.num_ratio, found + 1))
    in
    calculate_percent total found

  let avg_big_mistake_perc (runners : OverallResult.t list) =
    let total, found =
      List.fold runners ~init:(0, 0) ~f:(fun (total, found) r ->
          match r.stats with
          | None -> (total, found)
          | Some stats -> (total + stats.big_mistakes.num_ratio, found + 1))
    in
    calculate_percent total found

  let avg_small_mistake_perc (runners : OverallResult.t list) =
    let total, found =
      List.fold runners ~init:(0, 0) ~f:(fun (total, found) r ->
          match r.stats with
          | None -> (total, found)
          | Some stats -> (total + stats.small_mistakes.num_ratio, found + 1))
    in
    calculate_percent total found

  let most_tricky_control (runners : OverallResult.t list) =
    let mistakes =
      List.fold runners ~init:[] ~f:(fun mistakes r ->
          match r.stats with
          | None -> mistakes
          | Some stats ->
              (* NOTE: convert from indexes to control numbers *)
              mistakes @ List.map stats.mistake_indexes ~f:(fun m -> m + 1))
    in
    mode ~map:Int.Map.empty mistakes

  let macro_avg_mistake_time (runners : OverallResult.t list) =
    let averages =
      List.fold runners ~init:[] ~f:(fun averages r ->
          match r.splits with
          | None -> averages
          | Some splits ->
              let total, found =
                List.fold splits ~init:(0, 0) ~f:(fun (total, found) split ->
                    match split.mistake_time with
                    | None -> (total, found)
                    | Some m -> (total + m, found + 1))
              in
              calculate_percent total found :: averages)
    in
    calculate_percent
      (List.fold_left averages ~init:0 ~f:( + ))
      (List.length averages)

  let avg_mistake_num (runners : OverallResult.t list) =
    let averages =
      List.fold runners ~init:[] ~f:(fun averages r ->
          match r.stats with
          | None -> averages
          | Some stats -> stats.mistake_num :: averages)
    in
    calculate_percent
      (List.fold_left averages ~init:0 ~f:( + ))
      (List.length averages)

  let of_results (results : OverallResults.t) : t =
    (* NOTE: disqualified runners are not included in the stats *)
    let runners = results.finished in
    let all_runners = results.finished @ results.dsq in

    let men = runners_by_gender ~gender_prefix:"V-" runners in
    let all_men = runners_by_gender ~gender_prefix:"V-" all_runners in
    let women = runners_by_gender ~gender_prefix:"M-" runners in
    let all_women = runners_by_gender ~gender_prefix:"M-" all_runners in

    let tilt_overall = avg_stat ~field:Runner_stats.Fields.tilt_rate runners in
    let tilt_men = avg_stat ~field:Runner_stats.Fields.tilt_rate men in
    let tilt_women = avg_stat ~field:Runner_stats.Fields.tilt_rate women in

    let mistake_time_overall =
      avg_stat ~field:Runner_stats.Fields.mistake_time runners
    in
    let mistake_time_men =
      avg_stat ~field:Runner_stats.Fields.mistake_time men
    in
    let mistake_time_women =
      avg_stat ~field:Runner_stats.Fields.mistake_time women
    in

    let blunder_perc_overall = avg_blunder_perc runners in
    let blunder_perc_men = avg_blunder_perc men in
    let blunder_perc_women = avg_blunder_perc women in

    let big_mistake_perc_overall = avg_big_mistake_perc runners in
    let big_mistake_perc_men = avg_big_mistake_perc men in
    let big_mistake_perc_women = avg_big_mistake_perc women in

    let small_mistake_perc_overall = avg_small_mistake_perc runners in
    let small_mistake_perc_men = avg_small_mistake_perc men in
    let small_mistake_perc_women = avg_small_mistake_perc women in

    let most_tricky_overall = most_tricky_control runners in
    let most_tricky_men = most_tricky_control men in
    let most_tricky_women = most_tricky_control women in

    let avg_time_for_mistake_overall = macro_avg_mistake_time runners in
    let avg_time_for_mistake_men = macro_avg_mistake_time men in
    let avg_time_for_mistake_women = macro_avg_mistake_time women in

    let avg_mistake_num_overall = avg_mistake_num runners in
    let avg_mistake_num_men = avg_mistake_num men in
    let avg_mistake_num_women = avg_mistake_num women in

    {
      num_men = List.length all_men;
      num_women = List.length all_women;
      tilt_overall;
      tilt_men;
      tilt_women;
      mistake_time_overall;
      mistake_time_men;
      mistake_time_women;
      blunder_perc_overall;
      blunder_perc_men;
      blunder_perc_women;
      big_mistake_perc_overall;
      big_mistake_perc_men;
      big_mistake_perc_women;
      small_mistake_perc_overall;
      small_mistake_perc_men;
      small_mistake_perc_women;
      most_tricky_overall;
      most_tricky_men;
      most_tricky_women;
      avg_time_for_mistake_overall;
      avg_time_for_mistake_men;
      avg_time_for_mistake_women;
      avg_mistake_num_overall;
      avg_mistake_num_men;
      avg_mistake_num_women;
    }
end

module Course = struct
  type t = {
    url : string;
    id : string;
    hash : int;
    distance : float;
    controls_num : int;
    controls : string list option;
    results : OverallResults.t;
    stats : CourseStats.t;
  }
  [@@deriving fields, yojson]
end

module EventResults = struct
  type t = { url : string; courses : Course.t list } [@@deriving fields, yojson]
end

module LeagueEvent = struct
  type t = {
    nr : int;
    date : Time_ns_unix.t;
    location : string;
    results : EventResults.t option;
  }

  let of_td_list ~results td_list =
    assert (List.length td_list >= 3);
    let date = List.nth_exn td_list 1 in
    {
      nr = Int.of_string @@ List.nth_exn td_list 0;
      date = Time_ns_unix.parse ~fmt:"%Y-%m-%d" ~zone:Timezone.utc date;
      location = List.nth_exn td_list 2;
      results;
    }

  (* NOTE: here we define custom printers for the LeagueEvent.t . This is
     needed because [@@deriving show] does not display the unicode characters
     correctly for some reason. *)
  let pp ppf r =
    let results_url =
      match r.results with
      | None -> "None"
      | Some results -> sprintf "Some( %s )" results.url
    in
    Format.fprintf ppf "{ nr: %d; date: %s; location: %s; results_url: %s }"
      r.nr
      (Utils.format_time_as_date r.date)
      r.location results_url

  let show r = Format.asprintf "%a" pp r
end

module League = struct
  type t = { id : string; url : string; events : LeagueEvent.t list }
  [@@deriving show { with_path = false }, fields]
end

(** Strip string (remove whitespaces, tabs & newlines before and after the
    string). This also removes NO-BREAK SPACE codepoint (the equivalent of the
    &nbsp; entity in HTML). *)
let strip s =
  s |> String.strip |> String.substr_replace_all ~pattern:"\194\160" ~with_:""

(** parse dbsportas generic table and returns all rows (except header) *)
let parse_table_rows soup =
  let open Soup in
  soup $ ".w3-table" $$ "tr" |> to_list |> List.tl_exn

let parse_course page_html =
  let open Soup in
  let soup = parse page_html in
  let rows = parse_table_rows soup in
  let results =
    rows
    |> List.fold ~init:[] ~f:(fun acc tr ->
           let group_col = tr $ ".w3-text-green" in
           let group_results_url = group_col |> R.attribute "href" in
           let group_results_url = sprintf "%s%s" base_url group_results_url in
           let group_name = group_col |> R.leaf_text |> String.strip in

           let columns = tr $$ "td" |> to_list |> List.map ~f:R.leaf_text in
           assert (List.length columns = 8);

           let position, number, name, club, time, points, pace =
             match columns with
             | [ position; number; _; name; club; time; points; pace ] ->
                 let position = strip position in
                 let position =
                   if String.(position = "") then None
                   else Some (Int.of_string position)
                 in

                 let pace = strip pace in
                 let pace = if String.(pace = "") then None else Some pace in

                 ( position,
                   Int.of_string number,
                   String.strip name,
                   String.strip club,
                   String.strip time,
                   Int.of_string points,
                   pace )
             | _ ->
                 failwith
                   (sprintf
                      "unexpected number of columns in results table: %d \
                       (expected 8)"
                      (List.length columns))
           in

           let group =
             Simple_result.AgeGroup.Fields.create ~url:group_results_url
               ~group:group_name
           in

           let result =
             Simple_result.CourseResult.Fields.create ~position ~number ~group
               ~name ~club ~time ~points ~pace
           in
           result :: acc)
  in
  List.rev results

let parse_event ~league_id ~event_nr page_html =
  let open Soup in
  let soup = parse page_html in
  let rows = parse_table_rows soup in

  (* NOTE: download event splits data directly from the backed *)
  let event_data_url =
    String.substr_replace_all data_url ~pattern:league_num_placeholder
      ~with_:league_id
    |> String.substr_replace_all ~pattern:event_num_placeholder ~with_:event_nr
  in
  printf "Downloading backend data for event: %s\n" event_data_url;
  let event_data = fetch_page event_data_url in

  let results_table_resp = Results.parse_course_results_table event_data in
  let results_table = Results.ResultsTable.of_resp results_table_resp in
  (* List.iter results_table.course_results ~f:(fun c -> *)
  (*     printf "%s\n" (Results.CourseResult.yojson_of_t c |> Yojson.Safe.to_string)); *)

  let courses =
    rows
    |> List.fold ~init:[] ~f:(fun acc tr ->
           let course_col = tr $ ".w3-text-green" in
           let course_results_url = course_col |> R.attribute "href" in
           let course_results_url =
             sprintf "%s%s" base_url course_results_url
           in
           let course_id = course_col |> R.leaf_text |> strip in
           let detailed_results =
             List.find results_table.course_results ~f:(fun course_result ->
                 String.(course_id = course_result.course_name))
           in

           let ids = String.split course_id ~on:',' |> List.map ~f:strip in

           let course_parameters =
             tr $$ "td" |> to_list |> List.tl_exn |> List.map ~f:R.leaf_text
             |> List.map ~f:strip |> List.hd_exn
           in
           let course_parameters =
             course_parameters
             (* example course parameters: '4.420 km 21 KP' *)
             |> String.substr_replace_all ~pattern:"km " ~with_:""
             |> String.substr_replace_all ~pattern:"KP" ~with_:""
             |> strip
           in
           let course_parameters = String.split course_parameters ~on:' ' in
           let distance, controls_num =
             match course_parameters with
             | [ dist; pts ] -> (Float.of_string dist, Int.of_string pts)
             | _ -> assert false
           in

           let results_page = fetch_page course_results_url in
           let results = parse_course results_page in

           (* we can only calculate gender and group positions with the simple results data 
              because group info is not present in the splits data *)
           let detailed_results =
             Option.bind detailed_results ~f:(fun detailed_results ->
                 Some
                   (Results.CourseResult.update_gender_and_group_positions
                      detailed_results results))
           in

           let overall_results =
             OverallResults.of_simple_and_detailed_results ~simple:results
               ~detailed:detailed_results
           in

           let controls =
             Option.bind detailed_results ~f:(fun r -> Some r.controls)
           in
           let hash =
             match detailed_results with
             | None -> hash_string (sprintf "%s_%s" league_id course_id)
             | Some r -> r.course_id
           in

           let courses =
             List.map ids ~f:(fun id ->
                 let stats = CourseStats.of_results overall_results in
                 Course.Fields.create ~url:course_results_url ~id ~distance
                   ~hash ~controls_num ~controls ~results:overall_results ~stats)
           in
           acc @ courses)
  in

  courses

let parse_league_page ~league_id page_html =
  let open Soup in
  let soup = parse page_html in
  let rows = parse_table_rows soup in

  let events =
    rows
    |> List.fold ~init:[] ~f:(fun acc tr ->
           let tds =
             tr $$ "td" |> to_list |> List.map ~f:R.leaf_text
             |> List.map ~f:strip
           in
           let event_nr =
             Option.bind (List.nth tds 0) ~f:(fun nr ->
                 match Int.of_string with exception _ -> None | _ -> Some nr)
           in

           let results =
             tr $? ".w3-text-green"
             |> Option.bind ~f:(fun a ->
                    let url = R.attribute "href" a in
                    let url = sprintf "%s%s" base_url url in
                    printf "Downloading event page: %s\n" url;
                    let results_html = fetch_page url in
                    Time_ns_unix.pause (Time_ns.Span.create ~ms:1000 ());
                    let event_nr = Option.value_exn event_nr in
                    let courses =
                      parse_event ~league_id ~event_nr results_html
                    in
                    Some (EventResults.Fields.create ~url ~courses))
           in
           match tds with
           | [] -> acc
           | _ ->
               let event = LeagueEvent.of_td_list ~results tds in
               event :: acc)
  in
  List.rev events

let download_league_info ~league_id =
  let url = sprintf "%s/%s" league_url league_id in
  let page_html = fetch_page url in
  let events = parse_league_page ~league_id page_html in
  League.Fields.create ~url ~events ~id:league_id

module LeagueInfo = struct
  type t = { name : string; year : int; id : int } [@@deriving show, fields]

  let main_league_name = "Vilniaus ketvirtadieniai"
  let create_main = Fields.create ~name:main_league_name
end

let leagues =
  [
    (* 2020 *)
    LeagueInfo.create_main ~year:2020 ~id:141;
    LeagueInfo.Fields.create ~name:"Naktiniai Vilniaus ketvirtadieniai"
      ~year:2020 ~id:150;
    (* 2021 *)
    LeagueInfo.create_main ~year:2021 ~id:155;
    LeagueInfo.Fields.create ~name:"Naktiniai Vilniaus ketvirtadieniai"
      ~year:2021 ~id:167;
    (* 2022 *)
    LeagueInfo.create_main ~year:2022 ~id:182;
    LeagueInfo.Fields.create ~name:"Apuoko sprintai" ~year:2022 ~id:183;
    LeagueInfo.Fields.create ~name:"Naktiniai Vilniaus ketvirtadieniai"
      ~year:2022 ~id:192;
    (* 2023 *)
    LeagueInfo.create_main ~year:2023 ~id:205;
    LeagueInfo.Fields.create ~name:"Apuoko sprintai" ~year:2023 ~id:208;
    LeagueInfo.Fields.create ~name:"Naktiniai Vilniaus ketvirtadieniai"
      ~year:2023 ~id:214;
    (* 2024 *)
    LeagueInfo.create_main ~year:2024 ~id:217;
    LeagueInfo.Fields.create ~name:"Apuoko sprintai" ~year:2024 ~id:226;
    LeagueInfo.Fields.create ~name:"Naktiniai Vilniaus ketvirtadieniai"
      ~year:2024 ~id:227;
    (* 2025 *)
    LeagueInfo.create_main ~year:2025 ~id:244;
    LeagueInfo.Fields.create ~name:"Apuoko lyga" ~year:2025 ~id:243;
    (* 2026 *)
    LeagueInfo.create_main ~year:2026 ~id:269;
    LeagueInfo.Fields.create ~name:"Apuoko lyga" ~year:2026 ~id:272;
  ]

let%expect_test "download_league_info" =
  (* 
     id - 217 -> 2024 
     id - 244 -> 2025 
     id - 269 -> 2026
     *)
  let league_id = "269" in

  (* let league = download_league_info ~league_id in *)
  (* let _ = *)
  (*   League.yojson_of_t league *)
  (*   |> Yojson.Safe.to_file *)
  (*        (sprintf "/home/angel/Documents/ocaml/vkd/league%s_full.json" league_id) *)
  (* in *)
  let _ = league_id in
  printf "hello1";
  [%expect {| hello1 |}]

(* let%expect_test "parse_league_page finished league" = *)
(*   let filename = "/home/angel/Documents/ocaml/vkd/league.html" in *)
(*   let page_html = In_channel.read_all filename in *)
(*   let events = parse_league_page page_html in *)
(*   let league = League.Fields.create ~url:"" ~events in *)
(*   printf "%s" (League.show league); *)
(*   [%expect *)
(*     {| *)
(*     { url = ""; *)
(*       events = *)
(*       [{ nr: 1; date: 2025-03-27; location: Antakalnis; results_url: Some( /lt/mvarz/244/reztur/1 ) }; *)
(*         { nr: 2; date: 2025-04-03; location: Belmontas; results_url: Some( /lt/mvarz/244/reztur/2 ) }; *)
(*         { nr: 3; date: 2025-04-10; location: Bukčiai; results_url: Some( /lt/mvarz/244/reztur/3 ) }; *)
(*         { nr: 4; date: 2025-04-17; location: Dvarčionys; results_url: Some( /lt/mvarz/244/reztur/4 ) }; *)
(*         { nr: 5; date: 2025-04-24; location: Skersinė; results_url: Some( /lt/mvarz/244/reztur/5 ) }; *)
(*         { nr: 6; date: 2025-05-01; location: Kaminai (Apuoko lyga); results_url: Some( /lt/mvarz/244/reztur/6 ) }; *)
(*         { nr: 7; date: 2025-05-08; location: Ozas; results_url: Some( /lt/mvarz/244/reztur/7 ) }; *)
(*         { nr: 8; date: 2025-05-15; location: Šnipiškės; results_url: Some( /lt/mvarz/244/reztur/8 ) }; *)
(*         { nr: 9; date: 2025-05-22; location: Karoliniškės; results_url: Some( /lt/mvarz/244/reztur/9 ) }; *)
(*         { nr: 10; date: 2025-05-29; location: Žirmūnai; results_url: Some( /lt/mvarz/244/reztur/10 ) }; *)
(*         { nr: 11; date: 2025-06-05; location: Jeruzalė; results_url: Some( /lt/mvarz/244/reztur/11 ) }; *)
(*         { nr: 12; date: 2025-06-12; location: Šveicarija; results_url: Some( /lt/mvarz/244/reztur/12 ) }; *)
(*         { nr: 13; date: 2025-06-19; location: Karačiūnai; results_url: Some( /lt/mvarz/244/reztur/13 ) }; *)
(*         { nr: 14; date: 2025-06-26; location: Jomantas; results_url: Some( /lt/mvarz/244/reztur/14 ) }; *)
(*         { nr: 15; date: 2025-07-03; location: Smėlynė; results_url: Some( /lt/mvarz/244/reztur/15 ) }; *)
(*         { nr: 16; date: 2025-07-10; location: Vismalai; results_url: Some( /lt/mvarz/244/reztur/16 ) }; *)
(*         { nr: 17; date: 2025-07-17; location: Verkiai; results_url: Some( /lt/mvarz/244/reztur/17 ) }; *)
(*         { nr: 18; date: 2025-07-24; location: Šilėnai; results_url: Some( /lt/mvarz/244/reztur/18 ) }; *)
(*         { nr: 19; date: 2025-07-31; location: Vismaliukai; results_url: Some( /lt/mvarz/244/reztur/19 ) }; *)
(*         { nr: 20; date: 2025-08-07; location: Aukštagiris; results_url: Some( /lt/mvarz/244/reztur/20 ) }; *)
(*         { nr: 21; date: 2025-08-14; location: Balžis; results_url: Some( /lt/mvarz/244/reztur/21 ) }; *)
(*         { nr: 22; date: 2025-08-21; location: Strielčiukai; results_url: Some( /lt/mvarz/244/reztur/22 ) }; *)
(*         { nr: 23; date: 2025-08-28; location: Gulbinėliai; results_url: Some( /lt/mvarz/244/reztur/23 ) }; *)
(*         { nr: 24; date: 2025-09-04; location: Kalvarijos; results_url: Some( /lt/mvarz/244/reztur/24 ) }; *)
(*         { nr: 25; date: 2025-09-11; location: Visoriai; results_url: Some( /lt/mvarz/244/reztur/25 ) }; *)
(*         { nr: 26; date: 2025-09-18; location: Bajorai; results_url: Some( /lt/mvarz/244/reztur/26 ) }; *)
(*         { nr: 27; date: 2025-09-25; location: Šeškinė; results_url: Some( /lt/mvarz/244/reztur/27 ) }; *)
(*         { nr: 28; date: 2025-10-02; location: Pilaitė; results_url: Some( /lt/mvarz/244/reztur/28 ) }; *)
(*         { nr: 29; date: 2025-10-09; location: Gudeliai; results_url: Some( /lt/mvarz/244/reztur/29 ) }; *)
(*         { nr: 30; date: 2025-10-16; location: Vingis; results_url: Some( /lt/mvarz/244/reztur/30 ) } *)
(*         ] *)
(*       } |}]; *)
(**)
(*   printf "%s" (Yojson.Safe.to_string @@ League.yojson_of_t league); *)
(*   [%expect *)
(*     {| {"url":"","events":[{"nr":1,"date":"2025-03-27","location":"Antakalnis","results":{"url":"/lt/mvarz/244/reztur/1","courses":[]}},{"nr":2,"date":"2025-04-03","location":"Belmontas","results":{"url":"/lt/mvarz/244/reztur/2","courses":[]}},{"nr":3,"date":"2025-04-10","location":"Bukčiai","results":{"url":"/lt/mvarz/244/reztur/3","courses":[]}},{"nr":4,"date":"2025-04-17","location":"Dvarčionys","results":{"url":"/lt/mvarz/244/reztur/4","courses":[]}},{"nr":5,"date":"2025-04-24","location":"Skersinė","results":{"url":"/lt/mvarz/244/reztur/5","courses":[]}},{"nr":6,"date":"2025-05-01","location":"Kaminai (Apuoko lyga)","results":{"url":"/lt/mvarz/244/reztur/6","courses":[]}},{"nr":7,"date":"2025-05-08","location":"Ozas","results":{"url":"/lt/mvarz/244/reztur/7","courses":[]}},{"nr":8,"date":"2025-05-15","location":"Šnipiškės","results":{"url":"/lt/mvarz/244/reztur/8","courses":[]}},{"nr":9,"date":"2025-05-22","location":"Karoliniškės","results":{"url":"/lt/mvarz/244/reztur/9","courses":[]}},{"nr":10,"date":"2025-05-29","location":"Žirmūnai","results":{"url":"/lt/mvarz/244/reztur/10","courses":[]}},{"nr":11,"date":"2025-06-05","location":"Jeruzalė","results":{"url":"/lt/mvarz/244/reztur/11","courses":[]}},{"nr":12,"date":"2025-06-12","location":"Šveicarija","results":{"url":"/lt/mvarz/244/reztur/12","courses":[]}},{"nr":13,"date":"2025-06-19","location":"Karačiūnai","results":{"url":"/lt/mvarz/244/reztur/13","courses":[]}},{"nr":14,"date":"2025-06-26","location":"Jomantas","results":{"url":"/lt/mvarz/244/reztur/14","courses":[]}},{"nr":15,"date":"2025-07-03","location":"Smėlynė","results":{"url":"/lt/mvarz/244/reztur/15","courses":[]}},{"nr":16,"date":"2025-07-10","location":"Vismalai","results":{"url":"/lt/mvarz/244/reztur/16","courses":[]}},{"nr":17,"date":"2025-07-17","location":"Verkiai","results":{"url":"/lt/mvarz/244/reztur/17","courses":[]}},{"nr":18,"date":"2025-07-24","location":"Šilėnai","results":{"url":"/lt/mvarz/244/reztur/18","courses":[]}},{"nr":19,"date":"2025-07-31","location":"Vismaliukai","results":{"url":"/lt/mvarz/244/reztur/19","courses":[]}},{"nr":20,"date":"2025-08-07","location":"Aukštagiris","results":{"url":"/lt/mvarz/244/reztur/20","courses":[]}},{"nr":21,"date":"2025-08-14","location":"Balžis","results":{"url":"/lt/mvarz/244/reztur/21","courses":[]}},{"nr":22,"date":"2025-08-21","location":"Strielčiukai","results":{"url":"/lt/mvarz/244/reztur/22","courses":[]}},{"nr":23,"date":"2025-08-28","location":"Gulbinėliai","results":{"url":"/lt/mvarz/244/reztur/23","courses":[]}},{"nr":24,"date":"2025-09-04","location":"Kalvarijos","results":{"url":"/lt/mvarz/244/reztur/24","courses":[]}},{"nr":25,"date":"2025-09-11","location":"Visoriai","results":{"url":"/lt/mvarz/244/reztur/25","courses":[]}},{"nr":26,"date":"2025-09-18","location":"Bajorai","results":{"url":"/lt/mvarz/244/reztur/26","courses":[]}},{"nr":27,"date":"2025-09-25","location":"Šeškinė","results":{"url":"/lt/mvarz/244/reztur/27","courses":[]}},{"nr":28,"date":"2025-10-02","location":"Pilaitė","results":{"url":"/lt/mvarz/244/reztur/28","courses":[]}},{"nr":29,"date":"2025-10-09","location":"Gudeliai","results":{"url":"/lt/mvarz/244/reztur/29","courses":[]}},{"nr":30,"date":"2025-10-16","location":"Vingis","results":{"url":"/lt/mvarz/244/reztur/30","courses":[]}}]} |}] *)
(**)
(* let%expect_test "parse_league_page upcomming league" = *)
(*   let filename = "/home/angel/Documents/ocaml/vkd/2026_league.html" in *)
(*   let page_html = In_channel.read_all filename in *)
(*   let events = parse_league_page page_html in *)
(*   let league = League.Fields.create ~url:"" ~events in *)
(*   printf "%s" (League.show league); *)
(*   [%expect *)
(*     {| *)
(*     { url = ""; *)
(*       events = *)
(*       [{ nr: 1; date: 2026-03-26; location: Antakalnis; results_url: None }; *)
(*         { nr: 2; date: 2026-04-02; location: Bukčiai; results_url: None }; *)
(*         { nr: 3; date: 2026-04-09; location: Pilaitė; results_url: None }; *)
(*         { nr: 4; date: 2026-04-16; location: Skersinė; results_url: None }; *)
(*         { nr: 5; date: 2026-04-23; location: Gudeliai; results_url: None }; *)
(*         { nr: 6; date: 2026-04-30; location: Belmontas; results_url: None }; *)
(*         { nr: 7; date: 2026-05-07; location: Kaminai; results_url: None }; *)
(*         { nr: 8; date: 2026-05-14; location: Dvarčionys; results_url: None }; *)
(*         { nr: 9; date: 2026-05-21; location: Karoliniškės; results_url: None }; *)
(*         { nr: 10; date: 2026-05-28; location: Sapiegynė; results_url: None }; *)
(*         { nr: 11; date: 2026-06-04; location: Žirmūnai; results_url: None }; *)
(*         { nr: 12; date: 2026-06-11; location: Ozas; results_url: None }; *)
(*         { nr: 13; date: 2026-06-18; location: Smėlynė; results_url: None }; *)
(*         { nr: 14; date: 2026-06-25; location: Karačiūnai; results_url: None }; *)
(*         { nr: 15; date: 2026-07-02; location: Jomantas; results_url: None }; *)
(*         { nr: 16; date: 2026-07-09; location: Balžis; results_url: None }; *)
(*         { nr: 17; date: 2026-07-16; location: Verkiai; results_url: None }; *)
(*         { nr: 18; date: 2026-07-23; location: Šilėnai; results_url: None }; *)
(*         { nr: 19; date: 2026-07-30; location: Vismalai; results_url: None }; *)
(*         { nr: 20; date: 2026-08-06; location: Aukštagiris; results_url: None }; *)
(*         { nr: 21; date: 2026-08-13; location: Vismaliukai; results_url: None }; *)
(*         { nr: 22; date: 2026-08-20; location: Strielčiukai; results_url: None }; *)
(*         { nr: 23; date: 2026-08-27; location: Gulbinėliai; results_url: None }; *)
(*         { nr: 24; date: 2026-09-03; location: Kalvarijos; results_url: None }; *)
(*         { nr: 25; date: 2026-09-10; location: Lazdynai; results_url: None }; *)
(*         { nr: 26; date: 2026-09-17; location: Visoriai; results_url: None }; *)
(*         { nr: 27; date: 2026-09-24; location: Bajorai; results_url: None }; *)
(*         { nr: 28; date: 2026-10-01; location: Valakampiai; results_url: None }; *)
(*         { nr: 29; date: 2026-10-08; location: Šeškinė; results_url: None }; *)
(*         { nr: 30; date: 2026-10-15; location: Vingis; results_url: None }] *)
(*       } *)
(*            |}] *)

let%expect_test "parse_event grouped courses" =
  (* let url = "https://dbsportas.lt/lt/mvarz/244/reztur/6" in *)
  (* let out = fetch_page url in *)
  (* Out_channel.write_all "/home/angel/Documents/ocaml/vkd/2025_kaminai.html" *)
  (*   ~data:out; *)
  let filename = "/home/angel/Documents/ocaml/vkd/2025_kaminai.html" in
  let _ = filename in
  (* let page_html = In_channel.read_all filename in *)
  (* let courses = parse_event ~league_id:"244" ~event_nr:"6" page_html in *)
  (* let course = List.hd_exn courses in *)
  (* printf "%s\n" (Course.yojson_of_t course |> Yojson.Safe.to_string); *)
  (* let event_results = EventResults.Fields.create ~url:"" ~courses in *)
  (* let out = EventResults.yojson_of_t event_results |> Yojson.Safe.to_string in *)
  (* Out_channel.write_all "/home/angel/Documents/ocaml/vkd/league244_event1.json" *)
  (*   ~data:out; *)
  printf "%s\n" "hello";
  [%expect {|
    hello
           |}]
(* |}]; *)

(* printf "%s" (Yojson.Safe.to_string @@ EventResults.yojson_of_t event_results); *)
(* [%expect *)
(*   {| {"url":"","courses":[{"url":"/lt/mvarz/244/reztra/1/1%2C2","id":"1","distance":4.42,"controls":21,"results":[]},{"url":"/lt/mvarz/244/reztra/1/1%2C2","id":"2","distance":4.42,"controls":21,"results":[]},{"url":"/lt/mvarz/244/reztra/1/3","id":"3","distance":3.44,"controls":16,"results":[]},{"url":"/lt/mvarz/244/reztra/1/4","id":"4","distance":2.43,"controls":12,"results":[]},{"url":"/lt/mvarz/244/reztra/1/D","id":"D","distance":6.82,"controls":14,"results":[]},{"url":"/lt/mvarz/244/reztra/1/P","id":"P","distance":1.63,"controls":8,"results":[]}]} |}] *)

(* let%expect_test "parse_course results" = *)
(*   let filename = *)
(*     "/home/angel/Documents/ocaml/vkd/2025_antakalnis_course_1_2.html" *)
(*   in *)
(*   let page_html = In_channel.read_all filename in *)
(*   let results = parse_course page_html in *)
(*   List.iter results ~f:(fun result -> printf "%s\n" (CourseResult.show result)); *)
(*   [%expect *)
(*     {| *)
(*     { position: 1; number: 31; group: { url = "/lt/mvarz/244/rezgru/V-21A"; group = "V-21A" }; name: Časas Adomas; club: Ąžuolas ok ; time: 30:32; points: 100; pace: 6:54 } *)
(*     { position: 2; number: 343; group: { url = "/lt/mvarz/244/rezgru/V-21A"; group = "V-21A" }; name: Staišiūnas Viktoras; club: Ąžuolas ok ; time: 32:28; points: 95; pace: 7:20 } *)
(*     { position: 3; number: 1; group: { url = "/lt/mvarz/244/rezgru/V-40"; group = "V-40" }; name: Barkauskas Aidas; club: Ąžuolas ok ; time: 33:09; points: 94; pace: 7:30 } *)
(*     { position: 4; number: 104; group: { url = "/lt/mvarz/244/rezgru/V-40"; group = "V-40" }; name: Jauniškis Robertas; club: Apuokas osk ; time: 34:26; points: 91; pace: 7:47 } *)
(*     { position: 5; number: 328; group: { url = "/lt/mvarz/244/rezgru/V-40"; group = "V-40" }; name: Šinkūnas Rimvydas; club: Rudamina ok ; time: 34:55; points: 90; pace: 7:53 } *)
(*     { position: 6; number: 536; group: { url = "/lt/mvarz/244/rezgru/V-21A"; group = "V-21A" }; name: Stupelis Rimvydas; club: Telšė ok ; time: 34:56; points: 90; pace: 7:54 } *)
(*     { position: 7; number: 179; group: { url = "/lt/mvarz/244/rezgru/V-21A"; group = "V-21A" }; name: Užkuraitis Simanas; club: Vilkpėdė ; time: 35:10; points: 90; pace: 7:57 } *)
(*     { position: 8; number: 617; group: { url = "/lt/mvarz/244/rezgru/V-21A"; group = "V-21A" }; name: Iliev Angel; club: Bet koks ; time: 35:28; points: 89; pace: 8:01 } *)
(*     { position: 9; number: 95; group: { url = "/lt/mvarz/244/rezgru/V-18"; group = "V-18" }; name: Montvila Mykolas; club: Sostinės sc ; time: 35:29; points: 89; pace: 8:01 } *)
(*     { position: 10; number: 542; group: { url = "/lt/mvarz/244/rezgru/V-40"; group = "V-40" }; name: Markovas Paulius; club: Perkūnas ok ; time: 35:40; points: 89; pace: 8:04 } *)
(*     { position: 11; number: 1689; group: { url = "/lt/mvarz/244/rezgru/V-21A"; group = "V-21A" }; name: Švedarauskas Simonas; club: Verkiai ; time: 35:44; points: 89; pace: 8:05 } *)
(*     { position: 12; number: 321; group: { url = "/lt/mvarz/244/rezgru/V-50"; group = "V-50" }; name: Jokubauskis Kęstutis; club: Telšė ok ; time: 36:04; points: 88; pace: 8:09 } *)
(*     { position: 13; number: 175; group: { url = "/lt/mvarz/244/rezgru/V-50"; group = "V-50" }; name: Bertašius Rimvydas; club: Klajūnas ok ; time: 37:27; points: 86; pace: 8:28 } *)
(*     { position: 14; number: 45; group: { url = "/lt/mvarz/244/rezgru/V-40"; group = "V-40" }; name: Narvydas Simonas; club: SK Mohikanai ; time: 37:37; points: 85; pace: 8:30 } *)
(*     { position: 15; number: 349; group: { url = "/lt/mvarz/244/rezgru/V-40"; group = "V-40" }; name: Jasinevičius Tomas; club: Baltų lokys ; time: 38:13; points: 84; pace: 8:38 } *)
(*     { position: 16; number: 156; group: { url = "/lt/mvarz/244/rezgru/V-50"; group = "V-50" }; name: Rusakevičius Dainius; club: Horizontai KK ; time: 38:32; points: 84; pace: 8:43 } *)
(*     { position: 17; number: 225; group: { url = "/lt/mvarz/244/rezgru/V-40"; group = "V-40" }; name: Blaževičius Gediminas; club: Vilnius Tech ; time: 39:05; points: 83; pace: 8:50 } *)
(*     { position: 18; number: 168; group: { url = "/lt/mvarz/244/rezgru/M-21A"; group = "M-21A" }; name: Atgalainė Adrija; club: Lėvuo ok ; time: 39:19; points: 82; pace: 8:53 } *)
(*     { position: 19; number: 5; group: { url = "/lt/mvarz/244/rezgru/M-18"; group = "M-18" }; name: Dienytė Margarita; club: Perkūnas ok ; time: 39:55; points: 82; pace: 9:01 } *)
(*     { position: 20; number: 94; group: { url = "/lt/mvarz/244/rezgru/M-40"; group = "M-40" }; name: Brazauskaitė Leokadija; club: Kadipė ; time: 40:20; points: 81; pace: 9:07 } *)
(*     { position: 21; number: 112; group: { url = "/lt/mvarz/244/rezgru/V-50"; group = "V-50" }; name: Rinkevičius Darius; club: Gervės ; time: 40:23; points: 81; pace: 9:08 } *)
(*     { position: 22; number: 151; group: { url = "/lt/mvarz/244/rezgru/V-21B"; group = "V-21B" }; name: Pašuk Sergeij; club: Savas takas ; time: 40:37; points: 81; pace: 9:11 } *)
(*     { position: 23; number: 93; group: { url = "/lt/mvarz/244/rezgru/M-40"; group = "M-40" }; name: Aleksandraitytė Džiuginta; club: Lėvuo ok ; time: 40:39; points: 80; pace: 9:11 } *)
(*     { position: 24; number: 16; group: { url = "/lt/mvarz/244/rezgru/V-40"; group = "V-40" }; name: Petrevičius Aras; club: Fortūna ok ; time: 40:49; points: 80; pace: 9:14 } *)
(*     { position: 25; number: 114; group: { url = "/lt/mvarz/244/rezgru/V-40"; group = "V-40" }; name: Cicėnas Audrius; club: Rudamina ok ; time: 40:59; points: 80; pace: 9:16 } *)
(*     { position: 26; number: 63; group: { url = "/lt/mvarz/244/rezgru/M-40"; group = "M-40" }; name: Auštrienė Giedrė; club: G. A. ; time: 41:34; points: 79; pace: 9:24 } *)
(*     { position: 27; number: 39; group: { url = "/lt/mvarz/244/rezgru/V-40"; group = "V-40" }; name: Lelkaitis Valdas; club: Fortūna ok ; time: 41:40; points: 79; pace: 9:25 } *)
(*     { position: 28; number: 29; group: { url = "/lt/mvarz/244/rezgru/V-60"; group = "V-60" }; name: Stančikas Virginijus; club: Fortūna ok ; time: 41:41; points: 79; pace: 9:25 } *)
(*     { position: 29; number: 191; group: { url = "/lt/mvarz/244/rezgru/V-50"; group = "V-50" }; name: Sriubas Egidijus; club: Horizontai KK ; time: 42:12; points: 78; pace: 9:32 } *)
(*     { position: 30; number: 56; group: { url = "/lt/mvarz/244/rezgru/V-40"; group = "V-40" }; name: Petrilionis Marius; club: Ž999 ; time: 42:24; points: 78; pace: 9:35 } *)
(*     { position: 31; number: 335; group: { url = "/lt/mvarz/244/rezgru/V-40"; group = "V-40" }; name: Radžius Antanas; club: Lėvuo ok ; time: 42:31; points: 78; pace: 9:37 } *)
(*     { position: 32; number: 37; group: { url = "/lt/mvarz/244/rezgru/V-14"; group = "V-14" }; name: Časas Vincentas Petras; club: Perkūnas ok ; time: 43:00; points: 77; pace: 9:43 } *)
(*     { position: 33; number: 99; group: { url = "/lt/mvarz/244/rezgru/V-21A"; group = "V-21A" }; name: Ragauskas Audrius; club: Geno ; time: 43:04; points: 77; pace: 9:44 } *)
(*     { position: 34; number: 35; group: { url = "/lt/mvarz/244/rezgru/V-16"; group = "V-16" }; name: Balčiūnas Vincas; club: Sostinės sc ; time: 44:13; points: 76; pace: 10:00 } *)
(*     { position: 35; number: 1719; group: { url = "/lt/mvarz/244/rezgru/V-21B"; group = "V-21B" }; name: Jurkevičius Antanas; club: Nesunaikinami ; time: 44:35; points: 75; pace: 10:05 } *)
(*     { position: 36; number: 208; group: { url = "/lt/mvarz/244/rezgru/V-40"; group = "V-40" }; name: Navickas Darius; club: AzotoŠventė ; time: 44:37; points: 75; pace: 10:05 } *)
(*     { position: 37; number: 34; group: { url = "/lt/mvarz/244/rezgru/M-18"; group = "M-18" }; name: Balčiūnaitė Barbora; club: Sostinės sc ; time: 44:48; points: 75; pace: 10:08 } *)
(*     { position: 38; number: 88; group: { url = "/lt/mvarz/244/rezgru/V-60"; group = "V-60" }; name: Gavėnas Gintaras; club: Rudamina ok ; time: 45:36; points: 74; pace: 10:19 } *)
(*     { position: 39; number: 231; group: { url = "/lt/mvarz/244/rezgru/M-40"; group = "M-40" }; name: Volungevičienė Judita; club: Lėvuo ok ; time: 45:37; points: 74; pace: 10:19 } *)
(*     { position: 40; number: 49; group: { url = "/lt/mvarz/244/rezgru/V-21A"; group = "V-21A" }; name: Sveikauskas Julius; club: Devhausas ; time: 46:29; points: 73; pace: 10:30 } *)
(*     { position: 41; number: 816; group: { url = "/lt/mvarz/244/rezgru/V-21B"; group = "V-21B" }; name: Boženokas Michailas; club: Kuro aparatūra ; time: 46:54; points: 73; pace: 10:36 } *)
(*     { position: 42; number: 864; group: { url = "/lt/mvarz/244/rezgru/V-40"; group = "V-40" }; name: Leipus Vytautas; club: Peiliukai ; time: 47:16; points: 72; pace: 10:41 } *)
(*     { position: 43; number: 68; group: { url = "/lt/mvarz/244/rezgru/V-40"; group = "V-40" }; name: Saldžiūnas Viktoras; club: Devyni ok ; time: 47:40; points: 72; pace: 10:47 } *)
(*     { position: 44; number: 65; group: { url = "/lt/mvarz/244/rezgru/V-40"; group = "V-40" }; name: Jatkauskas Jonas; club: BGI ; time: 47:43; points: 72; pace: 10:47 } *)
(*     { position: 45; number: 28; group: { url = "/lt/mvarz/244/rezgru/V-60"; group = "V-60" }; name: Mejeras Gintaras; club: Rudamina ok ; time: 47:46; points: 72; pace: 10:48 } *)
(*     { position: 46; number: 90; group: { url = "/lt/mvarz/244/rezgru/V-21A"; group = "V-21A" }; name: Ivanovas Edgaras; club: Gaša ; time: 49:23; points: 70; pace: 11:10 } *)
(*     { position: 47; number: 530; group: { url = "/lt/mvarz/244/rezgru/M-21A"; group = "M-21A" }; name: Gembutaitė Sandra; club: Versmė ok ; time: 49:51; points: 70; pace: 11:16 } *)
(*     { position: 48; number: 323; group: { url = "/lt/mvarz/244/rezgru/V-50"; group = "V-50" }; name: Kulevičius Donaldas; club: Ąžuolas ok ; time: 50:53; points: 69; pace: 11:30 } *)
(*     { position: 49; number: 319; group: { url = "/lt/mvarz/244/rezgru/V-60"; group = "V-60" }; name: Kubaitis Arūnas; club: Ąžuolas ok ; time: 51:06; points: 68; pace: 11:33 } *)
(*     { position: 50; number: 184; group: { url = "/lt/mvarz/244/rezgru/M-21A"; group = "M-21A" }; name: Kanapinskaitė Viltė; club: Lėvuo ok ; time: 51:45; points: 68; pace: 11:42 } *)
(*     { position: 51; number: 219; group: { url = "/lt/mvarz/244/rezgru/V-16"; group = "V-16" }; name: Šapranauskas Jonas; club: Perkūnas ok ; time: 51:50; points: 68; pace: 11:43 } *)
(*     { position: 52; number: 54; group: { url = "/lt/mvarz/244/rezgru/V-40"; group = "V-40" }; name: Kušeliauskas Kęstutis; club: Perkūnas ok ; time: 52:23; points: 67; pace: 11:51 } *)
(*     { position: 53; number: 50; group: { url = "/lt/mvarz/244/rezgru/V-50"; group = "V-50" }; name: Sabataitis Kristijonas; club: A. V. ; time: 52:41; points: 67; pace: 11:55 } *)
(*     { position: 54; number: 899; group: { url = "/lt/mvarz/244/rezgru/V-40"; group = "V-40" }; name: Pranaitis Tomas; club: Ronis ; time: 52:42; points: 67; pace: 11:55 } *)
(*     { position: 55; number: 518; group: { url = "/lt/mvarz/244/rezgru/M-21B"; group = "M-21B" }; name: Staškevičiūtė Raminta; club: Pavasaris ; time: 52:43; points: 67; pace: 11:55 } *)
(*     { position: 56; number: 778; group: { url = "/lt/mvarz/244/rezgru/V-21B"; group = "V-21B" }; name: Jadenkus Domantas; club: Fortūna ok ; time: 52:49; points: 67; pace: 11:56 } *)
(*     { position: 57; number: 1561; group: { url = "/lt/mvarz/244/rezgru/M-21B"; group = "M-21B" }; name: Garbaliauskaitė Elena; club: Pavasaris ; time: 52:53; points: 67; pace: 11:57 } *)
(*     { position: 58; number: 82; group: { url = "/lt/mvarz/244/rezgru/V-40"; group = "V-40" }; name: Lukoševičius Mantas; club: Arboro ok ; time: 53:09; points: 67; pace: 12:01 } *)
(*     { position: 59; number: 261; group: { url = "/lt/mvarz/244/rezgru/M-16"; group = "M-16" }; name: Pigagaitė Aistė; club: Sostinės sc ; time: 53:48; points: 66; pace: 12:10 } *)
(*     { position: 60; number: 1796; group: { url = "/lt/mvarz/244/rezgru/V-21B"; group = "V-21B" }; name: Varonenka Juozas; club: Digital.ai ; time: 54:00; points: 66; pace: 12:13 } *)
(*     { position: 61; number: 762; group: { url = "/lt/mvarz/244/rezgru/M-21A"; group = "M-21A" }; name: Malcaitė Eglė; club: Run forest run ; time: 54:55; points: 65; pace: 12:25 } *)
(*     { position: 62; number: 295; group: { url = "/lt/mvarz/244/rezgru/V-60"; group = "V-60" }; name: Budginas Vytas; club: Rudamina ok ; time: 56:07; points: 64; pace: 12:41 } *)
(*     { position: 63; number: 41; group: { url = "/lt/mvarz/244/rezgru/M-21A"; group = "M-21A" }; name: Rimydytė Ona; club: Klajūnas ok ; time: 57:07; points: 63; pace: 12:55 } *)
(*     { position: 64; number: 1697; group: { url = "/lt/mvarz/244/rezgru/V-21B"; group = "V-21B" }; name: Neniškis Algirdas; club: Žvėrynėlis ; time: 57:16; points: 63; pace: 12:57 } *)
(*     { position: 65; number: 251; group: { url = "/lt/mvarz/244/rezgru/M-16"; group = "M-16" }; name: Šinkūnaitė Viltė; club: Perkūnas ok ; time: 57:17; points: 63; pace: 12:57 } *)
(*     { position: 66; number: 855; group: { url = "/lt/mvarz/244/rezgru/V-40"; group = "V-40" }; name: Bukauskas Virginijus; club: Erkūnas ; time: 57:24; points: 63; pace: 12:59 } *)
(*     { position: 67; number: 316; group: { url = "/lt/mvarz/244/rezgru/V-70"; group = "V-70" }; name: Dūda Kostas; club: Rudamina ok ; time: 57:40; points: 63; pace: 13:02 } *)
(*     { position: 68; number: 183; group: { url = "/lt/mvarz/244/rezgru/V-60"; group = "V-60" }; name: Abramenkov Sergeij; club: Dinamo ; time: 57:57; points: 63; pace: 13:06 } *)
(*     { position: 69; number: 665; group: { url = "/lt/mvarz/244/rezgru/M-21B"; group = "M-21B" }; name: Arlauskienė Edita; club: Nord Security ; time: 59:15; points: 62; pace: 13:24 } *)
(*     { position: 70; number: 666; group: { url = "/lt/mvarz/244/rezgru/V-21B"; group = "V-21B" }; name: Arlauskas Jonas; club: Swedbank ; time: 59:17; points: 62; pace: 13:24 } *)
(*     { position: 71; number: 516; group: { url = "/lt/mvarz/244/rezgru/V-21B"; group = "V-21B" }; name: Ašmonas Nojus; club: LKA ; time: 1:01:52; points: 60; pace: 13:59 } *)
(*     { position: 72; number: 364; group: { url = "/lt/mvarz/244/rezgru/M-21B"; group = "M-21B" }; name: Mačanaitė Kristina; club: BA ; time: 1:02:07; points: 60; pace: 14:03 } *)
(*     { position: 73; number: 102; group: { url = "/lt/mvarz/244/rezgru/M-40"; group = "M-40" }; name: Tarozaitė Birutė; club: Erkūnas ; time: 1:02:56; points: 60; pace: 14:14 } *)
(*     { position: 74; number: 210; group: { url = "/lt/mvarz/244/rezgru/M-40"; group = "M-40" }; name: Trečiokaitė Vilija; club: Erkūnas ; time: 1:03:02; points: 60; pace: 14:15 } *)
(*     { position: 75; number: 136; group: { url = "/lt/mvarz/244/rezgru/V-60"; group = "V-60" }; name: Žukauskas Artūras; club: Klajūnas ok ; time: 1:14:46; points: 54; pace: 16:54 } *)
(*     { position: ; number: 368; group: { url = "/lt/mvarz/244/rezgru/V-50"; group = "V-50" }; name: Kananavičius Robertas; club: VU ŽK ; time: dsq; points: 10; pace:  } *)
(*     { position: ; number: 64; group: { url = "/lt/mvarz/244/rezgru/V-40"; group = "V-40" }; name: Ričkus Arnoldas; club: Nieko ; time: dsq; points: 10; pace:  } *)
(*            |}]; *)
(*   printf "%s" *)
(*     (Yojson.Safe.to_string @@ CourseResult.yojson_of_t @@ List.hd_exn results); *)
(*   [%expect *)
(*     {| {"position":1,"number":31,"group":{"url":"/lt/mvarz/244/rezgru/V-21A","group":"V-21A"},"name":"Časas Adomas","club":"Ąžuolas ok ","time":"30:32","points":100,"pace":"6:54"} |}] *)

(* let%expect_test "course_stats" = *)
(*   let results = *)
(*     Yojson.Safe.from_file *)
(*       "/home/angel/Documents/ocaml/vkd/244_event_1_results.json" *)
(*     (* "/home/angel/Documents/ocaml/vkd/league244_event2.json" *) *)
(*     |> OverallResults.t_of_yojson *)
(*   in *)
(**)
(*   let stats = CourseStats.of_results results in *)
(*   printf "%s" (CourseStats.yojson_of_t stats |> Yojson.Safe.to_string); *)
(*   [%expect *)
(*     {| {"num_men":56,"num_women":19,"tilt_overall":32,"tilt_men":31,"tilt_women":33,"mistake_time_overall":238,"mistake_time_men":232,"mistake_time_women":257,"blunder_perc_overall":4,"blunder_perc_men":4,"blunder_perc_women":3,"big_mistake_perc_overall":35,"big_mistake_perc_men":33,"big_mistake_perc_women":40,"small_mistake_perc_overall":60,"small_mistake_perc_men":62,"small_mistake_perc_women":56,"most_tricky_overall":16,"most_tricky_men":16,"most_tricky_women":15,"avg_time_for_mistake_overall":36,"avg_time_for_mistake_men":36,"avg_time_for_mistake_women":35,"avg_mistake_num_overall":6,"avg_mistake_num_men":6,"avg_mistake_num_women":7} |}] *)
