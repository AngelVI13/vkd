open Core

let base_url = "https://vilniausketvirtadieniai.lt"

let fetch_page url =
  let res = Ezcurl.get ~url () in
  let out = match res with Ok c -> c.body | Error (_, s) -> failwith s in
  out

let now_ms () =
  let ns = Time_ns_unix.now () |> Time_ns_unix.to_int_ns_since_epoch in
  ns / 1_000_000

let later_ms ~(days : int) =
  let ns = Time_ns_unix.now () in
  let ns = Time_ns_unix.add ns (Time_ns_unix.Span.of_int_day days) in
  let ns = ns |> Time_ns_unix.to_int_ns_since_epoch in
  ns / 1_000_000

let google_maps_url lat lng =
  (* NOTE: to build gmaps link from lat,long coords - https://www.google.com/maps?q=54.722195,25.315314 *)
  sprintf "https://www.google.com/maps?q=%.10f,%.10f" lat lng

let download_map ~event_url =
  let timestamp = now_ms () in
  let full_url = sprintf "%s/settings.json?v=%d" event_url timestamp in
  let page = fetch_page full_url in
  let json = Yojson.Safe.from_string page in
  Trails.Settings.t_of_yojson json

let download_and_encode_img ~url =
  fetch_page url |> Base64.encode_exn ~pad:false

module MapSettings = struct
  type t = {
    key : string;
    title : string;
    (* NOTE: map data here is already base64 encoded *)
    default_map_src : string;
    default_map : string; [@opaque]
    bike_map_src : string option;
    bike_map : string option; [@opaque]
    location_lat : float;
    location_lon : float;
  }
  [@@deriving show]

  let t_of_Settings ~event_url (settings : Trails.Settings.t) =
    let default_map_src = sprintf "%s/%s" event_url settings.maps.default.url in
    let default_map = download_and_encode_img ~url:default_map_src in
    let bike_map_src, bike_map =
      match settings.maps.d with
      | None -> (None, None)
      | Some m ->
          let map_src = sprintf "%s/%s" event_url m.url in
          let map = download_and_encode_img ~url:map_src in
          (Some map_src, Some map)
    in
    let location_lat, location_lon =
      match settings.map_settings.controls.finish_loc with
      | [ lat; lon ] -> (lat, lon)
      | _ ->
          failwith
            (sprintf "failed to parse finish location for event: %s" event_url)
    in
    {
      key = settings.key;
      title = settings.title;
      default_map_src;
      default_map;
      bike_map_src;
      bike_map;
      location_lat;
      location_lon;
    }
end

module EventInfo = struct
  type t = {
    date : Time_ns_unix.t;
    thumbnail : string; [@opaque]
    thumbnail_src : string;
    event_link : string;
    location : string;
    map_info : string;
    map_links : string list;
    map_settings : MapSettings.t option;
  }
  [@@deriving fields, show]
end

let parse_date (date : string) =
  (* date format: 2026.04.02 *)
  Time_ns_unix.parse ~fmt:"%Y.%m.%d" ~zone:Timezone.utc date

let parse_events_page page_html =
  let open Soup in
  let soup = parse page_html in
  let events =
    soup $ ".PAGE__body" $$ ".stage" |> to_list
    |> List.map ~f:(fun stage ->
           let event_date =
             stage $ ".map" $ ".title" $ ".date" |> R.leaf_text |> parse_date
           in
           let location, event_link =
             stage $ ".stage-place" $ "a" |> fun n ->
             (R.leaf_text n, R.attribute "href" n)
           in
           let img_src = stage $ ".map" $ "img" |> R.attribute "src" in
           let img_data = download_and_encode_img ~url:img_src in
           let map_info = stage $ ".map-info" $ ".stage-info" |> R.leaf_text in
           let map_links =
             stage $ ".stage-details" $$ "a" |> to_list
             |> List.map ~f:(R.attribute "href")
             |> List.filter ~f:(String.is_substring ~substring:"trails.lt")
           in
           let map_settings =
             match map_links with
             | [] -> None
             | _ ->
                 let url = List.hd_exn map_links in
                 let last_slash = String.rindex_exn url '/' in
                 let event_url =
                   String.drop_suffix url (String.length url - last_slash)
                 in
                 let settings = download_map ~event_url in
                 Some (MapSettings.t_of_Settings ~event_url settings)
           in
           EventInfo.Fields.create ~date:event_date ~thumbnail_src:img_src
             ~thumbnail:img_data ~location ~event_link ~map_info ~map_links
             ~map_settings)
  in
  events

let download_events ~(year : int) =
  let url = sprintf "%s/%d" base_url year in
  let page = fetch_page url in
  parse_events_page page

(* let%expect_test "download_events" = *)
(*   let events = download_events ~year:2026 in *)
(*   let ev = List.nth_exn events 1 in *)
(*   printf "%s" (EventInfo.show ev); *)
(*   [%expect *)
(*     {| *)
(*     { Events.EventInfo.date = 2026-03-26 02:00:00.000000000+02:00; *)
(*       thumbnail = <opaque>; *)
(*       thumbnail_src = *)
(*       "https://vilniausketvirtadieniai.lt/2026/antakalnis/screenshot-2026-03-24-at-16.13.15.png/image"; *)
(*       event_link = "https://vilniausketvirtadieniai.lt/2026/antakalnis/"; *)
(*       location = "Antakalnis"; *)
(*       map_info = *)
(*       "\197\189em\196\151lapis:\nSkirmantas Ramo\197\161ka\n2022 m.\nM 1:5000, H 2.5"; *)
(*       map_links = *)
(*       ["https://trails.lt/events/vk-antakalnis-2026/#1%202"; *)
(*         "https://trails.lt/events/vk-antakalnis-2026/#3"; *)
(*         "https://trails.lt/events/vk-antakalnis-2026/#4"; *)
(*         "https://trails.lt/events/vk-antakalnis-2026/#5"; *)
(*         "https://trails.lt/events/vk-antakalnis-2026/#D"]; *)
(*       map_settings = *)
(*       (Some { Events.MapSettings.key = "vk-antakalnis-2026"; *)
(*               title = "VK Antakalnis 2026"; *)
(*               default_map_src = *)
(*               "https://trails.lt/events/vk-antakalnis-2026/vk-antakalnis-2026-default.gif"; *)
(*               default_map = <opaque>; *)
(*               bike_map_src = *)
(*               (Some "https://trails.lt/events/vk-antakalnis-2026/vk-antakalnis-2026-d.gif"); *)
(*               bike_map = <opaque>; location_lat = 54.722195; *)
(*               location_lon = 25.315314 }) *)
(*       } |}] *)

let%expect_test "parse_map_settings" =
  let json =
    Yojson.Safe.from_file "/home/angel/Documents/ocaml/vkd/map_settings.json"
  in
  let settings = Trails.Settings.t_of_yojson json in

  printf "%s" (Trails.Settings.yojson_of_t settings |> Yojson.Safe.to_string);

  [%expect
    {|
    {"success":true,"key":"vk-antakalnis-2026","title":"VK Antakalnis 2026","map_settings":{"controls":{"F":[54.722195,25.315314]}},"maps":{"D":{"url":"vk-antakalnis-2026-d.gif"},"default":{"url":"vk-antakalnis-2026-default.gif"}}} |}]
