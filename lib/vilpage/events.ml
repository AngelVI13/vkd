open Core

let base_url = "https://vilniausketvirtadieniai.lt"

let fetch_page url =
  let res = Ezcurl.get ~url () in
  let out = match res with Ok c -> c.body | Error (_, s) -> failwith s in
  out

module EventInfo = struct
  type t = {
    date : string;
    thumbnail : string;
    thumbnail_src : string;
    map_info : string;
    map_links : string list;
  }
end

let parse_events_page ~(year : int) page_html =
  let open Soup in
  let soup = parse page_html in
  let events =
    soup $ ".PAGE__body" $$ ".stage" |> to_list
    |> List.map ~f:(fun stage ->
           let event_date =
             stage $ ".map" $ ".title" $ ".date" |> R.leaf_text
           in
           let img_src = stage $ ".map" $ "img" |> R.attribute "src" in
           (* printf "%s\n\n" (fetch_page img_src); *)
           (* Out_channel.write_all *)
           (*   (sprintf "/home/angel/Documents/ocaml/vkd/%s_%d.png" event_date *)
           (*      year) *)
           (*   ~data:(fetch_page img_src); *)
           let map_info = stage $ ".map-info" $ ".stage-info" |> R.leaf_text in
           let links =
             stage $ ".stage-details" $$ "a" |> to_list
             |> List.map ~f:(R.attribute "href")
             |> List.filter ~f:(String.is_substring ~substring:"trails.lt")
           in
           printf "%s -> %s\n%s\n%s\n\n" event_date img_src map_info
             (String.concat ~sep:", " links))
    (* TODO: stage-details - parse links to results and everything. Check what
       happens in case if those are missing ?? *)
  in

  let _ = (soup, events, year) in
  ()

let download_events ~(year : int) =
  (* let url = sprintf "%s/%d" base_url year in *)
  (* let page = fetch_page url in *)
  (* Out_channel.write_all *)
  (*   (sprintf "/home/angel/Documents/ocaml/vkd/vil_%d.html" year) *)
  (*   ~data:page; *)
  let page =
    In_channel.read_all
      (sprintf "/home/angel/Documents/ocaml/vkd/vil_%d.html" year)
  in
  parse_events_page ~year page;
  ()

let download_map ~url =
  let _ = url in
  (* https://trails.lt/events/vk-antakalnis-2026/#1%202 *)
  (* TODO: to download the map simply make request to the url 
    https://trails.lt/events/senasalis-2025/settings.json?v=1774895976042
    where the data after ?v= is the '?v=' + Date.now()) (from JS) 
    The JSON response contains the map url for downloading and all the controls ontop of it 
    but then you have to draw the course based on the provided data in the response
    *)
  ()

let%expect_test "download_events" =
  download_events ~year:2026;
  [%expect
    {|
    2026.03.26 -> https://vilniausketvirtadieniai.lt/2026/antakalnis/screenshot-2026-03-24-at-16.13.15.png/image
    Žemėlapis:
    Skirmantas Ramoška
    2022 m.
    M 1:5000, H 2.5
    https://trails.lt/events/vk-antakalnis-2026/#1%202, https://trails.lt/events/vk-antakalnis-2026/#3, https://trails.lt/events/vk-antakalnis-2026/#4, https://trails.lt/events/vk-antakalnis-2026/#5, https://trails.lt/events/vk-antakalnis-2026/#D

    2026.03.22 -> https://vilniausketvirtadieniai.lt/2026/viluniskes/screenshot-2026-03-20-at-10.06.42.png/image
    Žemėlapis:
    Rimvydas Kutka
    2019 m.
    M 1:10000, H 2.5
    https://trails.lt/events/viluniskes-2026/#1, https://trails.lt/events/viluniskes-2026/#2, https://trails.lt/events/viluniskes-2026/#3, https://trails.lt/events/viluniskes-2026/#4, https://trails.lt/events/viluniskes-2026/#5

    2026.03.19 -> https://vilniausketvirtadieniai.lt/2026/skersine/screenshot-2026-03-16-at-11.16.42.png/image
    Žemėlapis:
    Skirmantas Ramoška
    2021 m.
    M 1:7500, H 2.5
    https://trails.lt/events/skersine-2026/#1, https://trails.lt/events/skersine-2026/#2, https://trails.lt/events/skersine-2026/#3, https://trails.lt/events/skersine-2026/#4, https://trails.lt/events/skersine-2026/#4

    2026.02.06 -> https://vilniausketvirtadieniai.lt/2026/vingis-iof-konferencija/screenshot-2026-02-05-at-12.14.15.png/image
    Žemėlapis:
    Skirmantas Ramoška
    2001 - 2026 m.
    M 1:7500, H 2.5
    https://trails.lt/events/vingis-iof-2026/#1, https://trails.lt/events/vingis-iof-2026/#2, https://trails.lt/events/vingis-iof-2026/#3 |}]
