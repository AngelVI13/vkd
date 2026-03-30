open Core

let base_url = "https://vilniausketvirtadieniai.lt"

let fetch_page url =
  let res = Ezcurl.get ~url () in
  let out = match res with Ok c -> c.body | Error (_, s) -> failwith s in
  out

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
           let map_info = stage $ ".map-info" $ ".stage-info" |> R.leaf_text in
           printf "%s -> %s\n%s\n\n" event_date img_src map_info)
    (* TODO: stage-details - parse links to results and everything. Check what
       happens in case if those are missing ?? *)
  in

  (* let rows = parse_table_rows soup in *)
  (* (* TODO: remove this after testing *) *)
  (* (* let rows = List.hd_exn rows in *) *)
  (* (* let rows = List.drop rows 4 in *) *)
  (* let rows = List.hd_exn rows in *)
  (* let rows = [ rows ] in *)
  (* (* let rows = List.take rows 5 in *) *)
  (* let events = *)
  (*   rows *)
  (*   |> List.fold ~init:[] ~f:(fun acc tr -> *)
  (*          let tds = *)
  (*            tr $$ "td" |> to_list |> List.map ~f:R.leaf_text *)
  (*            |> List.map ~f:strip *)
  (*          in *)
  (*          let event_nr = *)
  (*            Option.bind (List.nth tds 0) ~f:(fun nr -> *)
  (*                match Int.of_string with exception _ -> None | _ -> Some nr) *)
  (*          in *)
  (*          let results = *)
  (*            tr $? ".w3-text-green" *)
  (*            |> Option.bind ~f:(fun a -> *)
  (*                   let url = R.attribute "href" a in *)
  (*                   let url = sprintf "%s%s" base_url url in *)
  (*                   printf "Downloading event page: %s\n" url; *)
  (*                   let results_html = fetch_page url in *)
  (*                   Time_ns_unix.pause (Time_ns.Span.create ~ms:1000 ()); *)
  (*                   let event_nr = Option.value_exn event_nr in *)
  (*                   let courses = *)
  (*                     parse_event ~league_id ~event_nr results_html *)
  (*                   in *)
  (*                   Some (EventResults.Fields.create ~url ~courses)) *)
  (*          in *)
  (*          match tds with *)
  (*          | [] -> acc *)
  (*          | _ -> *)
  (*              let event = LeagueEvent.of_td_list ~results tds in *)
  (*              LeagueEvent.save_to_file ~league_id event; *)
  (*              event :: acc) *)
  (* in *)
  (* List.rev events *)
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

let%expect_test "download_events" =
  download_events ~year:2026;
  [%expect {|
    2026.03.26 -> https://vilniausketvirtadieniai.lt/2026/antakalnis/screenshot-2026-03-24-at-16.13.15.png/image
    Žemėlapis:
    Skirmantas Ramoška
    2022 m.
    M 1:5000, H 2.5

    2026.03.22 -> https://vilniausketvirtadieniai.lt/2026/viluniskes/screenshot-2026-03-20-at-10.06.42.png/image
    Žemėlapis:
    Rimvydas Kutka
    2019 m.
    M 1:10000, H 2.5

    2026.03.19 -> https://vilniausketvirtadieniai.lt/2026/skersine/screenshot-2026-03-16-at-11.16.42.png/image
    Žemėlapis:
    Skirmantas Ramoška
    2021 m.
    M 1:7500, H 2.5

    2026.02.06 -> https://vilniausketvirtadieniai.lt/2026/vingis-iof-konferencija/screenshot-2026-02-05-at-12.14.15.png/image
    Žemėlapis:
    Skirmantas Ramoška
    2001 - 2026 m.
    M 1:7500, H 2.5 |}]
