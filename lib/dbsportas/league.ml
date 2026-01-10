open Core

let base_url = "https://dbsportas.lt"

(* https://dbsportas.lt/lt/mvarz/244 *)
let fetch_page ~(name : string) url =
  let res = Ezcurl.get ~url () in
  let out = match res with Ok c -> c.body | Error (_, s) -> failwith s in
  Out_channel.write_all
    (sprintf "/home/angel/Documents/ocaml/vkd/%s.html" name)
    ~data:out

module Course = struct
  type t = { url : string; id : string; distance : float; controls : int }
  [@@deriving show { with_path = false }]

  let make ~url ~id ~distance ~controls = { url; id; distance; controls }
end

module EventResults = struct
  type t = { url : string; courses : Course.t list }
  [@@deriving show { with_path = false }]

  let make ~url ~courses = { url; courses }
end

module LeagueEvent = struct
  type t = {
    nr : int;
    date : string;
    location : string;
    results : EventResults.t option;
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
      r.nr r.date r.location results_url

  let show r = Format.asprintf "%a" pp r

  let of_td_list ~results td_list =
    assert (List.length td_list = 3);
    {
      nr = Int.of_string @@ List.nth_exn td_list 0;
      date = List.nth_exn td_list 1;
      location = List.nth_exn td_list 2;
      results;
    }
end

module League = struct
  type t = { url : string; events : LeagueEvent.t list }
  [@@deriving show { with_path = false }]

  let of_events events = { url = ""; events }
  let with_url l url = { l with url }
end

let parse_league_page page_html_chan =
  let open Soup in
  let soup = read_channel page_html_chan |> parse in
  let rows = soup $ ".w3-table" $$ "tr" |> to_list in
  let events =
    rows
    |> List.fold ~init:[] ~f:(fun acc tr ->
           let results =
             tr $? ".w3-text-green"
             |> Option.bind ~f:(fun a ->
                    let url = R.attribute "href" a in
                    (* TODO: add courses here *)
                    Some (EventResults.make ~url ~courses:[]))
           in
           let tds = tr $$ "td" |> to_list |> List.map ~f:R.leaf_text in
           match tds with
           | [] -> acc
           | _ ->
               let event = LeagueEvent.of_td_list ~results tds in
               event :: acc)
  in
  League.of_events (List.rev events)

let parse_event page_html_chan =
  let open Soup in
  let soup = read_channel page_html_chan |> parse in
  (* skip header row *)
  let rows = soup $ ".w3-table" $$ "tr" |> to_list |> List.tl_exn in
  let courses =
    rows
    |> List.fold ~init:[] ~f:(fun acc tr ->
           let course_col = tr $ ".w3-text-green" in
           let course_results_url = course_col |> R.attribute "href" in
           let course_id = course_col |> R.leaf_text |> String.strip in

           let ids =
             String.split course_id ~on:',' |> List.map ~f:String.strip
           in

           let course_parameters =
             tr $$ "td" |> to_list |> List.tl_exn |> List.map ~f:R.leaf_text
             |> List.hd_exn
           in
           let course_parameters =
             course_parameters
             (* example course parameters: '4.420 km 21 KP' *)
             |> String.substr_replace_all ~pattern:"km " ~with_:""
             |> String.substr_replace_all ~pattern:"KP" ~with_:""
             |> String.strip
           in
           let course_parameters = String.split course_parameters ~on:' ' in
           let distance, controls =
             match course_parameters with
             | [ dist; pts ] -> (Float.of_string dist, Int.of_string pts)
             | _ -> assert false
           in

           let courses =
             List.map ids ~f:(fun id ->
                 Course.make ~url:course_results_url ~id ~distance ~controls)
           in
           acc @ courses)
  in
  courses

let%expect_test "parse_league_page finished league" =
  let filename = "/home/angel/Documents/ocaml/vkd/league.html" in
  let page_html = In_channel.create filename in
  let league = parse_league_page page_html in
  printf "%s" (League.show league);
  [%expect
    {|
    { url = "";
      events =
      [{ nr: 1; date: 2025-03-27; location: Antakalnis ; results_url: Some( /lt/mvarz/244/reztur/1 ) };
        { nr: 2; date: 2025-04-03; location: Belmontas ; results_url: Some( /lt/mvarz/244/reztur/2 ) };
        { nr: 3; date: 2025-04-10; location: Bukčiai ; results_url: Some( /lt/mvarz/244/reztur/3 ) };
        { nr: 4; date: 2025-04-17; location: Dvarčionys ; results_url: Some( /lt/mvarz/244/reztur/4 ) };
        { nr: 5; date: 2025-04-24; location: Skersinė ; results_url: Some( /lt/mvarz/244/reztur/5 ) };
        { nr: 6; date: 2025-05-01; location: Kaminai (Apuoko lyga) ; results_url: Some( /lt/mvarz/244/reztur/6 ) };
        { nr: 7; date: 2025-05-08; location: Ozas ; results_url: Some( /lt/mvarz/244/reztur/7 ) };
        { nr: 8; date: 2025-05-15; location: Šnipiškės ; results_url: Some( /lt/mvarz/244/reztur/8 ) };
        { nr: 9; date: 2025-05-22; location: Karoliniškės ; results_url: Some( /lt/mvarz/244/reztur/9 ) };
        { nr: 10; date: 2025-05-29; location: Žirmūnai ; results_url: Some( /lt/mvarz/244/reztur/10 ) };
        { nr: 11; date: 2025-06-05; location: Jeruzalė ; results_url: Some( /lt/mvarz/244/reztur/11 ) };
        { nr: 12; date: 2025-06-12; location: Šveicarija ; results_url: Some( /lt/mvarz/244/reztur/12 ) };
        { nr: 13; date: 2025-06-19; location: Karačiūnai ; results_url: Some( /lt/mvarz/244/reztur/13 ) };
        { nr: 14; date: 2025-06-26; location: Jomantas ; results_url: Some( /lt/mvarz/244/reztur/14 ) };
        { nr: 15; date: 2025-07-03; location: Smėlynė ; results_url: Some( /lt/mvarz/244/reztur/15 ) };
        { nr: 16; date: 2025-07-10; location: Vismalai ; results_url: Some( /lt/mvarz/244/reztur/16 ) };
        { nr: 17; date: 2025-07-17; location: Verkiai ; results_url: Some( /lt/mvarz/244/reztur/17 ) };
        { nr: 18; date: 2025-07-24; location: Šilėnai ; results_url: Some( /lt/mvarz/244/reztur/18 ) };
        { nr: 19; date: 2025-07-31; location: Vismaliukai ; results_url: Some( /lt/mvarz/244/reztur/19 ) };
        { nr: 20; date: 2025-08-07; location: Aukštagiris ; results_url: Some( /lt/mvarz/244/reztur/20 ) };
        { nr: 21; date: 2025-08-14; location: Balžis ; results_url: Some( /lt/mvarz/244/reztur/21 ) };
        { nr: 22; date: 2025-08-21; location: Strielčiukai ; results_url: Some( /lt/mvarz/244/reztur/22 ) };
        { nr: 23; date: 2025-08-28; location: Gulbinėliai ; results_url: Some( /lt/mvarz/244/reztur/23 ) };
        { nr: 24; date: 2025-09-04; location: Kalvarijos ; results_url: Some( /lt/mvarz/244/reztur/24 ) };
        { nr: 25; date: 2025-09-11; location: Visoriai ; results_url: Some( /lt/mvarz/244/reztur/25 ) };
        { nr: 26; date: 2025-09-18; location: Bajorai ; results_url: Some( /lt/mvarz/244/reztur/26 ) };
        { nr: 27; date: 2025-09-25; location: Šeškinė ; results_url: Some( /lt/mvarz/244/reztur/27 ) };
        { nr: 28; date: 2025-10-02; location: Pilaitė ; results_url: Some( /lt/mvarz/244/reztur/28 ) };
        { nr: 29; date: 2025-10-09; location: Gudeliai ; results_url: Some( /lt/mvarz/244/reztur/29 ) };
        { nr: 30; date: 2025-10-16; location: Vingis ; results_url: Some( /lt/mvarz/244/reztur/30 ) }
        ]
      } |}]

let%expect_test "parse_league_page upcomming league" =
  let filename = "/home/angel/Documents/ocaml/vkd/2026_league.html" in
  let page_html = In_channel.create filename in
  let league = parse_league_page page_html in
  printf "%s" (League.show league);
  [%expect
    {|
    { url = "";
      events =
      [{ nr: 1; date: 2026-03-26; location: Antakalnis ; results_url: None };
        { nr: 2; date: 2026-04-02; location: Bukčiai ; results_url: None };
        { nr: 3; date: 2026-04-09; location: Pilaitė ; results_url: None };
        { nr: 4; date: 2026-04-16; location: Skersinė ; results_url: None };
        { nr: 5; date: 2026-04-23; location: Gudeliai ; results_url: None };
        { nr: 6; date: 2026-04-30; location: Belmontas ; results_url: None };
        { nr: 7; date: 2026-05-07; location: Kaminai ; results_url: None };
        { nr: 8; date: 2026-05-14; location: Dvarčionys ; results_url: None };
        { nr: 9; date: 2026-05-21; location: Karoliniškės ; results_url: None };
        { nr: 10; date: 2026-05-28; location: Sapiegynė ; results_url: None };
        { nr: 11; date: 2026-06-04; location: Žirmūnai ; results_url: None };
        { nr: 12; date: 2026-06-11; location: Ozas ; results_url: None };
        { nr: 13; date: 2026-06-18; location: Smėlynė ; results_url: None };
        { nr: 14; date: 2026-06-25; location: Karačiūnai ; results_url: None };
        { nr: 15; date: 2026-07-02; location: Jomantas ; results_url: None };
        { nr: 16; date: 2026-07-09; location: Balžis ; results_url: None };
        { nr: 17; date: 2026-07-16; location: Verkiai ; results_url: None };
        { nr: 18; date: 2026-07-23; location: Šilėnai ; results_url: None };
        { nr: 19; date: 2026-07-30; location: Vismalai ; results_url: None };
        { nr: 20; date: 2026-08-06; location: Aukštagiris ; results_url: None };
        { nr: 21; date: 2026-08-13; location: Vismaliukai ; results_url: None };
        { nr: 22; date: 2026-08-20; location: Strielčiukai ; results_url: None };
        { nr: 23; date: 2026-08-27; location: Gulbinėliai ; results_url: None };
        { nr: 24; date: 2026-09-03; location: Kalvarijos ; results_url: None };
        { nr: 25; date: 2026-09-10; location: Lazdynai ; results_url: None };
        { nr: 26; date: 2026-09-17; location: Visoriai ; results_url: None };
        { nr: 27; date: 2026-09-24; location: Bajorai ; results_url: None };
        { nr: 28; date: 2026-10-01; location: Valakampiai ; results_url: None };
        { nr: 29; date: 2026-10-08; location: Šeškinė ; results_url: None };
        { nr: 30; date: 2026-10-15; location: Vingis ; results_url: None }]
      }
           |}]

let%expect_test "parse_event grouped courses" =
  let filename = "/home/angel/Documents/ocaml/vkd/2025_antakalnis.html" in
  let page_html = In_channel.create filename in
  let courses = parse_event page_html in
  List.iter courses ~f:(fun course -> printf "%s\n" (Course.show course));
  [%expect
    {|
    { url = "/lt/mvarz/244/reztra/1/1%2C2"; id = "1"; distance = 4.42;
      controls = 21 }
    { url = "/lt/mvarz/244/reztra/1/1%2C2"; id = "2"; distance = 4.42;
      controls = 21 }
    { url = "/lt/mvarz/244/reztra/1/3"; id = "3"; distance = 3.44; controls = 16
      }
    { url = "/lt/mvarz/244/reztra/1/4"; id = "4"; distance = 2.43; controls = 12
      }
    { url = "/lt/mvarz/244/reztra/1/D"; id = "D"; distance = 6.82; controls = 14
      }
    { url = "/lt/mvarz/244/reztra/1/P"; id = "P"; distance = 1.63; controls = 8 }
    |}]
