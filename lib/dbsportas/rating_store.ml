open Core

let runner_hash ~id ~course = sprintf "%d__%s" id course

module History = struct
  type t = {
    position : int option list;
    league_id : string list;
    event_nr : int list;
    event_loc : string list;
    rating : float list;
    rating_diff : float list;
    rd : float list;
    rd_diff : float list;
    vol : float list;
    vol_diff : float list;
  }
  [@@deriving show, fields]

  let empty () =
    {
      position = [];
      league_id = [];
      event_nr = [];
      event_loc = [];
      rating = [];
      rating_diff = [];
      rd = [];
      rd_diff = [];
      vol = [];
      vol_diff = [];
    }

  let add t ~position ~league_id ~event_nr ~event_loc ~rating ~rd ~vol =
    let rating_diff =
      match List.last t.rating with
      | None -> 0.
      | Some prev_rating -> rating -. prev_rating
    in

    let rd_diff =
      match List.last t.rd with None -> 0. | Some prev_rd -> rd -. prev_rd
    in

    let vol_diff =
      match List.last t.vol with None -> 0. | Some prev_vol -> vol -. prev_vol
    in

    {
      position = t.position @ [ position ];
      league_id = t.league_id @ [ league_id ];
      event_nr = t.event_nr @ [ event_nr ];
      event_loc = t.event_loc @ [ event_loc ];
      rating = t.rating @ [ rating ];
      rating_diff = t.rating_diff @ [ rating_diff ];
      rd = t.rd @ [ rd ];
      rd_diff = t.rd_diff @ [ rd_diff ];
      vol = t.vol @ [ vol ];
      vol_diff = t.vol_diff @ [ vol_diff ];
    }

  let show_diff t =
    List.foldi t.rating_diff ~init:"" ~f:(fun i acc diff ->
        let event_loc = List.nth_exn t.event_loc i in
        let event_nr = List.nth_exn t.event_nr i in
        let position =
          match List.nth_exn t.position i with
          | None -> "-"
          | Some pos -> Int.to_string pos
        in
        let rating = List.nth_exn t.rating i in
        let rd = List.nth_exn t.rd i in
        acc
        ^ sprintf "%d.%s(%s) %.2f(%.2f-%.1f), " event_nr event_loc position diff
            rating rd)
end

module Info = struct
  type t = {
    id : int;
    name : string;
    course : string;
    rating : float;
    rd : float;
    vol : float;
    history : History.t;
  }
  [@@deriving show, fields]

  let update t ~position ~league_id ~event_nr ~event_loc ~rating ~rd ~vol =
    let history =
      History.add t.history ~position ~league_id ~event_nr ~event_loc ~rating
        ~rd ~vol
    in
    { t with history; rating; rd; vol }

  let to_participant t =
    Glicko2.Race.Participant.Fields.create ~id:t.id
      ~stats:
        (Some
           (Glicko2.Race.Stats.Fields.create ~rating:t.rating ~rd:t.rd
              ~vol:t.vol))
end

module Store = struct
  type t = { map : Info.t String.Map.t; settings : Glicko2.Settings.t }

  let create ~(settings : Glicko2.Settings.t) =
    { map = String.Map.empty; settings }

  let show t ?(course = "1") () =
    let runners =
      Map.data t.map
      (* |> List.map ~f:(fun info -> *)
      (*        printf "Info: %s\n" (Info.show info); *)
      (*        info) *)
      |> List.filter ~f:(fun info -> String.(info.course = course))
      |> List.sort ~compare:(fun info1 info2 ->
             Float.compare info1.rating info2.rating)
    in

    let out = sprintf "Ratings for '%s' course\n" course in
    let out =
      out ^ sprintf "Settings: %s\n" (Glicko2.Settings.show t.settings)
    in
    List.foldi runners ~init:out ~f:(fun i acc info ->
        acc
        ^ sprintf "%d %.2f %s (%d) (rd=%.2f, vol=%.2f):\n    %s\n\n" i
            info.rating info.name info.id info.rd info.vol
            (History.show_diff info.history))

  let add_if_not_exist t ~id ~name ~course =
    let hash = runner_hash ~id ~course in
    let map =
      match Map.find t.map hash with
      | None ->
          let runner_info =
            Info.Fields.create ~id ~name ~course
              ~rating:t.settings.initial_rating ~rd:t.settings.rd
              ~vol:t.settings.vol ~history:(History.empty ())
          in
          Map.add_exn t.map ~key:hash ~data:runner_info
      | Some _ -> t.map
    in

    { t with map }

  let info t ~id ~course =
    let hash = runner_hash ~id ~course in
    Map.find t.map hash

  let participant t ~id ~course =
    let hash = runner_hash ~id ~course in
    match Map.find t.map hash with
    | None -> None
    | Some info -> Some (Info.to_participant info)

  let all_participants t ~course =
    let m = Map.filter t.map ~f:(fun info -> String.(info.course = course)) in
    List.map (Map.data m) ~f:Info.to_participant

  let update_info t ~id ~course ~position ~league_id ~event_nr ~event_loc
      ~rating ~rd ~vol =
    let hash = runner_hash ~id ~course in
    let map =
      Map.update t.map hash ~f:(fun info ->
          match info with
          | None -> assert false
          | Some info ->
              Info.update info ~position ~league_id ~event_nr ~event_loc ~rating
                ~rd ~vol)
    in
    { t with map }
end

let calculate_ratings ~(settings : Glicko2.Settings.t)
    ~(league : League.League.t) : Store.t =
  let store = Store.create ~settings in

  let store =
    List.fold league.events ~init:store ~f:(fun store event ->
        match event.results with
        | None -> store
        | Some results ->
            List.fold results.courses ~init:store ~f:(fun store course ->
                let all_runners = course.results.finished in
                let store =
                  List.fold all_runners ~init:store ~f:(fun store runner ->
                      (* make sure runner exists in the store *)
                      Store.add_if_not_exist store ~id:runner.runner_nr
                        ~name:runner.name ~course:course.id)
                in

                let all_known_participants =
                  Store.all_participants store ~course:course.id
                in

                let race =
                  List.map all_runners ~f:(fun runner ->
                      let participant =
                        Store.participant store ~id:runner.runner_nr
                          ~course:course.id
                        |> Option.value_exn
                      in
                      [ participant ])
                in

                (* printf "Participants in event: %d %s %d %s\n" (List.length race) *)
                (*   course.id event.nr event.location; *)
                let glicko2 =
                  Glicko2.Ranking.of_race ~settings ~race
                    ~all_known_participants
                  |> Glicko2.Ranking.update_ratings
                in
                let updated_ratings = Glicko2.Ranking.players glicko2 in

                List.fold updated_ratings ~init:store ~f:(fun store ratings ->
                    let position =
                      match
                        List.findi all_runners ~f:(fun _ runner ->
                            runner.runner_nr = ratings.id)
                      with
                      | None -> None
                      | Some (idx, _) -> Some (idx + 1)
                    in
                    (* convert from index to position *)
                    Store.update_info store ~id:ratings.id ~position
                      ~league_id:league.id ~event_nr:event.nr
                      ~event_loc:event.location ~course:course.id
                      ~rating:ratings.rating ~rd:ratings.rd ~vol:ratings.vol)))
  in
  store

let%expect_test "calculate_ratings" =
  let league =
    Yojson.Safe.from_file
      "/home/angel/Documents/ocaml/vkd/league244_full_object.json"
    |> League.League.t_of_yojson
  in

  let settings =
    (* NOTE: these are the default settings *)
    Glicko2.Settings.create ~initial_rating:1500. ~rd:350. ~vol:0.06 ~tau:0.5 ()
  in

  let store = calculate_ratings ~settings ~league in
  (* TODO: 
           1. Play with different vol settings or tau settings. 
           2. Make sure that RD grows good amounts for each skipped events
           3. Double check if results make sense
           4. Handle disqualified runners
           *)

  printf "%s" (Store.show store ~course:"1" ());

  [%expect
    {|
    Ratings for '1' course
    Settings: { tau = 0.5; initial_rating = 1500.; rd = 350.; vol = 0.06 }
    0 841.77 Versockas Daumantas (1050) (rd=47.03, vol=0.06) - 0.00, -208.93, -14.94, -6.47, -18.75,
    1 901.62 Žiaukas Pijus (439) (rd=46.55, vol=0.06) - 0.00, -11.53, 27.36, -19.77,
    2 914.99 Lipkevičiūtė Samanta (438) (rd=49.47, vol=0.06) - 0.00,
    3 938.38 Boženokas Michailas (318) (rd=42.96, vol=0.06) - 0.00, -178.93,
    4 952.03 Žukauskas Artūras (136) (rd=44.03, vol=0.06) - 0.00, -43.67,
    5 952.35 Ažusienienė Sigita (324) (rd=49.47, vol=0.06) - 0.00,
    6 961.10 Stakišaitis Arūnas (77) (rd=38.42, vol=0.06) - 0.00, -19.38,
    7 961.78 Vaičiūnienė Renata (514) (rd=49.47, vol=0.06) - 0.00,
    8 965.94 Abramenkov Sergeij (183) (rd=39.20, vol=0.06) - 0.00, -26.73, -98.44,
    9 971.20 Kozyr Natalija (314) (rd=49.47, vol=0.06) - 0.00,
    10 972.26 Balbatunov Aleksandr (460) (rd=46.20, vol=0.06) - 0.00,
    11 980.63 Krasauskas Vytautas (327) (rd=49.47, vol=0.06) - 0.00,
    12 982.32 Baravikov Robertas (770) (rd=68.42, vol=0.06) - 0.00,
    13 983.16 Verbickas Jonas (732) (rd=43.31, vol=0.06) - 0.00, -37.98,
    14 988.71 Kazlauskas Ignas (437) (rd=46.20, vol=0.06) - 0.00,
    15 991.96 Kutniauskas Ovidijus (403) (rd=63.96, vol=0.06) - 0.00,
    16 1013.43 Visnap Viktor (821) (rd=66.17, vol=0.06) - 0.00,
    17 1019.84 Trečiokaitė Vilija (210) (rd=32.88, vol=0.06) - 0.00, -33.28, -28.46, 10.11, -36.99, -10.26, 121.31, -11.92,
    18 1023.47 Sutkutė Laura (366) (rd=63.96, vol=0.06) - 0.00,
    19 1030.29 Chassazirov Aleksandr (822) (rd=66.17, vol=0.06) - 0.00,
    20 1031.78 Oškinis Vytautas (288) (rd=49.47, vol=0.06) - 0.00,
    21 1034.19 Bukauskas Virginijus (855) (rd=33.87, vol=0.06) - 0.00, -51.24, -54.65, 21.71,
    22 1034.44 Šidlauskas Jurgis (487) (rd=71.50, vol=0.06) - 0.00, -97.65,
    23 1036.59 Mačanaitė Kristina (364) (rd=59.49, vol=0.06) - 0.00,
    24 1041.41 Jovaišas Jonas (1814) (rd=46.20, vol=0.06) - 0.00,
    25 1043.59 Arlauskienė Edita (665) (rd=33.12, vol=0.06) - 0.00, -53.01, -36.91, 10.10, 6.43, -9.15, 48.64,
    26 1043.66 Yla Lukas (402) (rd=33.83, vol=0.06) - 0.00, 184.83, -48.66, -100.23,
    27 1049.63 Liepa Dominykas (1597) (rd=46.20, vol=0.06) - 0.00,
    28 1050.22 Ašmonas Nojus (516) (rd=59.49, vol=0.06) - 0.00,
    29 1053.15 Tarozaitė Birutė (102) (rd=39.23, vol=0.06) - 0.00, -51.91, -33.88, -25.96, 141.92,
    30 1057.96 Čuprinskas Giedrius (182) (rd=33.50, vol=0.06) - 0.00, -5.55, -116.83,
    31 1058.87 Švetkauskas Edgaras (55) (rd=36.75, vol=0.06) - 0.00, -112.12, 69.44,
    32 1066.98 Pakarnienė Raminta (147) (rd=49.03, vol=0.06) - 0.00, -43.29, -19.59, -12.17,
    33 1074.85 Rutkauskas Robertas (378) (rd=34.74, vol=0.06) - 0.00, -72.31, -0.89, 22.62, -17.92, -4.31,
    34 1080.28 Arlauskas Jonas (666) (rd=33.85, vol=0.06) - 0.00, -63.52, -2.81, 21.43, 6.50, 54.83,
    35 1082.21 Garliauskas Mantas (462) (rd=46.20, vol=0.06) - 0.00,
    36 1083.07 Šlepetis Linas (1653) (rd=49.47, vol=0.06) - 0.00,
    37 1085.80 Pušinaitė Jurgita (443) (rd=63.96, vol=0.06) - 0.00,
    38 1088.33 Kauzonas Simonas (1160) (rd=65.08, vol=0.06) - 0.00,
    39 1089.41 Danius Marius (1064) (rd=42.31, vol=0.06) - 0.00, -81.67, -49.81, -14.95, 6.18, -8.94, 66.63, -10.84, -8.81,
    40 1090.44 Pigagienė Iveta (259) (rd=46.20, vol=0.06) - 0.00,
    41 1092.50 Kutyriova Tatjana (1652) (rd=49.47, vol=0.06) - 0.00,
    42 1098.66 Varnelis Darius (459) (rd=46.20, vol=0.06) - 0.00,
    43 1104.65 Donelaitis Lukas (1119) (rd=65.08, vol=0.06) - 0.00,
    44 1111.56 Danilevičius Mikas (756) (rd=61.01, vol=0.06) - 0.00, -75.27,
    45 1119.06 Pozingytė Ernesta (509) (rd=46.20, vol=0.06) - 0.00,
    46 1129.91 Zlatkus Ričardas (517) (rd=49.47, vol=0.06) - 0.00,
    47 1130.79 Kundrotis Gediminas (1083) (rd=72.66, vol=0.06) - 0.00,
    48 1130.80 Mainelis Antanas (739) (rd=53.91, vol=0.06) - 0.00, -46.47,
    49 1131.67 Štrafėlas Robertas (216) (rd=65.82, vol=0.06) - 0.00,
    50 1132.00 Šinkūnaitė Viltė (251) (rd=59.49, vol=0.06) - 0.00,
    51 1133.07 Podiriaka Vladislavas (434) (rd=63.96, vol=0.06) - 0.00,
    52 1137.95 Jastrzebski Michal (1043) (rd=71.41, vol=0.06) - 0.00,
    53 1139.44 Kurajevas Aleksandras (500) (rd=46.20, vol=0.06) - 0.00,
    54 1145.05 Skirmantas Rimantas (43) (rd=34.93, vol=0.06) - 0.00, -88.23,
    55 1147.73 Naujokaitis Mantas (919) (rd=89.63, vol=0.06) - 0.00,
    56 1148.36 Pečiokas Šarūnas (1847) (rd=65.82, vol=0.06) - 0.00,
    57 1148.83 Jautakienė Giedrė (306) (rd=63.96, vol=0.06) - 0.00,
    58 1151.92 Pužas Donatas (605) (rd=66.24, vol=0.06) - 0.00,
    59 1152.67 Bačius Vincas (445) (rd=41.57, vol=0.06) - 0.00, -64.17,
    60 1155.88 Kobec Svetlana (61) (rd=46.20, vol=0.06) - 0.00,
    61 1164.10 Pilkauskas Gediminas (374) (rd=46.20, vol=0.06) - 0.00,
    62 1164.59 Tolkušinas Gabrielius (477) (rd=63.96, vol=0.06) - 0.00,
    63 1166.45 Keina Darius (1566) (rd=80.83, vol=0.06) - 0.00,
    64 1168.04 Dūda Kostas (316) (rd=48.95, vol=0.06) - 0.00, 63.30,
    65 1170.98 Matakas Česlovas (889) (rd=66.17, vol=0.06) - 0.00,
    66 1171.07 Rimydytė Ona (41) (rd=32.40, vol=0.06) - 0.00, -61.84, -29.78, 5.11, 37.81, 26.76, 7.93, 9.70, 22.20, -5.96, 23.04, -3.33, 2.42, 0.49, -3.77, -22.28, -12.34, 15.67,
    67 1175.09 Jatkauskas Jonas (65) (rd=30.52, vol=0.06) - 0.00, -77.75, -5.72, -42.19, -2.72, -45.84, -72.90, 5.22, 21.35, -11.67, 6.61, 26.24, -20.49, -23.27,
    68 1178.75 Dauderienė Živilė (38) (rd=51.67, vol=0.06) - 0.00, -39.67,
    69 1191.10 Rutkauskas Domantas (115) (rd=73.11, vol=0.06) - 0.00,
    70 1193.14 Garbaliauskaitė Elena (1561) (rd=39.32, vol=0.06) - 0.00, -47.90,
    71 1196.10 Archipova Dienienė Saulė (14) (rd=63.96, vol=0.06) - 0.00,
    72 1196.35 Pazdrazdis Rolandas (488) (rd=76.40, vol=0.06) - 0.00,
    73 1198.21 Vilčinskas Rimantas (1678) (rd=34.14, vol=0.06) - 0.00, -22.77, -31.40, -28.41, -32.86, -24.80,
    74 1202.17 Striškaitė Živilė (1095) (rd=79.33, vol=0.06) - 0.00,
    75 1202.77 Budginas Vytas (295) (rd=40.09, vol=0.06) - 0.00, 29.88,
    76 1211.48 Kavaliauskas Tadas (903) (rd=88.92, vol=0.06) - 0.00,
    77 1211.86 Povilonytė Ieva (356) (rd=63.96, vol=0.06) - 0.00,
    78 1214.06 Milašauskas Lukas (426) (rd=65.82, vol=0.06) - 0.00,
    79 1214.72 Mockevičius Marius (404) (rd=34.34, vol=0.06) - 0.00, -131.97,
    80 1225.05 Juodviršis Mindaugas (626) (rd=46.20, vol=0.06) - 0.00,
    81 1230.81 Kulevičius Donaldas (323) (rd=38.37, vol=0.06) - 0.00, -132.89,
    82 1233.94 Sivickis Dainius (1768) (rd=30.77, vol=0.06) - 0.00, -169.85, 105.50, 8.84, -56.31, -1.29, 2.39, 15.64,
    83 1235.58 Petrauskas Paulius (927) (rd=78.34, vol=0.06) - 0.00,
    84 1238.03 Ričkus Arnoldas (64) (rd=34.89, vol=0.08) - 0.00, -40.41, 497.22, -52.60, -40.22, -43.39, -49.22, -25.18, 0.96, 10.22, -19.71,
    85 1241.94 Černiauskas Laurynas (1045) (rd=88.92, vol=0.06) - 0.00,
    86 1242.34 Musajevaitė Agnė (577) (rd=73.11, vol=0.06) - 0.00,
    87 1242.57 Varonenka Juozas (1796) (rd=31.77, vol=0.06) - 0.00, -12.90, 1.71, -3.72, 61.71, -4.38,
    88 1247.29 Brazinskas Tomas (575) (rd=49.42, vol=0.06) - 0.00, 11.35, -23.28,
    89 1248.77 Kaminskij Aleksei (785) (rd=76.24, vol=0.06) - 0.00,
    90 1268.30 Staškevičiūtė Raminta (518) (rd=59.49, vol=0.06) - 0.00,
    91 1268.51 Juška Skaidrius (189) (rd=44.30, vol=0.06) - 0.00, 3.58,
    92 1269.57 Pauliukevičius Povilas (606) (rd=44.27, vol=0.06) - 0.00, -123.15, -38.23,
    93 1270.90 Pigagaitė Aistė (261) (rd=39.77, vol=0.06) - 0.00, 57.12,
    94 1274.36 Matulis Daumantas (926) (rd=49.47, vol=0.06) - 0.00,
    95 1281.93 Pranaitis Tomas (899) (rd=59.49, vol=0.06) - 0.00,
    96 1283.78 Aglinskas Ramūnas (266) (rd=49.47, vol=0.06) - 0.00,
    97 1286.79 Neniškis Algirdas (1697) (rd=32.27, vol=0.06) - 0.00, 206.42, -65.27,
    98 1293.21 Labenskis Vytautas (128) (rd=49.47, vol=0.06) - 0.00,
    99 1293.47 Bakutis Algimantas (545) (rd=70.77, vol=0.06) - 0.00,
    100 1293.56 Vilčiauskas Mindaugas (118) (rd=73.11, vol=0.06) - 0.00,
    101 1293.91 Malcaitė Eglė (762) (rd=29.61, vol=0.06) - 0.00, 169.85, -15.01, 55.61, -12.39, -17.79, -0.89, 60.12, -12.00, -32.73, -35.04, -7.82, -13.48, -31.04,
    102 1294.02 Lažaunikas Rytis (169) (rd=42.99, vol=0.06) - 0.00, -55.48,
    103 1294.75 Navickas Darius (208) (rd=36.30, vol=0.06) - 0.00, -67.17, -137.47, -27.88,
    104 1294.78 Mejeras Gintaras (28) (rd=31.68, vol=0.07) - 0.00, -259.52, 10.78, 138.93,
    105 1305.26 Dumšė Paulius (252) (rd=43.12, vol=0.06) - 0.00, 10.39, 38.86, 18.64,
    106 1310.62 Šimėnas Valdemaras (1685) (rd=30.90, vol=0.06) - 0.00, -139.14, -35.85,
    107 1313.27 Rudėnas Darius (13) (rd=63.96, vol=0.06) - 0.00,
    108 1315.92 Jasutis Augmantas (461) (rd=32.49, vol=0.06) - 0.00, 211.46, -6.87, -29.60, -43.56,
    109 1315.92 Liogė Ugnius (83) (rd=40.47, vol=0.06) - 0.00, 88.31,
    110 1322.82 Šapranauskas Jonas (219) (rd=59.49, vol=0.06) - 0.00,
    111 1329.02 Petrauskis Darius (1097) (rd=55.92, vol=0.06) - 0.00, -79.81,
    112 1331.42 Mednikovas Justinas (934) (rd=75.29, vol=0.06) - 0.00,
    113 1331.55 Lukoševičius Mantas (82) (rd=34.15, vol=0.06) - 0.00, 115.32, -11.18,
    114 1332.22 Sabataitis Kristijonas (50) (rd=29.30, vol=0.06) - 0.00, 9.61, -36.18, 6.02, 29.46, -13.15, 0.40, 9.28, 22.57, -5.74, 12.73, -6.53, 17.18, -0.43, 0.89, -9.45,
    115 1333.10 Ivanovas Edgaras (90) (rd=32.41, vol=0.06) - 0.00, -10.66, -60.91, 44.96, -33.51, -6.98, -4.93, 14.17,
    116 1338.49 Vaitkūnas Andrius (856) (rd=35.51, vol=0.06) - 0.00, -11.58, -13.66, -22.33, -28.05,
    117 1339.68 Sriubas Egidijus (191) (rd=28.72, vol=0.06) - 0.00, -199.48, -8.92, 3.61, -41.68, 56.74, 36.01, -0.60, 0.74, -1.81, -17.01, -31.30, -6.35, -31.85, -12.61, -7.72, -20.75,
    118 1344.79 Ožema Andrijis (18) (rd=63.96, vol=0.06) - 0.00,
    119 1351.77 Žiogas Dovydas (384) (rd=42.60, vol=0.06) - 0.00, 68.91,
    120 1355.08 Aleknavičius Jurgis (108) (rd=68.42, vol=0.06) - 0.00,
    121 1357.55 Krauleidis Justinas (89) (rd=31.03, vol=0.06) - 0.00, 60.47, -24.19, -19.35, -23.08, 14.65, -27.26,
    122 1360.54 Kryžanauskas Audrius (476) (rd=63.96, vol=0.06) - 0.00,
    123 1362.31 Kanapinskaitė Viltė (184) (rd=38.03, vol=0.06) - 0.00, 25.87,
    124 1370.35 Džervus Jonas (91) (rd=79.33, vol=0.06) - 0.00,
    125 1374.60 Petrauskas Albertas (142) (rd=36.90, vol=0.06) - 0.00, -88.60, -72.40,
    126 1375.42 Ziaziulia Ivanas (96) (rd=73.11, vol=0.06) - 0.00,
    127 1379.11 Časas Vincentas Petras (37) (rd=37.18, vol=0.06) - 0.00, -202.67,
    128 1379.46 Šnirpūnas Mantas (1026) (rd=38.63, vol=0.06) - 0.00, 15.22, 52.17,
    129 1384.86 Lapinskas Ernestas (72) (rd=42.70, vol=0.06) - 0.00, -33.41, -40.40,
    130 1391.33 Astrauskas Rokas (505) (rd=46.20, vol=0.06) - 0.00,
    131 1392.06 Pranciulis Rimvydas (5217) (rd=63.96, vol=0.06) - 0.00,
    132 1394.13 Kubaitis Arūnas (319) (rd=28.65, vol=0.06) - 0.00, 97.17, -139.95, 86.83,
    133 1401.04 Dulskis Laurynas (715) (rd=39.95, vol=0.06) - 0.00, 48.70,
    134 1402.13 Leipus Vytautas (864) (rd=28.88, vol=0.06) - 0.00, 8.51, -65.65, 83.25, -13.29, -16.47, -3.40, 24.00, -0.33, 21.48, 53.72, 10.37, -28.12, -26.24, -14.53, -15.51, -4.37, 21.07, -5.89, -12.05, -7.70, -24.55, 12.07, -39.73,
    135 1404.47 Paukštė Mindaugas (300) (rd=42.72, vol=0.06) - 0.00, 100.59, 30.56, -37.04,
    136 1410.97 Rimša Evaldas (629) (rd=54.95, vol=0.06) - 0.00, 200.27, -13.91,
    137 1411.19 Kozič Vitalij (872) (rd=65.08, vol=0.06) - 0.00,
    138 1411.75 Kušeliauskas Kęstutis (54) (rd=38.60, vol=0.06) - 0.00, 102.57,
    139 1419.54 Mačys Regimantas (1182) (rd=76.40, vol=0.06) - 0.00,
    140 1421.41 Cvetkovas Valerijus (280) (rd=44.16, vol=0.06) - 0.00, 137.01, -68.89,
    141 1422.50 Jurkevičius Antanas (1719) (rd=31.92, vol=0.06) - 0.00, -107.87, -32.91, -49.74, 7.55, 25.69, 38.89,
    142 1425.76 Šnirpūnas Vincas (133) (rd=35.44, vol=0.06) - 0.00, 132.26, -31.90, 22.76,
    143 1427.64 Okulič - Kazarinas Vaidotas (120) (rd=28.75, vol=0.06) - 0.00, -87.61, -12.81, 18.76, 1.42, 0.10, -23.42, -21.26,
    144 1428.89 Ptašekas Julius (325) (rd=56.26, vol=0.06) - 0.00, 33.98,
    145 1437.33 Šilgalis Vaidotas (699) (rd=49.47, vol=0.06) - 0.00,
    146 1440.99 Ragauskas Audrius (99) (rd=28.97, vol=0.06) - 0.00, -131.73, 31.52, 3.57, -7.16, -35.30, 11.95,
    147 1446.71 Sriubas Vainius (475) (rd=63.96, vol=0.06) - 0.00,
    148 1450.59 Čiauška Tadas (935) (rd=75.29, vol=0.06) - 0.00,
    149 1451.79 Stadalius Vaidas (238) (rd=65.08, vol=0.06) - 0.00,
    150 1454.29 Ažukas Jonas (1058) (rd=40.42, vol=0.06) - 0.00, 176.36, 30.20,
    151 1469.08 Gembutaitė Sandra (530) (rd=28.93, vol=0.06) - 0.00, 51.34, 15.29, 41.07, -32.38, 29.21, 32.43, -18.87, -13.26, 20.51, 32.62, 2.70, -5.80, -22.22, -16.94, -14.27, -33.90, 7.37, 20.62, -3.76,
    152 1477.24 Vaižmužys Justinas (255) (rd=28.52, vol=0.07) - 0.00, -7.32, -145.95, -50.75, 93.39, -59.37, -14.41, -4.35,
    153 1480.29 Romanovas Marijus (1668) (rd=29.44, vol=0.07) - 0.00, 86.91, 175.58, -24.83, 58.48, -23.38, -25.58, -49.09, -14.01, -56.28, 6.97, 1.73, -21.30,
    154 1486.37 Volungevičienė Judita (231) (rd=59.49, vol=0.06) - 0.00,
    155 1494.18 Sveikauskas Julius (49) (rd=33.19, vol=0.06) - 0.00, 46.79, 5.65, 4.66, -19.20, -16.46,
    156 1498.70 Tauraitis Dalius (904) (rd=62.41, vol=0.06) - 0.00, -129.41,
    157 1500.00 Gavėnas Gintaras (88) (rd=59.49, vol=0.06) - 0.00,
    158 1501.79 Šimėnas Andrius (541) (rd=65.82, vol=0.06) - 0.00,
    159 1503.05 Petrevičius Aras (16) (rd=28.85, vol=0.06) - 0.00, -116.27, 4.88, 54.52, 20.42, -16.78, -42.17, -73.91, -18.46,
    160 1507.75 Boženokas Michailas (816) (rd=28.34, vol=0.06) - 0.00, -23.36, 0.71, 71.30,
    161 1513.33 Olišauskas Raimondas (941) (rd=72.66, vol=0.06) - 0.00,
    162 1515.74 Norkus Simonas (619) (rd=49.47, vol=0.06) - 0.00,
    163 1526.51 Garnelis Justas (849) (rd=34.25, vol=0.09) - 0.00, -6.75, 442.08, 21.41, -58.87, -28.48, -12.26, 81.15, -50.53, -36.13, -55.47, -1.34, -4.47, -11.89, -43.85, 44.98, 5.03, -36.77, 60.55, -9.20, 40.17, 72.16,
    164 1536.55 Jadenkus Domantas (778) (rd=29.56, vol=0.06) - 0.00, 209.27, 23.29, 49.32,
    165 1539.10 Pakarnis Vytautas (160) (rd=30.16, vol=0.06) - 0.00, -157.39, -141.45, 82.27, 2.44, -47.96, 12.63, 38.61,
    166 1539.39 Žilinskas Julius (116) (rd=27.87, vol=0.06) - 0.00, -22.99, -47.03, -37.27, -36.06, 37.38, 16.76, -19.47, 7.55, 15.36, -31.48,
    167 1552.61 Saldžiūnas Viktoras (68) (rd=32.77, vol=0.06) - 0.00, 121.11, -0.35,
    168 1557.72 Januškevičius Regimantas (119) (rd=33.85, vol=0.06) - 0.00, -16.67, 91.30, -13.73,
    169 1567.51 Pašuk Sergeij (151) (rd=31.69, vol=0.06) - 0.00, -125.35, -25.21,
    170 1576.61 Mitrikas Matas (1639) (rd=49.47, vol=0.06) - 0.00,
    171 1585.12 Švedarauskas Simonas (1689) (rd=29.34, vol=0.07) - 0.00, -352.64, 16.39, 19.92, 91.48, -26.30, -19.71, 0.27, -41.99, -1.71, 8.95, -17.37, 32.33, -34.05, 4.90, 36.64,
    172 1587.59 Mikalauskas Robertas (146) (rd=84.43, vol=0.06) - 0.00,
    173 1594.44 Petrauskaitė Neda (263) (rd=46.20, vol=0.06) - 0.00,
    174 1597.96 Giraitis Mindaugas (512) (rd=28.76, vol=0.06) - 0.00, 310.48, -15.02, 53.50, -14.04, 21.91, -1.81, 1.73, -14.27, -24.55, 4.17, -15.44, -8.27, -9.36, -6.53, -19.66, 43.22, -40.59, 3.52, 44.80, -16.12, -3.65, -2.55,
    175 1599.07 Žiogas Mykolas (633) (rd=36.89, vol=0.06) - 0.00, 14.81, 28.16, 4.61, 35.50,
    176 1605.03 Zaicevas Giedrius (260) (rd=29.27, vol=0.06) - 0.00, -133.04, 24.24, 33.53, 44.85, -9.79, -21.96, -18.03,
    177 1612.09 Traubienė Aistė (347) (rd=43.89, vol=0.06) - 0.00, -210.96, -22.82,
    178 1613.72 Mačiulis Dainius (971) (rd=65.08, vol=0.06) - 0.00,
    179 1613.76 Babelis Tomas (817) (rd=49.47, vol=0.06) - 0.00,
    180 1626.91 Grigaliūnaitė Agnė (97) (rd=46.20, vol=0.06) - 0.00,
    181 1631.57 Aleksandraitytė Džiuginta (93) (rd=33.69, vol=0.06) - 0.00, -60.09, -12.78,
    182 1631.77 Stančikas Virginijus (29) (rd=28.31, vol=0.07) - 0.00, 31.51, -53.85, 8.57, 58.64, -61.52, -65.20, 23.59, 16.21, 59.79, -49.92, -3.27, 13.09, -3.96, 10.25, -13.10, -26.79, 34.76, 31.26, -14.58,
    183 1631.92 Paspirgėlytė Agnė (289) (rd=28.41, vol=0.06) - 0.00, 7.37, 42.12, 5.18, 6.07, -10.92, -32.95, 25.80, -53.21, -12.44, 16.89, 2.88,
    184 1636.85 Vidugiris Modestas (307) (rd=29.00, vol=0.06) - 0.00, 15.01, -19.73, 27.15, 48.13, 8.64, 31.24, 10.83, -1.63,
    185 1639.20 Žvinytė Inga (320) (rd=29.12, vol=0.06) - 0.00, -46.50, 134.29, 31.06, 6.82, 15.08, -52.78,
    186 1641.97 Balčiūnaitė Barbora (34) (rd=33.25, vol=0.06) - 0.00, 118.28, 10.06,
    187 1643.35 Paukštė Kristupas (636) (rd=46.20, vol=0.06) - 0.00,
    188 1645.13 Auštrienė Giedrė (63) (rd=32.78, vol=0.06) - 0.00, 2.05, -66.69, 46.21,
    189 1647.82 Zacharov Igor (113) (rd=29.63, vol=0.06) - 0.00, -91.54, 56.72, 10.93, 19.54, 9.51, -19.41, -29.96,
    190 1651.57 Karnavičius Jurgis (547) (rd=46.20, vol=0.06) - 0.00,
    191 1654.13 Pranckaitis Dovydas (911) (rd=39.04, vol=0.06) - 0.00, -148.46, -10.01,
    192 1663.49 Kniukšta Romualdas (30) (rd=63.96, vol=0.06) - 0.00,
    193 1664.18 Kondrotas Raimondas (171) (rd=32.70, vol=0.06) - 0.00, -18.22, -24.27, -0.75,
    194 1669.41 Cicėnas Audrius (114) (rd=32.49, vol=0.06) - 0.00, -58.75, -58.66, 73.45, 36.18,
    195 1676.03 Sriubaitė Eva (126) (rd=39.17, vol=0.06) - 0.00, -137.69,
    196 1683.05 Rusakevičius Dainius (156) (rd=27.89, vol=0.07) - 0.00, -93.48, 17.01, -51.02, 58.74, -13.61, 0.37, -11.43, -21.13, 66.38, -24.17, -16.88, 17.66, 7.09, 24.54, -45.73, -4.98, 4.08, -30.25,
    197 1685.44 Račkauskas Vaidas (847) (rd=37.02, vol=0.07) - 0.00, -267.11, -55.83,
    198 1693.92 Mutka Mikko (1803) (rd=28.21, vol=0.06) - 0.00, -39.84, 16.65, -7.60, 23.69, -20.61, -13.54, -0.26, -27.59, -49.83, -23.05, -17.57, 18.04, 5.46, -17.28, -36.80, 14.41,
    199 1699.91 Dienytė Margarita (5) (rd=36.04, vol=0.06) - 0.00, -40.58, -18.48,
    200 1702.77 Diržiūtė Gedvilė (164) (rd=89.14, vol=0.06) - 0.00,
    201 1706.39 Lelkaitis Valdas (39) (rd=27.49, vol=0.06) - 0.00, 7.38, 101.02, 35.75, -11.36, -15.54, 11.17, -34.33, -23.66, -20.17, 7.73, 13.06, -14.59,
    202 1716.61 Krupinas Eugenijus (1631) (rd=49.47, vol=0.06) - 0.00,
    203 1724.96 Radžius Antanas (335) (rd=28.52, vol=0.07) - 0.00, 12.15, -149.32, 139.78, 71.22, 38.85, 11.44, 31.44, 21.48, -47.50,
    204 1732.51 Petrilionis Marius (56) (rd=26.36, vol=0.06) - 0.00, -43.24, -34.43, 11.04, 70.27, 89.96, 29.87,
    205 1740.63 Šinkūnas Rimvydas (328) (rd=30.06, vol=0.06) - 0.00, -264.36, 40.62, 38.24, 2.40, -26.05,
    206 1741.54 Pralgauskis Danielius (5254) (rd=63.96, vol=0.06) - 0.00,
    207 1741.84 Julovas Germanas (596) (rd=31.65, vol=0.06) - 0.00, -37.98, 34.27, 76.19, -9.90,
    208 1742.62 Narvydas Simonas (45) (rd=29.49, vol=0.07) - 0.00, -20.11, -104.73, -3.76, 131.62, 51.27, -45.60, -29.21, -51.06, -15.40, 11.67, -9.17,
    209 1752.68 Benetis Vytenis (1774) (rd=66.17, vol=0.06) - 0.00,
    210 1755.69 Ozolinš Vytautas (993) (rd=30.36, vol=0.06) - 0.00, 100.17, 46.94, 61.21, 43.72, 25.11, -43.30, -17.32,
    211 1755.77 Kriukas Darius (341) (rd=49.66, vol=0.06) - 0.00, 267.92,
    212 1763.20 Jasinevičius Tomas (349) (rd=35.93, vol=0.06) - 0.00, -59.94, -16.61, 17.69, 8.59,
    213 1770.34 Mickevičius Rokas (923) (rd=88.92, vol=0.06) - 0.00,
    214 1772.59 Atgalainė Adrija (168) (rd=59.49, vol=0.06) - 0.00,
    215 1773.65 Mackonis Adolfas (84) (rd=35.43, vol=0.06) - 0.00, 98.99, -51.13,
    216 1784.55 Žuravliova Viktorija (1698) (rd=40.59, vol=0.06) - 0.00, 44.57,
    217 1786.22 Blaževičius Gediminas (225) (rd=59.49, vol=0.06) - 0.00,
    218 1794.27 Kriukas Oskaras (138) (rd=50.12, vol=0.06) - 0.00, 190.23,
    219 1795.70 Mickuvienė Algirda (140) (rd=66.24, vol=0.06) - 0.00,
    220 1804.46 Kaupinis Algirdas (124) (rd=37.08, vol=0.06) - 0.00, 88.16, 130.08,
    221 1805.50 Masevičiūtė Kristina (539) (rd=46.20, vol=0.06) - 0.00,
    222 1813.74 Jokubauskis Kęstutis (321) (rd=29.34, vol=0.06) - 0.00, 88.66, 53.27, -68.62, 7.04, 18.76, -11.84, -35.10, 7.59, -14.80, 22.90, 3.88, -11.62, 13.87, -47.21, -21.45, 5.81, -12.33, -8.98, -30.47,
    223 1814.91 Girčys Benas (621) (rd=40.37, vol=0.06) - 0.00, 91.97, -55.12, -48.84,
    224 1815.34 Gunnarsson Troj (501) (rd=59.33, vol=0.06) - 0.00, -233.22,
    225 1817.49 Rimkutė Gabija (234) (rd=43.50, vol=0.06) - 0.00, 68.07,
    226 1817.87 Šumskas Vilius (46) (rd=29.07, vol=0.06) - 0.00, 68.43, -20.88, -45.49, -6.11, 3.91, 24.40, 44.94, -29.83, 21.93, -22.74, -15.82, -57.66, 2.62, -20.64, 21.05, 11.65, 19.03,
    227 1825.03 Gradauskas Vytautas (409) (rd=47.87, vol=0.06) - 0.00, 96.94,
    228 1829.10 Rinkevičius Darius (112) (rd=41.52, vol=0.06) - 0.00, 97.39,
    229 1841.09 Makutėnas Mikalojus (213) (rd=29.60, vol=0.06) - 0.00, -147.55, 50.04, 2.09, 81.62, -70.56, 14.17,
    230 1842.24 Januška Deividas (408) (rd=47.78, vol=0.06) - 0.00, 132.18,
    231 1844.61 Strolė Salvijus (946) (rd=73.11, vol=0.06) - 0.00,
    232 1846.39 Brazauskaitė Leokadija (94) (rd=41.77, vol=0.06) - 0.00, 101.05,
    233 1847.31 Petrauskas Andrius (330) (rd=31.63, vol=0.06) - 0.00, 246.40, -19.30, -49.66, 32.87, 19.81, -1.64,
    234 1849.34 Činčikaitė Miglė (471) (rd=40.71, vol=0.06) - 0.00, -24.05, -67.38, 44.93,
    235 1851.60 Rudis Algimantas (100) (rd=39.32, vol=0.06) - 0.00, 77.52, 38.26,
    236 1856.93 Raugevičius Martynas (513) (rd=35.01, vol=0.06) - 0.00, 41.51, 12.70, -47.98, 15.87,
    237 1858.35 Montvila Mykolas (95) (rd=28.84, vol=0.06) - 0.00, 10.05, -62.29, 25.65, -15.66, -17.90, 14.18, -14.05, 2.59, 8.19, 3.14, 10.15, 17.07, -22.99, 6.66, -55.16, 30.24, -4.29, -1.97, 29.47,
    238 1859.29 Dulskis Marijus (720) (rd=32.01, vol=0.06) - 0.00, -63.16, -70.10, -16.53, 8.27, -52.15, -50.38, 24.51,
    239 1862.31 Mockaitis Rėjus (938) (rd=79.33, vol=0.06) - 0.00,
    240 1867.20 Zaveckas Andrius (232) (rd=35.37, vol=0.06) - 0.00, 118.60, 77.27, 62.36, 58.12, 52.68,
    241 1870.65 Balčiūnas Vincas (35) (rd=31.89, vol=0.06) - 0.00, 257.47, 56.48, 2.18,
    242 1871.91 Jadenkus Evaldas (353) (rd=65.82, vol=0.06) - 0.00,
    243 1880.58 Papreckis Povilas (148) (rd=47.22, vol=0.06) - 0.00, 178.49,
    244 1881.92 Jauniškis Robertas (104) (rd=29.51, vol=0.06) - 0.00, -66.24, -54.98, 49.54, 5.24, 24.61, 21.83, -21.94, -39.54,
    245 1882.78 Ziaziulytė Goda (162) (rd=46.20, vol=0.06) - 0.00,
    246 1886.08 Regesas Augustas (930) (rd=38.88, vol=0.06) - 0.00, 98.36, -27.11, 63.54,
    247 1891.51 Rudys Audrius (152) (rd=33.14, vol=0.06) - 0.00, 24.26, 17.63, 18.58, -25.86, 37.09, 4.36, -37.35,
    248 1892.36 Šimkonis Andrius (275) (rd=38.94, vol=0.06) - 0.00, 46.85, 38.36, 8.24,
    249 1934.65 Slankauskas Kęstutis (67) (rd=28.78, vol=0.06) - 0.00, 112.67, 65.54, 38.86, -20.51, -54.58, 14.87, 29.55, -17.26, -34.37, 17.24, 31.02, -35.76, -26.63, 4.82, 27.93, 18.47, 23.68, 20.14,
    250 1938.86 Bertašius Rimvydas (175) (rd=30.27, vol=0.06) - 0.00, 64.84, 48.42, -110.50, 45.27, 15.33, 15.29, 28.84, 13.36, -22.73,
    251 1938.98 Volungevičius Mantas (351) (rd=36.13, vol=0.06) - 0.00, 78.00, -5.38, 30.43,
    252 1951.52 Janušis Gediminas (333) (rd=92.63, vol=0.06) - 0.00,
    253 1959.57 Šalkauskas Martynas (483) (rd=63.96, vol=0.06) - 0.00,
    254 1962.27 Aranskis Artūras (272) (rd=41.32, vol=0.06) - 0.00, 270.43, 150.76, 48.06,
    255 1963.92 Buožys Egidijus (185) (rd=33.96, vol=0.06) - 0.00, 40.49, -59.22, 24.39, -6.84, 20.58,
    256 1965.86 Miežinis Dominykas (143) (rd=49.81, vol=0.06) - 0.00, 139.75,
    257 1966.25 Matonis Audrūnas (566) (rd=42.18, vol=0.06) - 0.00, 178.21, -20.13,
    258 1969.50 Kazlauskas Tadas (107) (rd=31.83, vol=0.06) - 0.00, 40.30, -29.53, -8.62, 51.36, 8.68, -6.15,
    259 1980.50 Iliev Angel (617) (rd=29.96, vol=0.06) - 0.00, 26.16, -25.88, 33.31, 42.62, -28.18, 25.42, -45.85, 43.74, -39.57, -19.14, -9.48, -0.14, -24.80, -15.34, 1.85, 4.66, 18.89, 29.91, 28.23, 19.86, 5.33,
    260 1990.31 Markovas Paulius (542) (rd=38.19, vol=0.06) - 0.00, 85.36, 23.32,
    261 1990.33 Memelau Aliaksei (75) (rd=31.66, vol=0.06) - 0.00, -64.95, -40.01, 12.28, 60.69, -21.49, 33.09, 33.05,
    262 2004.75 Stupelis Rimvydas (536) (rd=48.02, vol=0.06) - 0.00, 68.60,
    263 2005.47 Zaliauskas Povilas (499) (rd=50.95, vol=0.06) - 0.00, 54.93,
    264 2008.21 Ražaitytė Gabija (947) (rd=73.11, vol=0.06) - 0.00,
    265 2013.84 Lazauskas Domas (130) (rd=54.62, vol=0.06) - 0.00, 19.30,
    266 2014.81 Ščerbavičius Edvardas (7) (rd=64.87, vol=0.06) - 0.00, -6.31,
    267 2016.60 Bačkys Rolandas (546) (rd=46.20, vol=0.06) - 0.00,
    268 2025.08 Traubas Adomas (69) (rd=32.63, vol=0.06) - 0.00, 6.52, 53.39, 2.25, -5.86, 12.78, 26.53, -8.22, -73.70,
    269 2035.26 Paužaitė Sandra (968) (rd=82.92, vol=0.06) - 0.00,
    270 2048.12 Užkuraitis Simanas (179) (rd=32.04, vol=0.07) - 0.00, -131.54, 42.43, -51.18, 40.72, 32.10, 44.66, -15.78, 0.55, 17.35, 22.22, 42.76, 9.47, -1.22, -59.47, -29.83, 25.89, 45.48, 26.43, 17.09, 15.58, 22.57, 8.96, 18.30, -3.39, -14.54,
    271 2065.99 Petrauskas Giedrius (98) (rd=36.41, vol=0.06) - 0.00, 61.13, 40.99,
    272 2066.67 Šalkauskas Vilius (482) (rd=49.85, vol=0.06) - 0.00, 91.34,
    273 2075.43 Mendelevičius Mantas (331) (rd=59.11, vol=0.06) - 0.00, 66.66,
    274 2077.03 Ralys Vytautas (974) (rd=72.66, vol=0.06) - 0.00,
    275 2094.36 Traubaitė Judita (150) (rd=70.77, vol=0.06) - 0.00,
    276 2094.63 Lekaveckas Mindaugas (978) (rd=34.42, vol=0.06) - 0.00, -4.92, -11.18, -26.96, -15.43, 40.98, 24.69, 12.39,
    277 2096.48 Staišiūnas Viktoras (343) (rd=35.07, vol=0.06) - 0.00, 52.78, 49.80, 19.21, -21.14, 5.17,
    278 2097.49 Dienys Algirdas (3) (rd=35.23, vol=0.06) - 0.00, -71.02, 87.57, 42.99, -25.89, -16.08, 26.91,
    279 2108.98 Učkuronis Aleksandras (193) (rd=50.95, vol=0.06) - 0.00, 19.08, -30.75,
    280 2122.81 Petrovas Lukas (570) (rd=50.09, vol=0.06) - 0.00, 131.72,
    281 2125.83 Finaženokas Andrius (178) (rd=65.82, vol=0.06) - 0.00,
    282 2133.31 Krėpšta Simonas (122) (rd=78.34, vol=0.06) - 0.00,
    283 2143.17 Kantautas Marius (52) (rd=33.92, vol=0.06) - 0.00, 78.96, -11.79, 27.87, -2.12, -19.23, 19.34, -10.05, 16.10, 25.25, -41.71, -9.32,
    284 2157.82 Šulčys Nerijus (579) (rd=72.66, vol=0.06) - 0.00,
    285 2161.11 Kuzminskis Tomas (125) (rd=41.96, vol=0.06) - 0.00, 73.66, 6.80, -6.42, -24.44,
    286 2188.99 Barkauskas Aidas (1) (rd=36.86, vol=0.08) - 0.00, 86.98, 47.39, 33.20, -124.93, 9.72, -187.77, 63.29, 26.54, 49.76, 43.87, 49.80, 27.15, 31.43, -10.30, 11.74, 16.42, 20.21, -44.58, 26.33, 31.02, 15.01, -10.34,
    287 2193.69 Mackevičius Tadas (945) (rd=58.35, vol=0.06) - 0.00, 104.14,
    288 2197.82 Lapinskas Ramojus (154) (rd=44.41, vol=0.06) - 0.00, 38.25, 45.12, 4.79,
    289 2205.60 Rimkus Povilas (491) (rd=36.57, vol=0.06) - 0.00, 112.66, 14.18, 27.71, 28.51, 36.06,
    290 2207.07 Mironovas Dmitrijus (640) (rd=59.39, vol=0.06) - 0.00, 111.22,
    291 2212.53 Kudriavcevas Paulius (40) (rd=66.23, vol=0.06) - 0.00, 88.59, -7.42,
    292 2244.63 Mickus Dovydas (574) (rd=71.71, vol=0.06) - 0.00,
    293 2251.05 Aleliūnas Vilius (129) (rd=56.17, vol=0.06) - 0.00, 113.43,
    294 2258.72 Jokubauskis Tijus (165) (rd=37.39, vol=0.06) - 0.00, -7.84, 37.51, -1.94, 21.79, -3.08, -8.73, 7.34, -4.64, 9.16, 24.74, 2.18, -60.28, 17.68,
    295 2262.69 Vaitkus Lukas (199) (rd=73.11, vol=0.06) - 0.00,
    296 2267.97 Časas Adomas (31) (rd=36.80, vol=0.06) - 0.00, 101.59, 39.57, -2.32, 45.44, 28.51, 26.53, 16.48, 7.88,
    297 2268.98 Cibas Domantas (951) (rd=80.81, vol=0.06) - 0.00,
    298 2295.58 Rimkus Tautvydas (101) (rd=47.39, vol=0.06) - 0.00, 114.35, 63.25,
    299 2358.82 Lapinskas Andojas (269) (rd=72.66, vol=0.06) - 0.00,
    300 2379.66 Mickus Donatas (180) (rd=75.29, vol=0.06) - 0.00, |}]

(* TODO:Steps to do 
  1. Create Rating_store.t with Glicko2 settings 
  2. For every league event:
    2.1 For each course of the vent:
      2.1.1 Add (add_if_not_exist) each participant from the event course to the Rating_store.t (for each course)
      2.1.2 Create a new Glicko2.Ranking.t with the settings from Rating_store.t 
      2.1.3 Add each participant from the event course to the Glicko2.Ranking.t
      2.1.4 Add event course results to the Glicko2.Ranking.t 
      2.1.5 Calculate new ratings for results 
        2.1.5.1 Limit how much rating a DSQ looses
      2.1.6 Get all players from the Glicko2.Ranking.t and call Rating_store.update_info for each player 
*)
