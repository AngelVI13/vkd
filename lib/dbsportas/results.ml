open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module CourseResult = struct
  type t = {
    course_name : string;
    course_id : int;
    controls : string list;
    finished : Runner_result.t list;
    dsq : Runner_result.t list;
  }
  [@@deriving fields, yojson]

  let of_resp (runners : Response.RunnerResp.t list)
      (course : Response.CourseResp.t) =
    let course_id = Int.of_string course.id in
    let runners = List.filter runners ~f:(fun r -> r.course_id = course_id) in
    let course_name =
      match List.hd runners with Some r -> r.course_name | None -> ""
    in
    let controls = String.split ~on:'-' course.controls in

    let control_time_field = Split.Fields.time in
    let control_position_field = Split.Fields.position in
    let overall_time_field = Split.Fields.overall_time in
    let overall_position_field = Split.Fields.overall_position in

    let runners =
      (* create RunnerResult.t from json response *)
      List.map runners ~f:Runner_result.of_resp
      (* create RunnersMap.t and calculate positions (control & overall) for each runner *)
      |> Runners_map.of_runners
      |> Runners_map.update_runner_positions ~time_field:control_time_field
           ~position_field:control_position_field
      |> Runners_map.update_runner_positions ~time_field:overall_time_field
           ~position_field:overall_position_field
      |> Runners_map.update_runner_mistakes |> Runners_map.to_runners
      (* sort runners based on their overall time *)
      |> List.sort ~compare:(fun r1 r2 -> Int.compare r1.time r2.time)
    in

    let finished =
      List.filter runners ~f:(fun r ->
          Runner_result.equal_resultStatus r.status Runner_result.Finished)
    in
    let dsq =
      List.filter runners ~f:(fun r ->
          Runner_result.equal_resultStatus r.status Runner_result.Dsq)
    in

    (* TODO: calculate mistakes here and add them to the split record *)
    Fields.create ~course_name ~course_id ~controls ~finished ~dsq
end

module ResultsTable = struct
  type t = { course_results : CourseResult.t list } [@@deriving yojson]

  let of_resp (resp : Response.ResultsTableResp.t) =
    let course_results =
      List.map resp.courses ~f:(CourseResult.of_resp resp.runners)
    in
    { course_results }
end

let parse_course_results_table json_chan =
  let json = Yojson.Safe.from_channel json_chan in
  Response.ResultsTableResp.t_of_yojson json

let%expect_test "parse_course_results_table" =
  let filename = "/home/angel/Documents/ocaml/vkd/splits_resp.json" in
  let splits_resp = In_channel.create filename in
  let results_table_resp = parse_course_results_table splits_resp in
  let results_table = ResultsTable.of_resp results_table_resp in
  (* let out = Yojson.Safe.to_string (ResultsTable.yojson_of_t results_table) in *)
  (* let out = *)
  (*   List.nth_exn results_table.course_results 0 *)
  (*   |> CourseResult.yojson_of_t |> Yojson.Safe.to_string *)
  (* in *)
  let _ = results_table in
  (* Out_channel.write_all *)
  (*   "/home/angel/Documents/ocaml/vkd/course_results_test.json" ~data:out; *)
  (* Yojson.Safe.to_file *)
  (*   "/home/angel/Documents/ocaml/vkd/splits_resp_processed.json" *)
  (*   (ResultsTable.yojson_of_t results_table); *)
  printf "%s" "hello";
  [%expect
    {|
      Barkauskas Aidas 1: 1.157742 [-, -, 23, -, -, -, -, -, -, -, -, -, -, -, -, 11, -, -, -, -, -, -, ]
      Dienytė Margarita 5: 1.396501 [-, -, 13, -, -, 66, -, -, -, -, 20, 50, -, -, -, -, -, -, -, -, -, -, ]
      Petrevičius Aras 16: 1.427988 [-, -, -, -, -, -, -, 12, -, -, 37, 21, -, -, -, 25, -, -, -, -, -, -, ]
      Mejeras Gintaras 28: 1.671137 [-, -, 39, -, -, 17, 62, 45, -, -, -, 19, -, -, -, 37, -, -, -, -, -, -, ]
      Stančikas Virginijus 29: 1.352139 [11, -, 13, -, -, -, -, 93, 89, -, -, -, -, -, -, 21, 49, -, -, -, -, -, ]
      Časas Adomas 31: 1.036786 [11, -, -, -, -, -, -, 26, -, -, -, -, -, -, -, 24, -, -, -, -, -, -, ]
      Balčiūnaitė Barbora 34: 1.473908 [10, -, -, -, -, 11, 21, -, -, -, -, 21, -, -, 11, 160, 12, -, -, -, -, -, ]
      Balčiūnas Vincas 35: 1.546939 [-, -, -, -, 80, -, 85, -, -, -, -, 49, -, -, 14, 14, 84, -, -, -, -, -, ]
      Časas Vincentas Petras 37: 1.504373 [-, -, 10, -, 68, 75, -, 12, -, -, 14, 34, -, -, -, 46, -, -, -, -, -, -, ]
      Lelkaitis Valdas 39: 1.457726 [-, -, -, -, 49, -, 80, -, -, -, -, -, -, -, -, 23, -, -, -, -, -, -, ]
      Rimydytė Ona 41: 1.998251 [35, -, 12, -, 18, -, -, 30, -, -, 31, -, 27, -, 39, 25, -, -, -, -, -, -, ]
      Narvydas Simonas 45: 1.313737 [41, -, -, -, 16, -, -, 10, -, -, -, 52, -, -, 32, 49, -, -, -, -, -, -, ]
      Sveikauskas Julius 49: 1.626239 [-, -, -, 15, -, 36, 77, 35, -, -, -, -, -, -, -, -, 51, -, -, -, -, -, ]
      Sabataitis Kristijonas 50: 1.725121 [18, 12, -, -, -, -, -, -, -, -, -, -, 33, -, -, 202, 31, -, -, -, -, -, ]
      Kušeliauskas Kęstutis 54: 1.765101 [29, -, -, -, -, -, -, 16, -, -, 53, 116, -, -, 53, -, 83, -, -, -, -, -, ]
      Petrilionis Marius 56: 1.483382 [-, -, -, -, -, -, -, 80, 12, -, -, -, 13, -, 27, 27, 10, -, -, -, -, -, ]
      Auštrienė Giedrė 63: 1.357709 [33, 10, -, -, -, -, 56, -, -, -, -, -, 14, -, 10, 14, 166, -, -, -, -, -, ]
      Ričkus Arnoldas 64: 1.521577 [22, -, -, 12, 186, -, 761, 75, 30, -, -, 126, -, -, 28, -, -, -, -, -, -, -, ]
      Jatkauskas Jonas 65: 1.595179 [-, -, -, -, 127, -, 26, -, 11, -, 57, 67, -, -, -, 14, -, -, -, -, 14, -, ]
      Saldžiūnas Viktoras 68: 1.667638 [-, -, -, -, -, 20, -, -, 30, -, -, -, -, -, 16, 30, 19, -, -, -, -, -, ]
      Lukoševičius Mantas 82: 1.744745 [-, -, -, -, -, -, -, 197, 18, 32, -, 12, -, -, 11, -, 25, -, -, 13, -, -, ]
      Gavėnas Gintaras 88: 1.595335 [-, -, -, -, -, -, -, -, 15, -, -, 25, 10, -, 26, 18, -, -, -, -, -, -, ]
      Ivanovas Edgaras 90: 1.727697 [-, -, -, -, -, -, -, 41, 11, -, -, 30, -, -, 23, 21, -, -, -, -, 27, -, ]
      Aleksandraitytė Džiuginta 93: 1.422157 [-, -, -, -, 48, -, -, -, 14, -, -, -, -, -, -, 10, -, -, -, -, 19, -, ]
      Brazauskaitė Leokadija 94: 1.411079 [-, -, 10, -, -, 20, -, 12, 13, -, 22, 36, -, -, 12, -, -, -, -, -, -, -, ]
      Montvila Mykolas 95: 1.234919 [15, 16, -, -, -, -, -, -, -, -, 22, 27, -, -, 27, -, 21, -, -, -, -, -, ]
      Ragauskas Audrius 99: 1.506706 [12, -, 13, -, 15, -, -, 51, 12, 24, -, 25, -, -, -, -, -, -, -, -, -, -, ]
      Tarozaitė Birutė 102: 2.201749 [25, -, -, -, -, 87, -, -, 59, -, -, -, 62, -, 53, 57, 29, -, -, -, -, -, ]
      Jauniškis Robertas 104: 1.204665 [-, 17, -, -, -, -, -, -, -, -, 31, -, -, -, -, -, -, -, -, -, -, -, ]
      Rinkevičius Darius 112: 1.412828 [-, -, 12, -, -, 17, -, 14, -, -, 24, 34, -, -, 18, 18, -, -, -, -, -, 10, ]
      Cicėnas Audrius 114: 1.433819 [-, -, -, -, -, 15, -, 26, -, -, -, -, -, -, 24, 41, 47, -, -, -, -, -, ]
      Žukauskas Artūras 136: 2.615743 [-, -, -, -, -, 111, 40, 132, 12, -, -, 44, 18, -, -, 37, 22, -, -, -, -, -, ]
      Pašuk Sergeij 151: 1.311712 [-, -, 23, -, -, -, -, 187, -, -, 62, 35, -, -, -, 11, -, -, -, -, -, -, ]
      Rusakevičius Dainius 156: 1.348105 [15, -, -, -, -, -, -, 23, 23, -, -, 50, -, -, -, 23, -, -, 10, -, 20, -, ]
      Atgalainė Adrija 168: 1.375510 [-, -, -, -, 31, -, -, -, 14, -, -, 28, 10, -, 10, 14, -, -, -, -, -, -, ]
      Bertašius Rimvydas 175: 1.251964 [-, -, -, -, -, 29, 40, 68, -, -, 12, -, -, -, -, -, 82, -, -, -, -, -, ]
      Užkuraitis Simanas 179: 1.229604 [-, -, -, -, -, 18, -, -, -, -, 15, -, -, -, 31, 31, -, -, -, -, -, -, ]
      Abramenkov Sergeij 183: 2.027405 [11, 65, -, 40, -, -, 29, -, -, -, 94, -, -, -, 18, 105, 51, -, -, -, -, -, ]
      Kanapinskaitė Viltė 184: 1.810496 [22, -, -, -, -, -, -, 10, 12, -, 12, 16, -, -, 51, 23, 32, -, -, -, -, -, ]
      Sriubas Egidijus 191: 1.476385 [-, -, -, -, -, -, -, 36, 22, -, 41, -, -, -, 23, 11, -, -, -, -, -, -, ]
      Navickas Darius 208: 1.560933 [15, -, 10, -, -, -, 18, -, 12, -, 34, 36, -, -, -, -, -, -, -, -, -, -, ]
      Trečiokaitė Vilija 210: 2.205248 [34, -, -, -, -, 79, -, 11, 55, -, -, -, 74, -, 49, 21, 48, -, -, -, -, -, ]
      Šapranauskas Jonas 219: 1.813411 [15, -, -, -, -, 27, -, 13, 29, -, -, 34, 52, -, 24, 55, -, -, -, -, -, -, ]
      Blaževičius Gediminas 225: 1.367347 [-, -, 11, -, -, -, -, -, -, -, -, -, -, -, 41, 49, 43, 10, 28, -, 12, -, ]
      Volungevičienė Judita 231: 1.595918 [14, -, 14, -, -, -, -, -, 20, -, 30, -, -, -, 13, -, 15, -, -, -, -, -, ]
      Šinkūnaitė Viltė 251: 2.004082 [-, -, -, -, -, -, -, 11, -, -, 70, -, 65, -, 33, 55, 32, -, -, -, -, -, ]
      Pigagaitė Aistė 261: 1.882216 [-, -, -, -, -, -, 35, -, 53, -, 22, -, -, -, 31, 70, 21, -, -, -, -, -, ]
      Budginas Vytas 295: 1.852905 [-, -, 15, -, 189, -, 35, -, 15, -, -, -, 16, -, 40, 35, -, -, -, -, -, -, ]
      Dūda Kostas 316: 2.017493 [19, -, 26, -, -, -, -, 11, 18, -, -, -, -, -, -, -, -, -, 33, -, -, -, ]
      Kubaitis Arūnas 319: 1.703277 [18, -, -, -, -, -, -, 21, 34, -, 36, -, -, -, 36, 145, 23, -, 10, -, -, -, ]
      Jokubauskis Kęstutis 321: 1.261808 [-, -, 10, -, -, -, -, 10, 31, -, 10, -, -, 10, 11, -, -, -, -, -, -, -, ]
      Kulevičius Donaldas 323: 1.780175 [-, -, -, 65, 25, -, -, 32, 14, -, -, 60, -, -, 29, 13, -, -, -, -, -, -, ]
      Šinkūnas Rimvydas 328: 1.219441 [-, 31, 10, 29, 12, 17, -, -, -, -, -, -, -, -, -, 11, -, -, -, -, -, -, ]
      Radžius Antanas 335: 1.487464 [-, -, 11, -, -, -, -, -, -, -, 30, 23, -, -, 13, 29, 31, -, -, -, 13, -, ]
      Staišiūnas Viktoras 343: 1.128621 [-, -, 22, -, -, -, -, -, 12, -, -, 20, -, -, -, -, -, -, -, -, -, -, ]
      Jasinevičius Tomas 349: 1.237237 [57, -, 23, 28, -, -, -, 171, -, -, -, 12, -, -, 10, -, -, -, -, -, -, -, ]
      Mačanaitė Kristina 364: 2.068633 [13, 62, -, -, -, 179, -, -, -, -, 74, 98, -, -, 35, 13, 53, 13, -, -, -, -, ]
      Kananavičius Robertas 368: 2.145985 [42, -, -, 37, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, ]
      Ašmonas Nojus 516: 1.954831 [-, 68, -, -, -, 191, -, -, 10, -, 83, 112, -, -, 43, 168, 42, -, -, -, -, -, ]
      Staškevičiūtė Raminta 518: 1.844315 [-, -, 11, -, -, -, 21, -, -, -, -, 35, 12, -, 41, 16, 34, -, -, -, -, -, ]
      Gembutaitė Sandra 530: 1.675412 [27, 57, 12, -, -, -, -, 17, 54, -, -, 118, -, -, 34, 54, 30, -, -, -, -, -, ]
      Stupelis Rimvydas 536: 1.148527 [-, -, -, -, -, 124, -, -, -, -, -, -, -, -, 29, 15, -, -, -, -, -, -, ]
      Markovas Paulius 542: 1.247813 [13, -, 15, -, -, -, -, -, 10, -, -, -, -, -, -, 10, 16, -, -, -, -, -, ]
      Iliev Angel 617: 1.240816 [-, -, 14, -, -, -, -, -, -, -, 13, 44, -, -, -, 25, 11, -, -, -, -, -, ]
      Arlauskienė Edita 665: 1.974406 [48, -, 25, -, 66, -, -, 13, -, -, 21, 54, -, -, 27, 45, 169, -, -, -, -, -, ]
      Arlauskas Jonas 666: 1.883345 [35, -, 27, -, 70, -, -, -, -, -, 27, -, -, -, 21, 32, 247, -, -, -, -, -, ]
      Malcaitė Eglė 762: 1.921283 [43, -, 95, -, 23, -, 23, 50, -, -, -, -, 16, -, 52, 27, 30, -, -, -, 15, -, ]
      Jadenkus Domantas 778: 1.847813 [-, -, -, -, -, 72, -, -, -, -, 15, 19, 99, -, 38, 26, 39, -, -, -, -, -, ]
      Boženokas Michailas 816: 1.640816 [24, -, 10, -, -, -, 41, 13, -, -, -, 32, 34, -, 10, 27, -, -, -, -, -, -, ]
      Bukauskas Virginijus 855: 1.873786 [73, -, 14, -, -, -, -, 22, 23, 36, -, 17, 12, -, 53, 230, 16, -, 16, -, 10, 10, ]
      Leipus Vytautas 864: 1.653644 [51, 43, -, -, -, 11, -, 55, 41, -, -, -, -, -, 15, 27, -, -, -, -, -, -, ]
      Pranaitis Tomas 899: 1.574789 [-, 15, -, -, -, -, 461, 10, 69, 18, 17, 62, -, -, -, 47, -, -, -, 21, -, -, ]
      Garbaliauskaitė Elena 1561: 1.850146 [-, -, 20, -, -, -, -, -, -, -, -, 13, 33, -, 35, 17, 14, 31, -, -, -, -, ]
      Švedarauskas Simonas 1689: 1.246512 [-, 12, -, -, -, -, 15, -, 15, -, 16, -, -, -, 36, 21, -, -, 17, -, -, -, ]
      Neniškis Algirdas 1697: 2.003499 [15, -, -, -, -, 53, -, 40, -, -, -, 81, -, -, 20, 18, 65, -, -, -, -, -, ]
      Jurkevičius Antanas 1719: 1.425506 [36, 99, 12, -, -, 19, -, 132, 14, -, 30, -, -, -, 36, 20, -, -, -, -, -, -, ]
      Varonenka Juozas 1796: 1.678078 [21, -, -, -, -, -, -, 362, 11, -, 15, -, -, -, 58, 51, 43, -, -, -, -, -, ]
      Ožema Andrijis 18: 1.682255 [63, 15, 80, -, 36, -, 88, -, -, -, 224, -, -, 15, -, -, 10, ]
      Dauderienė Živilė 38: 1.608200 [-, -, 84, 37, -, -, -, 24, -, -, -, -, -, -, -, -, -, ]
      Laukaitis Andrius 53: 0.958112 [-, 11, -, -, -, 15, -, -, -, -, 30, -, -, -, -, -, -, ]
      Švetkauskas Edgaras 55: 1.635535 [-, -, 22, -, 37, 31, 14, 95, 24, -, -, -, -, -, -, -, -, ]
      Dapkevičius Vytenis 66: 1.241956 [13, -, 33, 13, -, -, -, 112, -, -, 17, -, 14, -, -, 19, -, ]
      Macijauskė Daiva 73: 1.332692 [-, -, 57, 19, -, -, -, -, -, -, 112, -, 36, -, -, -, -, ]
      Stakišaitis Arūnas 77: 2.014806 [-, 104, 39, 57, -, 28, 83, -, 62, 12, -, -, 105, -, -, -, -, ]
      Balčiūnas Tomas 79: 1.463563 [-, 74, 12, -, -, -, -, 50, -, -, -, -, -, -, -, -, -, ]
      Liogė Ugnius 83: 1.298077 [-, 28, -, 15, 10, -, -, -, -, -, 125, -, 10, -, -, -, -, ]
      Žiliajevas Saulius Igoris 86: 1.342667 [60, 131, 22, -, 383, -, -, -, -, -, -, -, 13, -, -, -, -, ]
      Montvilienė Ieva 87: 1.533030 [-, 49, 13, 14, -, -, 18, 80, -, -, -, -, -, -, -, -, -, ]
      Grigaliūnaitė Agnė 97: 1.312150 [-, 139, 17, 68, 35, -, 10, -, -, -, -, -, -, -, -, 13, -, ]
      Dilė Nida 141: 1.562642 [-, 57, 55, -, -, -, -, 49, -, -, -, -, -, -, -, -, -, ]
      Laukaitienė Vaida 144: 1.300954 [-, -, 22, -, -, 37, 10, 14, -, 89, -, -, -, -, -, -, -, ]
      Serapinas Valdas 166: 1.698178 [-, -, 55, 27, -, -, -, 21, 29, -, -, -, -, -, -, 90, -, ]
      Juškevičius Valentinas 167: 1.375854 [-, -, 31, 14, -, 66, -, -, -, 34, -, -, -, -, -, -, -, ]
      Umbrasas Gediminas 170: 2.684510 [-, 32, 39, 11, 60, -, 30, 71, -, 100, 85, -, -, 26, -, -, -, ]
      Montvila Antanas 200: 1.582812 [-, 32, 15, 79, 83, -, 35, -, -, -, -, -, -, -, -, -, -, ]
      Davidenko Natalija 202: 1.476082 [-, -, 21, -, -, 26, -, 69, 20, -, -, -, -, -, -, -, -, ]
      Montvilaitė Ona 212: 2.131549 [-, -, -, -, 27, 12, 13, 68, 43, 22, -, 10, -, 62, -, -, -, ]
      Pigagienė Iveta 259: 1.609513 [27, -, -, -, 42, -, 132, 13, -, -, -, -, -, -, -, 12, -, ]
      Miltinis Marius 290: 1.678429 [-, -, -, 14, -, 30, 59, 85, 69, -, 29, -, -, 18, -, -, -, ]
      Bajoriūnas Gediminas 299: 1.829727 [11, -, 103, 24, -, 13, 15, -, -, 94, -, -, -, -, -, -, -, ]
      Boženokas Michailas 318: 2.018182 [-, -, 15, 109, 116, 20, -, -, -, 24, 44, -, 18, -, -, 277, -, ]
      Šimėnas Herkus 359: 1.104020 [-, -, 11, -, -, 11, -, 87, 14, -, -, 15, -, -, -, -, -, ]
      Kryžanauskas Ridas 363: 1.600122 [-, -, 75, 19, -, -, -, 33, 140, 29, 19, -, -, -, -, 43, -, ]
      Ambrasiūnas Mindaugas 365: 1.474765 [72, 11, 11, 15, -, -, -, -, -, 22, -, -, -, 106, -, -, 11, ]
      Minkevičius Vilius 367: 1.973819 [357, 97, 79, 199, 10, 33, 61, -, -, 37, -, -, 26, -, -, -, -, ]
      Kananavičienė Indrė 369: 1.813084 [1387, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, ]
      Zlatkus Ričardas 517: 1.770501 [-, 93, -, -, 82, -, 11, -, -, -, -, -, -, -, -, -, -, ]
      Rudinskis Povilas 519: 1.292308 [-, -, -, -, 19, -, 34, -, -, -, -, -, -, -, -, -, -, ]
      Statauskaitė Vilma 688: 1.289294 [-, -, 13, 21, -, -, -, 55, -, -, -, -, -, -, -, -, -, ]
      Čepkauskas Osmundas 716: 1.724374 [47, -, 36, 20, 37, 41, -, -, -, -, -, -, 21, 71, -, 19, -, ]
      Šimėnas Skalmantas 871: 1.656606 [-, 66, -, 29, 21, 72, -, -, -, -, -, -, 32, -, -, -, -, ]
      Jačiauskas Tadas 885: 2.332770 [-, -, 81, -, 81, 1307, -, -, -, -, -, -, -, -, -, -, -, ]
      Romanovas Marijus 1668: 1.146272 [-, -, 12, 45, -, 10, -, -, -, -, -, -, -, -, -, -, -, ]
      Vilčinskas Rimantas 1678: 1.458998 [-, -, 39, 11, -, 12, 10, -, 21, -, -, -, 33, -, -, -, -, ]
      Šimėnas Valdemaras 1685: 1.406480 [-, -, 31, 28, 21, -, -, 203, -, -, 425, -, -, 18, -, -, -, ]
      Kantakevičius Vaidas 1754: 1.784738 [-, -, -, 11, -, 14, 22, -, 66, -, 28, 20, -, -, -, -, -, ]
      Jovaišas Jonas 1814: 1.757098 [21, -, -, 30, 51, -, 17, 261, -, 99, 11, -, -, -, -, -, -, ]
      Skaisgirytė Ona 24: 1.713627 [-, 20, 49, 21, -, 28, -, 23, -, 268, 17, -, -, ]
      Tuomenas Vytautas 48: 1.904791 [79, -, 78, -, -, -, 86, 15, -, 13, 69, -, -, ]
      Nekrašas Vygantas 85: 1.627386 [25, 22, -, -, -, 29, -, -, -, 184, 29, -, -, ]
      Kirilauskas Daumantas 153: 1.518072 [18, -, 244, 59, -, -, -, 12, 127, 72, 136, -, -, ]
      Gylienė Teresė 268: 2.520315 [-, -, 97, 32, -, -, -, 17, -, 43, -, -, 10, ]
      Navickas Morkus 286: 1.512665 [-, 31, -, 180, 234, -, 82, 172, 21, 115, 12, -, -, ]
      Kondratas Romualdas 302: 1.301147 [-, 17, 15, -, -, -, -, 22, -, 46, 14, -, -, ]
      Šimėnas Albertas 308: 1.315663 [312, 14, -, -, -, 212, -, -, 16, 54, 27, -, -, ]
      Mackevičius Vigirdas 317: 2.254700 [-, -, -, 55, -, -, -, 33, 108, -, -, -, -, ]
      Gylys Edmundas 337: 2.522741 [17, -, 101, 26, 20, -, -, 16, -, 32, -, -, 11, ]
      Misevičiūtė Justina 357: 1.523432 [-, -, 48, 189, -, -, -, -, 86, 32, -, 30, -, ]
      Vilimas Donatas 361: 1.594052 [65, 11, -, -, -, 168, -, 68, -, 33, -, -, -, ]
      Pranaitis Norbertas 362: 1.995196 [-, -, 102, 160, -, -, -, 178, -, 208, 40, 31, -, ]
      Berenis Jurgis 618: 1.672607 [-, 25, -, 125, 41, -, -, -, -, 74, -, -, -, ]
      Simonavičius Ąžuolas 682: 1.666887 [-, 23, -, 133, 19, 12, -, -, -, 78, -, -, -, ]
      Mackevičienė Nijolė 791: 2.249848 [-, -, -, 62, -, -, -, 44, 100, 16, -, -, -, ]
      Slivinskas Giedrius 1574: 0.950893 [55, -, -, 31, 27, -, 112, -, 15, -, -, 37, -, ]
      Puidokas Paulius 1635: 1.158915 [-, 14, 14, -, -, 16, 11, 34, 27, 101, -, -, -, ]
      Rudėnas Darius 5013: 1.984814 [-, -, 50, 112, -, -, -, -, 184, -, -, 39, -, -, -, ]
      Kniukšta Romualdas 5030: 1.332837 [18, -, -, -, -, 23, -, -, -, -, -, -, -, 22, -, ]
      Petravičius Rokas 5123: 1.213251 [28, -, -, -, -, -, -, -, -, -, -, 12, -, 16, -, ]
      Kučejevaitė Goda 5127: 1.400276 [-, -, 34, -, -, 14, 17, -, -, 48, -, -, -, -, -, ]
      Siudikaitė Junda 5134: 1.519669 [-, -, 50, 32, -, -, 47, -, -, -, -, -, -, -, -, ]
      Petkevičius Emilis 5139: 1.097907 [-, -, 24, 23, -, -, -, 28, -, -, -, -, -, -, -, ]
      Mickuvienė Algirda 5140: 1.169082 [22, -, -, -, -, 23, -, -, -, -, -, -, -, -, -, ]
      Siudikas Kęstutis 5155: 1.367840 [79, -, -, 19, -, 31, -, -, -, -, -, -, -, -, -, ]
      Siudikienė Dalia 5157: 1.811594 [14, 14, -, 28, -, 25, -, -, -, -, -, -, 21, -, -, ]
      Aleliūnaitė Irmantė 5212: 1.463727 [-, -, 49, -, -, 11, -, -, -, -, 12, -, 108, -, -, ]
      Platakis Audrius 5216: 1.206612 [19, -, -, 49, -, 20, 13, -, -, -, -, -, -, -, -, ]
      Pranciulis Rimvydas 5217: 1.298827 [-, 10, -, -, -, -, 35, -, -, 54, -, -, -, 23, -, ]
      Zaliauskas Juras 5224: 1.336094 [-, -, 10, 16, -, -, 53, -, -, -, -, -, 14, -, -, ]
      Bacevičiūtė Medeina 5230: 1.863497 [-, 14, -, -, -, 217, -, 28, -, -, -, -, -, 112, -, ]
      Ambrazas Ignas 5235: 1.155280 [20, 10, -, 17, -, -, -, -, -, -, -, -, -, -, -, ]
      Žvirblis Arvydas 5242: 1.561767 [19, -, -, 75, -, 34, 13, -, -, -, -, 47, -, -, -, ]
      Tunkevičiūtė Gabrielė 5248: 1.362319 [-, -, -, 23, -, 55, -, -, 35, -, -, -, -, 12, -, ]
      Antanavičius Kęstutis 5252: 1.421670 [-, -, -, 29, -, 15, 74, -, -, -, -, -, 13, -, -, ]
      Pralgauskis Danielius 5254: 1.112169 [12, 17, -, -, -, 20, -, -, -, -, 12, -, -, 23, -, ]
      Mickevičius Rokas 5260: 1.121858 [-, -, 164, 62, -, 18, -, -, -, -, -, 10, -, -, -, ]
      Kiela Daumantas 5261: 1.115191 [-, -, 21, 18, -, 13, -, -, -, 91, -, -, -, -, -, ]
      Stanevičienė Rūta 5262: 2.201518 [-, -, -, -, -, -, 17, 87, 36, -, -, -, -, 37, -, ]
      Trakimas Vytautas 5264: 1.179876 [-, -, -, -, -, 29, -, -, -, -, -, -, -, -, -, ]
      Dzidzevičius Linas 5269: 1.262250 [-, -, -, 20, -, 60, 25, -, -, -, -, -, -, -, -, ]
      Mickevičiūtė Juodišienė Karolina 5273: 1.357488 [-, 10, -, -, -, 42, 24, -, -, -, -, -, -, -, -, ]
      Sriubas Vainius 5276: 1.254658 [13, -, -, -, -, 10, -, 21, 41, -, -, 12, -, -, -, ]
      Musajevaitė Agnė 5277: 1.322853 [13, -, -, 21, -, 98, 40, -, -, -, -, 11, -, -, -, ]
      Malūkas Arūnas 5281: 1.398419 [-, -, -, -, -, -, 236, -, -, -, -, -, -, 31, 18, ]
      Lapinskas Mindaugas 5283: 1.682540 [97, -, -, 21, -, -, -, -, -, 45, 93, 44, -, -, -, ]
      Liubartas Artūras 5291: 1.410628 [-, -, -, 17, -, -, 53, -, -, -, -, -, -, -, -, ]
      Lasmanis Martinš 5300: 1.163799 [-, 16, -, -, -, 13, -, -, -, -, -, -, -, 23, -, ]
      Povilonytė Ieva 356: 0.952555 [325, -, 14, 35, 103, -, -, -, -, ]
      Bliujūtė Rasa 360: 1.808271 [-, 20, 32, 28, -, 26, -, -, -, ]
      Sutkutė Laura 366: 1.211364 [275, -, -, -, 70, -, 29, -, -, ]
      Kareckytė Eila 896: 0.988342 [-, 68, 106, 179, 30, 27, 52, 15, -, ]
      Archipovas Vincas 1637: 1.083548 [-, -, -, 21, -, -, 41, 91, -, ]
      hello |}]
