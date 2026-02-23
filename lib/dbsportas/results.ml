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
  [@@deriving fields, yojson, show]

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
      |> List.mapi ~f:(fun i r ->
             Runner_result.update_overall_position r
               ~field:Runner_stats.Fields.overall_position (i + 1)
             |> Runner_result.update_race_execution
             |> Runner_result.update_mistake_cluster)
    in

    (* calculate potential position by taking your potential time and finding
       your potential position by looping thourgh the results from top to bottom
       and finding the position at which a runners time is bigger than yours, that is
       your potential position *)
    let finished =
      List.map finished ~f:(fun r ->
          let potential_position =
            if Option.value_exn r.stats.overall_position = 1 then 1
            else
              let potential_time = Option.value_exn r.stats.potential_time in
              let position =
                List.fold finished ~init:None ~f:(fun pos r ->
                    match pos with
                    | Some _ -> pos
                    | None ->
                        if r.time > potential_time then r.stats.overall_position
                        else None)
                |> Option.value_exn
              in
              position
          in
          Runner_result.update_potential_position r potential_position)
    in

    let dsq =
      List.filter runners ~f:(fun r ->
          Runner_result.equal_resultStatus r.status Runner_result.Dsq)
    in

    Fields.create ~course_name ~course_id ~controls ~finished ~dsq

  let update_gender (t : t) (runner_map : String.t Int.Map.t)
      ~(gender_prefix : string) =
    let _, finished =
      List.fold t.finished ~init:(1, []) ~f:(fun (position, finished) r ->
          let r_group = Map.find_exn runner_map r.number in

          if String.is_prefix ~prefix:gender_prefix r_group then
            let r =
              Runner_result.update_gender_or_group_position r
                ~field:Runner_stats.Fields.position_gender position
            in
            (position + 1, r :: finished)
          else (position, r :: finished))
    in
    (* here we reverse the finished because while `folding` we created the new
       list in reverse order *)
    { t with finished = List.rev finished }

  let update_group (t : t) (runner_map : String.t Int.Map.t) =
    let groups =
      List.sort_and_group t.finished ~compare:(fun r1 r2 ->
          let group1 = Map.find_exn runner_map r1.number in
          let group2 = Map.find_exn runner_map r2.number in
          String.compare group1 group2)
    in
    let group_pos_map : Int.t Int.Map.t = Int.Map.empty in
    let group_pos_map =
      List.fold groups ~init:group_pos_map ~f:(fun group_pos_map group ->
          let map =
            List.foldi group ~init:group_pos_map ~f:(fun i map r ->
                Map.set map ~key:r.number ~data:(i + 1))
          in
          map)
    in
    let finished =
      List.map t.finished ~f:(fun r ->
          let position = Map.find_exn group_pos_map r.number in
          Runner_result.update_gender_or_group_position r
            ~field:Runner_stats.Fields.position_group position)
    in

    { t with finished }

  let update_gender_and_group_positions t
      (simple_results : Simple_result.CourseResult.t list) =
    let runner_map : String.t Int.Map.t = Int.Map.empty in
    let runner_map =
      List.fold simple_results ~init:runner_map ~f:(fun map runner ->
          Map.set map ~key:runner.number ~data:runner.group.group)
    in

    (* update gender position for men *)
    let t = update_gender t runner_map ~gender_prefix:"V-" in
    (* update gender position for women *)
    let t = update_gender t runner_map ~gender_prefix:"M-" in

    (* update group position for each athlete. Groups are V-21A, M-40 etc. *)
    let t = update_group t runner_map in
    t
end

module ResultsTable = struct
  type t = { course_results : CourseResult.t list } [@@deriving yojson]

  let of_resp (resp : Response.ResultsTableResp.t) =
    let course_results =
      List.map resp.courses ~f:(CourseResult.of_resp resp.runners)
    in
    { course_results }
end

let parse_course_results_table data =
  let json = Yojson.Safe.from_string data in
  Response.ResultsTableResp.t_of_yojson json

let%expect_test "parse_course_results_table" =
  let filename = "/home/angel/Documents/ocaml/vkd/splits_resp.json" in
  let splits_resp = In_channel.read_all filename in
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
  (* printf "%s" out; *)
  [%expect
    {|
      Barkauskas Aidas 1: 1.157742 []
      Dienytė Margarita 5: 1.396501 []
      Petrevičius Aras 16: 1.427988 []
      Mejeras Gintaras 28: 1.671137 []
      Stančikas Virginijus 29: 1.352139 []
      Časas Adomas 31: 1.036786 []
      Balčiūnaitė Barbora 34: 1.473908 []
      Balčiūnas Vincas 35: 1.546939 []
      Časas Vincentas Petras 37: 1.504373 []
      Lelkaitis Valdas 39: 1.457726 []
      Rimydytė Ona 41: 1.998251 []
      Narvydas Simonas 45: 1.313737 []
      Sveikauskas Julius 49: 1.626239 []
      Sabataitis Kristijonas 50: 1.725121 []
      Kušeliauskas Kęstutis 54: 1.765101 []
      Petrilionis Marius 56: 1.483382 []
      Auštrienė Giedrė 63: 1.357709 []
      Ričkus Arnoldas 64: 1.521577 []
      Jatkauskas Jonas 65: 1.595179 []
      Saldžiūnas Viktoras 68: 1.667638 []
      Lukoševičius Mantas 82: 1.744745 []
      Gavėnas Gintaras 88: 1.595335 []
      Ivanovas Edgaras 90: 1.727697 []
      Aleksandraitytė Džiuginta 93: 1.422157 []
      Brazauskaitė Leokadija 94: 1.411079 []
      Montvila Mykolas 95: 1.234919 []
      Ragauskas Audrius 99: 1.506706 []
      Tarozaitė Birutė 102: 2.201749 []
      Jauniškis Robertas 104: 1.204665 []
      Rinkevičius Darius 112: 1.412828 []
      Cicėnas Audrius 114: 1.433819 []
      Žukauskas Artūras 136: 2.615743 []
      Pašuk Sergeij 151: 1.311712 []
      Rusakevičius Dainius 156: 1.348105 []
      Atgalainė Adrija 168: 1.375510 []
      Bertašius Rimvydas 175: 1.251964 []
      Užkuraitis Simanas 179: 1.229604 []
      Abramenkov Sergeij 183: 2.027405 []
      Kanapinskaitė Viltė 184: 1.810496 []
      Sriubas Egidijus 191: 1.476385 []
      Navickas Darius 208: 1.560933 []
      Trečiokaitė Vilija 210: 2.205248 []
      Šapranauskas Jonas 219: 1.813411 []
      Blaževičius Gediminas 225: 1.367347 []
      Volungevičienė Judita 231: 1.595918 []
      Šinkūnaitė Viltė 251: 2.004082 []
      Pigagaitė Aistė 261: 1.882216 []
      Budginas Vytas 295: 1.852905 []
      Dūda Kostas 316: 2.017493 []
      Kubaitis Arūnas 319: 1.703277 []
      Jokubauskis Kęstutis 321: 1.261808 []
      Kulevičius Donaldas 323: 1.780175 []
      Šinkūnas Rimvydas 328: 1.219441 []
      Radžius Antanas 335: 1.487464 []
      Staišiūnas Viktoras 343: 1.128621 []
      Jasinevičius Tomas 349: 1.237237 []
      Mačanaitė Kristina 364: 2.068633 []
      Kananavičius Robertas 368: 2.145985 []
      Ašmonas Nojus 516: 1.954831 []
      Staškevičiūtė Raminta 518: 1.844315 []
      Gembutaitė Sandra 530: 1.675412 []
      Stupelis Rimvydas 536: 1.148527 []
      Markovas Paulius 542: 1.247813 []
      Iliev Angel 617: 1.240816 []
      Arlauskienė Edita 665: 1.974406 []
      Arlauskas Jonas 666: 1.883345 []
      Malcaitė Eglė 762: 1.921283 []
      Jadenkus Domantas 778: 1.847813 []
      Boženokas Michailas 816: 1.640816 []
      Bukauskas Virginijus 855: 1.873786 []
      Leipus Vytautas 864: 1.653644 []
      Pranaitis Tomas 899: 1.574789 []
      Garbaliauskaitė Elena 1561: 1.850146 []
      Švedarauskas Simonas 1689: 1.246512 []
      Neniškis Algirdas 1697: 2.003499 []
      Jurkevičius Antanas 1719: 1.425506 []
      Varonenka Juozas 1796: 1.678078 []
      Ožema Andrijis 18: 1.682255 []
      Dauderienė Živilė 38: 1.608200 []
      Laukaitis Andrius 53: 0.958112 []
      Švetkauskas Edgaras 55: 1.635535 []
      Dapkevičius Vytenis 66: 1.241956 []
      Macijauskė Daiva 73: 1.332692 []
      Stakišaitis Arūnas 77: 2.014806 []
      Balčiūnas Tomas 79: 1.463563 []
      Liogė Ugnius 83: 1.298077 []
      Žiliajevas Saulius Igoris 86: 1.342667 []
      Montvilienė Ieva 87: 1.533030 []
      Grigaliūnaitė Agnė 97: 1.312150 []
      Dilė Nida 141: 1.562642 []
      Laukaitienė Vaida 144: 1.300954 []
      Serapinas Valdas 166: 1.698178 []
      Juškevičius Valentinas 167: 1.375854 []
      Umbrasas Gediminas 170: 2.684510 []
      Montvila Antanas 200: 1.582812 []
      Davidenko Natalija 202: 1.476082 []
      Montvilaitė Ona 212: 2.131549 []
      Pigagienė Iveta 259: 1.609513 []
      Miltinis Marius 290: 1.678429 []
      Bajoriūnas Gediminas 299: 1.829727 []
      Boženokas Michailas 318: 2.018182 []
      Šimėnas Herkus 359: 1.104020 []
      Kryžanauskas Ridas 363: 1.600122 []
      Ambrasiūnas Mindaugas 365: 1.474765 []
      Minkevičius Vilius 367: 1.973819 []
      Kananavičienė Indrė 369: 1.813084 []
      Zlatkus Ričardas 517: 1.770501 []
      Rudinskis Povilas 519: 1.292308 []
      Statauskaitė Vilma 688: 1.289294 []
      Čepkauskas Osmundas 716: 1.724374 []
      Šimėnas Skalmantas 871: 1.656606 []
      Jačiauskas Tadas 885: 2.332770 []
      Romanovas Marijus 1668: 1.146272 []
      Vilčinskas Rimantas 1678: 1.458998 []
      Šimėnas Valdemaras 1685: 1.406480 []
      Kantakevičius Vaidas 1754: 1.784738 []
      Jovaišas Jonas 1814: 1.757098 []
      Skaisgirytė Ona 24: 1.713627 []
      Tuomenas Vytautas 48: 1.904791 []
      Nekrašas Vygantas 85: 1.627386 []
      Kirilauskas Daumantas 153: 1.518072 []
      Gylienė Teresė 268: 2.520315 []
      Navickas Morkus 286: 1.512665 []
      Kondratas Romualdas 302: 1.301147 []
      Šimėnas Albertas 308: 1.315663 []
      Mackevičius Vigirdas 317: 2.254700 []
      Gylys Edmundas 337: 2.522741 []
      Misevičiūtė Justina 357: 1.523432 []
      Vilimas Donatas 361: 1.594052 []
      Pranaitis Norbertas 362: 1.995196 []
      Berenis Jurgis 618: 1.672607 []
      Simonavičius Ąžuolas 682: 1.666887 []
      Mackevičienė Nijolė 791: 2.249848 []
      Slivinskas Giedrius 1574: 0.950893 []
      Puidokas Paulius 1635: 1.158915 []
      Rudėnas Darius 5013: 1.984814 []
      Kniukšta Romualdas 5030: 1.332837 []
      Petravičius Rokas 5123: 1.213251 []
      Kučejevaitė Goda 5127: 1.400276 []
      Siudikaitė Junda 5134: 1.519669 []
      Petkevičius Emilis 5139: 1.097907 []
      Mickuvienė Algirda 5140: 1.169082 []
      Siudikas Kęstutis 5155: 1.367840 []
      Siudikienė Dalia 5157: 1.811594 []
      Aleliūnaitė Irmantė 5212: 1.463727 []
      Platakis Audrius 5216: 1.206612 []
      Pranciulis Rimvydas 5217: 1.298827 []
      Zaliauskas Juras 5224: 1.336094 []
      Bacevičiūtė Medeina 5230: 1.863497 []
      Ambrazas Ignas 5235: 1.155280 []
      Žvirblis Arvydas 5242: 1.561767 []
      Tunkevičiūtė Gabrielė 5248: 1.362319 []
      Antanavičius Kęstutis 5252: 1.421670 []
      Pralgauskis Danielius 5254: 1.112169 []
      Mickevičius Rokas 5260: 1.121858 []
      Kiela Daumantas 5261: 1.115191 []
      Stanevičienė Rūta 5262: 2.201518 []
      Trakimas Vytautas 5264: 1.179876 []
      Dzidzevičius Linas 5269: 1.262250 []
      Mickevičiūtė Juodišienė Karolina 5273: 1.357488 []
      Sriubas Vainius 5276: 1.254658 []
      Musajevaitė Agnė 5277: 1.322853 []
      Malūkas Arūnas 5281: 1.398419 []
      Lapinskas Mindaugas 5283: 1.682540 []
      Liubartas Artūras 5291: 1.410628 []
      Lasmanis Martinš 5300: 1.163799 []
      Povilonytė Ieva 356: 0.952555 []
      Bliujūtė Rasa 360: 1.808271 []
      Sutkutė Laura 366: 1.211364 []
      Kareckytė Eila 896: 0.988342 []
      Archipovas Vincas 1637: 1.083548 []
      hello |}]
